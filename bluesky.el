;;; bluesky.el, a Bluesky client for Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2024, 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/ekg
;; Package-Requires: ((plz "0.9.0") (futur "1.7") (vui "20260130.2113"))
;; Keywords: outlines, hypermedia
;; Version: 0.0.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; bluesky is a client for the Bluesky social network, following the atproto
;; spec.  It should be compatible with any other server following that spec.
;; Emacs is not always so capable in the UI department, but this tries to render
;; everything important as clearly as possible.

(require 'bluesky-conn)
(require 'bluesky-ui)
(require 'auth-source)
(require 'browse-url)
(require 'cl-lib)
(require 'futur)
(require 'seq)
(require 'subr-x)
(require 'vui)

(defgroup bluesky nil
  "Bluesky client for Emacs."
  :group 'applications)

(defcustom bluesky-default-host "bsky.social"
  "The default host of the Bluesky server."
  :type 'string
  :group 'bluesky)

(defconst bluesky-timeline-buffer-name "*Bluesky Timeline*"
  "The name of the Bluesky timeline buffer.")

(defconst bluesky-thread-buffer-name "*Bluesky Thread*"
  "The name of the Bluesky thread buffer.")

(defface bluesky-heading
  '((t :inherit bold :height 1.2))
  "Face for Bluesky buffer headings.")

(defface bluesky-error
  '((t :inherit error))
  "Face for Bluesky errors.")

(defface bluesky-muted
  '((t :inherit shadow))
  "Face for subdued Bluesky UI text.")

(defface bluesky-current-post
  '((t :inherit highlight :extend t))
  "Face for the currently selected Bluesky post.")

(defvar-local bluesky--navigation-override-mode nil
  "Non-nil when Bluesky navigation keys should override minor modes.")

(defvar bluesky--navigation-override-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'bluesky-feed-next-post)
    (define-key map (kbd "k") #'bluesky-feed-previous-post)
    (define-key map (kbd "o") #'bluesky-open-current)
    (define-key map (kbd "L") #'bluesky-toggle-like)
    (define-key map (kbd "R") #'bluesky-toggle-repost)
    (define-key map (kbd "b") #'bluesky-toggle-bookmark)
    (define-key map (kbd "r") #'bluesky-reply)
    map)
  "High-precedence keymap for Bluesky navigation.")

(add-to-list 'emulation-mode-map-alists
             `((bluesky--navigation-override-mode
                . ,bluesky--navigation-override-map)))

(defvar bluesky-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'bluesky-feed-refresh)
    (define-key map "j" #'bluesky-feed-next-post)
    (define-key map "k" #'bluesky-feed-previous-post)
    (define-key map "l" #'bluesky-feed-extend)
    (define-key map "o" #'bluesky-open-current)
    (define-key map "L" #'bluesky-toggle-like)
    (define-key map "R" #'bluesky-toggle-repost)
    (define-key map "b" #'bluesky-toggle-bookmark)
    (define-key map "r" #'bluesky-reply)
    (define-key map (kbd "RET") #'bluesky-open-thread)
    map)
  "Keymap for Bluesky feed mode.")

(define-derived-mode bluesky-mode vui-mode "Bluesky"
  "Major mode for Bluesky buffers consisting of lists of posts."
  (setq truncate-lines t)
  (buffer-disable-undo)
  (setq-local bluesky--navigation-override-mode t)
  (visual-line-mode 1))

(defvar-local bluesky-host bluesky-default-host
  "Host used in a particular feed.")

(defvar-local bluesky-feed-session nil
  "The Bluesky feed session associated with the buffer's feed.")

(defvar-local bluesky-feed-root nil
  "The VUI root instance for the Bluesky feed.")

(defvar-local bluesky-current-post-overlay nil
  "Overlay highlighting the current Bluesky post.")

(defun bluesky--future-set-state (future state-key value-fn error-key)
  "Set VUI STATE-KEY from FUTURE using VALUE-FN, or ERROR-KEY on failure."
  (futur-bind
   future
   (vui-async-callback (value)
     (vui-batch
      (vui-set-state state-key (funcall value-fn value))
      (vui-set-state :loading nil)
      (vui-set-state :error nil)))
   (vui-async-callback (err)
     (vui-batch
      (vui-set-state :loading nil)
      (vui-set-state error-key err)))))

(defun bluesky--error-message (err)
  "Return a readable error string for ERR."
  (if-let* ((payload (and (consp err) (eq (car err) 'bluesky-api-error)
                          (cadr err)))
            (message (or (plist-get payload :message)
                         (plist-get payload :error))))
      message
    (error-message-string err)))

(defun bluesky--post-id (post)
  "Return a stable id for POST."
  (or (plist-get post :uri)
      (plist-get post :cid)
      (secure-hash 'sha1 (prin1-to-string post))))

(defun bluesky--quoted-post (post)
  "Return POST's quoted post view, if present."
  (let* ((embed (or (plist-get post :embed)
                    (plist-get (plist-get post :record) :embed)))
         (quoted (plist-get embed :record))
         (value (plist-get quoted :value)))
    (when (and quoted value (plist-get quoted :author))
      (let ((quoted-post (copy-sequence quoted)))
        (setq quoted-post (plist-put quoted-post :record value))
        quoted-post))))

(defun bluesky--flatten-posts (posts &optional depth max-depth)
  "Return a depth-first flat list of navigable POSTS.
DEPTH defaults to 0 and MAX-DEPTH defaults to 1, so top-level posts and
one quoted-post level are included."
  (let ((depth (or depth 0))
        (max-depth (or max-depth 1))
        items)
    (dolist (post posts (nreverse items))
      (let ((id (bluesky--post-id post)))
        (push (list :id id :post post :depth depth) items)
        (when (< depth max-depth)
          (dolist (child (bluesky--flatten-posts
                          (delq nil (list (bluesky--quoted-post post)))
                          (1+ depth)
                          max-depth))
            (push child items)))))))

(defun bluesky--flatten-thread (thread &optional depth)
  "Return a flat, depth-first list of posts from THREAD.
THREAD is an `app.bsky.feed.defs#threadViewPost' shape."
  (let ((depth (or depth 0))
        (post (plist-get thread :post))
        items)
    (when post
      (push (list :id (bluesky--post-id post) :post post :depth depth) items))
    (dolist (reply (append (plist-get thread :replies) nil))
      (dolist (child (bluesky--flatten-thread reply (1+ depth)))
        (push child items)))
    (nreverse items)))

(defun bluesky--thread-ancestors (thread)
  "Return THREAD's ancestor chain as root-first thread nodes."
  (let (ancestors)
    (while (plist-get thread :parent)
      (setq thread (plist-get thread :parent))
      (push thread ancestors))
    ancestors))

(defun bluesky--thread-items (thread)
  "Return a flat list of renderable items for THREAD, including ancestors."
  (let (items)
    (dolist (ancestor (bluesky--thread-ancestors thread))
      (let ((post (plist-get ancestor :post)))
        (when post
          (push (list :id (bluesky--post-id post) :post post :depth 0)
                items))))
    (append (nreverse items) (bluesky--flatten-thread thread))))

(defun bluesky--timeline-state (key)
  "Return timeline component state KEY in the current buffer."
  (when bluesky-feed-root
    (plist-get (vui-instance-state bluesky-feed-root) key)))

(defun bluesky--selected-item ()
  "Return the selected timeline/thread item in the current buffer."
  (let* ((items (bluesky--timeline-state :items))
         (selected-id (bluesky--timeline-state :selected-id)))
    (or (seq-find (lambda (item)
                    (equal selected-id (plist-get item :id)))
                  items)
        (car items))))

(defun bluesky--selected-post ()
  "Return the selected post in the current buffer."
  (plist-get (bluesky--selected-item) :post))

(defun bluesky--json-truthy-p (value)
  "Return non-nil when VALUE represents true in Bluesky JSON data."
  (and value (not (eq value :json-false))))

(defun bluesky--set-timeline-state (key value)
  "Set timeline component state KEY to VALUE in the current buffer."
  (unless bluesky-feed-root
    (user-error "No Bluesky feed is active in this buffer"))
  (let ((vui--root-instance bluesky-feed-root)
        (vui--current-instance bluesky-feed-root))
    (vui-set-state key value)))

(defun bluesky--item-bounds (item-id)
  "Return buffer bounds for navigable ITEM-ID."
  (let ((pos (point-min))
        start end)
    (while (< pos (point-max))
      (let* ((next (next-single-property-change
                    pos 'bluesky-item-id nil (point-max)))
             (value (get-text-property pos 'bluesky-item-id)))
        (when (equal value item-id)
          (setq start (or start pos))
          (setq end next))
        (setq pos (max (1+ pos) next))))
    (when (and start end)
      (cons (save-excursion
              (goto-char start)
              (line-beginning-position))
            (save-excursion
              (goto-char end)
              (line-end-position))))))

(defun bluesky--highlight-selected (item-id)
  "Highlight ITEM-ID in the current buffer."
  (when (and (overlayp bluesky-current-post-overlay)
             (overlay-buffer bluesky-current-post-overlay))
    (delete-overlay bluesky-current-post-overlay)
    (setq bluesky-current-post-overlay nil))
  (when-let* ((bounds (and item-id (bluesky--item-bounds item-id))))
    (setq bluesky-current-post-overlay (make-overlay (car bounds) (cdr bounds)))
    (overlay-put bluesky-current-post-overlay 'face 'bluesky-current-post)
    (overlay-put bluesky-current-post-overlay 'priority 10)
    (goto-char (car bounds))))

(defun bluesky--schedule-highlight (item-id)
  "Highlight ITEM-ID after the current render cycle settles."
  (let ((buffer (current-buffer)))
    (run-with-timer
     0.05 nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (bluesky--highlight-selected item-id)))))))

(defun bluesky--highlight-current ()
  "Reapply the highlight for the current selected timeline item."
  (when (and (bound-and-true-p bluesky-feed-root)
             (buffer-live-p (current-buffer)))
    (bluesky--schedule-highlight
     (plist-get (vui-instance-state bluesky-feed-root) :selected-id))))

(defun bluesky--highlight-current-now ()
  "Immediately reapply the highlight for the current selected timeline item."
  (when (and (bound-and-true-p bluesky-feed-root)
             (buffer-live-p (current-buffer)))
    (bluesky--highlight-selected
     (plist-get (vui-instance-state bluesky-feed-root) :selected-id))))

(defun bluesky--move-selection (delta)
  "Move current timeline selection by DELTA."
  (let* ((items (bluesky--timeline-state :items))
         (ids (mapcar (lambda (item) (plist-get item :id)) items))
         (selected-id (bluesky--timeline-state :selected-id))
         (index (or (cl-position selected-id ids :test #'equal) 0))
         (next-index (max 0 (min (1- (length ids)) (+ index delta)))))
    (unless ids
      (user-error "No posts loaded"))
    (let ((next-id (nth next-index ids)))
      (bluesky--set-timeline-state :selected-id next-id)
      (bluesky--schedule-highlight next-id))))

(defun bluesky-feed-refresh ()
  "Refresh the Bluesky feed."
  (interactive)
  (if bluesky-feed-root
      (let ((vui--root-instance bluesky-feed-root)
            (vui--current-instance bluesky-feed-root))
        (vui-set-state :refresh-requested (current-time)))
    (user-error "No Bluesky feed is active in this buffer")))

(defun bluesky-feed-extend ()
  "Load the Bluesky feed."
  (interactive)
  (if bluesky-feed-root
      (let ((vui--root-instance bluesky-feed-root)
            (vui--current-instance bluesky-feed-root))
        (vui-set-state :extend-requested (current-time)))
    (user-error "No Bluesky feed is active in this buffer")))

(defun bluesky-feed-next-post ()
  "Move to the next post in the timeline."
  (interactive)
  (bluesky--move-selection 1))

(defun bluesky-feed-previous-post ()
  "Move to the previous post in the timeline."
  (interactive)
  (bluesky--move-selection -1))

(defun bluesky--facet-open-targets (record)
  "Return openable link targets from RECORD facets."
  (let (targets)
    (dolist (facet (append (plist-get record :facets) nil))
      (dolist (feature (append (plist-get facet :features) nil))
        (when (equal (plist-get feature :$type) "app.bsky.richtext.facet#link")
          (when-let* ((uri (plist-get feature :uri)))
            (push (cons uri uri) targets)))))
    (nreverse targets)))

(defun bluesky--embed-open-targets (embed)
  "Return openable link and media targets from EMBED."
  (let (targets)
    (when embed
      (when-let* ((external (plist-get embed :external))
                  (uri (plist-get external :uri)))
        (push (cons (format "External: %s" uri) uri) targets))
      (dolist (image (append (plist-get embed :images) nil))
        (when-let* ((url (or (plist-get image :fullsize)
                             (plist-get image :thumb))))
          (push (cons (format "Image: %s" url) url) targets)))
      (when-let* ((playlist (plist-get embed :playlist)))
        (push (cons (format "Video playlist: %s" playlist) playlist) targets))
      (when-let* ((thumbnail (plist-get embed :thumbnail)))
        (push (cons (format "Video thumbnail: %s" thumbnail) thumbnail) targets))
      (when-let* ((media (plist-get embed :media)))
        (setq targets (append (bluesky--embed-open-targets media) targets))))
    (nreverse targets)))

(defun bluesky--post-open-targets (post)
  "Return openable targets from POST."
  (let* ((record (plist-get post :record))
         (targets (append (bluesky--facet-open-targets record)
                          (bluesky--embed-open-targets (plist-get post :embed))
                          (bluesky--embed-open-targets (plist-get record :embed)))))
    (seq-uniq targets (lambda (a b) (equal (cdr a) (cdr b))))))

(defun bluesky-open-current ()
  "Open a link or media URL from the selected post."
  (interactive)
  (let* ((post (or (bluesky--selected-post)
                   (user-error "No post selected")))
         (targets (bluesky--post-open-targets post)))
    (unless targets
      (user-error "Selected post has no links or media to open"))
    (let* ((target (if (= (length targets) 1)
                       (car targets)
                     (let* ((choices (mapcar #'car targets))
                            (choice (completing-read "Open: " choices nil t)))
                       (assoc choice targets))))
           (url (cdr target)))
      (browse-url url))))

(defun bluesky--request-refresh ()
  "Request a refresh for the active Bluesky VUI component."
  (if bluesky-feed-root
      (let ((vui--root-instance bluesky-feed-root)
            (vui--current-instance bluesky-feed-root))
        (vui-set-state :refresh-requested (current-time)))
    (user-error "No Bluesky feed is active in this buffer")))

(defun bluesky--run-post-action (description future)
  "Run FUTURE for a post action described by DESCRIPTION."
  (let ((buffer (current-buffer)))
    (futur-bind
     future
     (lambda (_value)
       (message "Bluesky: %s" description)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (bluesky--request-refresh))))
     (lambda (err)
       (message "Bluesky: %s failed: %s"
                description
                (bluesky--error-message err))
       (futur-failed err)))))

(defun bluesky-toggle-like ()
  "Like or unlike the selected post."
  (interactive)
  (let* ((post (or (bluesky--selected-post)
                   (user-error "No post selected")))
         (viewer (plist-get post :viewer))
         (like-uri (plist-get viewer :like)))
    (bluesky--run-post-action
     (if like-uri "unliked post" "liked post")
     (if like-uri
         (bluesky-conn-delete-record bluesky-host
                                     (plist-get bluesky-feed-session :handle)
                                     like-uri)
       (bluesky-conn-create-like bluesky-host
                                 (plist-get bluesky-feed-session :handle)
                                 post)))))

(defun bluesky-toggle-repost ()
  "Repost or unrepost the selected post."
  (interactive)
  (let* ((post (or (bluesky--selected-post)
                   (user-error "No post selected")))
         (viewer (plist-get post :viewer))
         (repost-uri (plist-get viewer :repost)))
    (bluesky--run-post-action
     (if repost-uri "removed repost" "reposted post")
     (if repost-uri
         (bluesky-conn-delete-record bluesky-host
                                     (plist-get bluesky-feed-session :handle)
                                     repost-uri)
       (bluesky-conn-create-repost bluesky-host
                                   (plist-get bluesky-feed-session :handle)
                                   post)))))

(defun bluesky-toggle-bookmark ()
  "Bookmark or unbookmark the selected post."
  (interactive)
  (let* ((post (or (bluesky--selected-post)
                   (user-error "No post selected")))
         (viewer (plist-get post :viewer))
         (bookmarked (bluesky--json-truthy-p (plist-get viewer :bookmarked))))
    (bluesky--run-post-action
     (if bookmarked "removed bookmark" "bookmarked post")
     (if bookmarked
         (bluesky-conn-delete-bookmark bluesky-host
                                       (plist-get bluesky-feed-session :handle)
                                       post)
       (bluesky-conn-create-bookmark bluesky-host
                                     (plist-get bluesky-feed-session :handle)
                                     post)))))

(defun bluesky-reply (text)
  "Reply to the selected post with TEXT."
  (interactive
   (list (read-string "Reply: ")))
  (unless (string-empty-p text)
    (let* ((post (or (bluesky--selected-post)
                     (user-error "No post selected")))
           (viewer (plist-get post :viewer)))
      (when (bluesky--json-truthy-p (plist-get viewer :replyDisabled))
        (user-error "Replies are disabled for this post"))
      (bluesky--run-post-action
       "replied to post"
       (bluesky-conn-create-reply bluesky-host
                                  (plist-get bluesky-feed-session :handle)
                                  post
                                  text)))))

(defun bluesky-open-thread ()
  "Open a thread view for the selected post."
  (interactive)
  (let* ((post (or (bluesky--selected-post)
                   (user-error "No post selected")))
         (uri (or (plist-get post :uri)
                  (user-error "Selected post does not have a URI")))
         (host bluesky-host)
         (session bluesky-feed-session)
         (handle (plist-get session :handle)))
    (let ((buffer (get-buffer-create bluesky-thread-buffer-name)))
      (with-current-buffer buffer
        (bluesky-mode))
      (let ((root (vui-mount
                   (vui-component 'bluesky-thread
                     :host host
                     :handle handle
                     :uri uri)
                   bluesky-thread-buffer-name)))
        (with-current-buffer (vui-instance-buffer root)
          (setq-local vui--root-instance root)
          (setq-local bluesky--navigation-override-mode t)
          (setq-local bluesky-current-post-overlay nil)
          (setq-local bluesky-host host)
          (setq-local bluesky-feed-session session)
          (setq-local bluesky-feed-root root))
        (pop-to-buffer (vui-instance-buffer root))))))

(vui-defcomponent bluesky-timeline (host handle)
  "Render a Bluesky timeline."
  :state ((posts nil)
          (cursor nil)
          (loading nil)
          (error nil)
          (items nil)
          (selected-id nil)
          (refresh-requested nil)
          (extend-requested nil))
  :render
  (progn
    (let ((current-items (bluesky--flatten-posts posts)))
      (vui-use-effect (posts selected-id)
        (let ((ids (mapcar (lambda (item) (plist-get item :id)) current-items)))
          (vui-batch
           (vui-set-state :items current-items)
           (when (and ids (not (member selected-id ids)))
             (vui-set-state :selected-id (car ids)))))
        nil))
    (vui-use-effect (selected-id items)
      (bluesky--schedule-highlight selected-id)
      nil)
    (vui-use-effect (host handle refresh-requested)
      (vui-batch
       (vui-set-state :loading t)
       (vui-set-state :error nil))
      (bluesky--future-set-state
       (bluesky-conn-get-timeline host handle nil 50)
       :posts
       (lambda (feed)
         (vui-batch
          (vui-set-state :cursor (plist-get feed :cursor)))
         (mapcar (lambda (entry) (plist-get entry :post))
                 (append (plist-get feed :feed) nil)))
       :error)
      nil)
    (vui-use-effect (extend-requested)
      (when (and extend-requested cursor (not loading))
        (vui-batch
         (vui-set-state :loading t)
         (vui-set-state :error nil))
        (bluesky--future-set-state
         (bluesky-conn-get-timeline host handle cursor 50)
         :posts
         (lambda (feed)
           (vui-batch
            (vui-set-state :cursor (plist-get feed :cursor)))
           (append posts
                   (mapcar (lambda (entry) (plist-get entry :post))
                           (append (plist-get feed :feed) nil))))
         :error))
      nil)
    (vui-vstack
     (vui-hstack
      (vui-text (format "Timeline for %s" handle) :face 'bluesky-heading)
      (vui-button "Refresh"
        :on-click (lambda ()
                    (vui-set-state :refresh-requested (current-time)))))
     (when error
       (vui-text (bluesky--error-message error) :face 'bluesky-error))
     (when loading
       (vui-text "Loading..." :face 'bluesky-muted))
     (if items
         (vui-list items
                   (lambda (item)
                     (bluesky-ui-post host
                                      (plist-get item :post)
                                      (plist-get item :id)
                                      (plist-get item :depth)))
                   (lambda (item) (plist-get item :id))
                   :spacing 1)
       (unless loading
         (vui-text "No posts loaded." :face 'bluesky-muted)))
     (when cursor
       (vui-button "Load more"
         :on-click (lambda ()
                     (vui-set-state :extend-requested (current-time))))))))

(vui-defcomponent bluesky-thread (host handle uri)
  "Render the thread containing URI."
  :state ((thread nil)
          (loading nil)
          (error nil)
          (items nil)
          (selected-id nil)
          (refresh-requested nil))
  :render
  (progn
    (let ((current-items (and thread (bluesky--thread-items thread))))
      (vui-use-effect (thread selected-id)
        (let ((ids (mapcar (lambda (item) (plist-get item :id)) current-items)))
          (vui-batch
           (vui-set-state :items current-items)
           (when (and ids (not (member selected-id ids)))
             (vui-set-state :selected-id (car ids)))))
        nil))
    (vui-use-effect (selected-id items)
      (bluesky--schedule-highlight selected-id)
      nil)
    (vui-use-effect (host handle uri refresh-requested)
      (vui-batch
       (vui-set-state :loading t)
       (vui-set-state :error nil))
      (bluesky--future-set-state
       (bluesky-conn-get-post-thread host handle uri 10 20)
       :thread
       (lambda (resp) (plist-get resp :thread))
       :error)
      nil)
    (vui-vstack
     (vui-text "Thread" :face 'bluesky-heading)
     (when error
       (vui-text (bluesky--error-message error) :face 'bluesky-error))
     (when loading
       (vui-text "Loading..." :face 'bluesky-muted))
     (if items
         (vui-list items
                   (lambda (item)
                     (bluesky-ui-post host
                                      (plist-get item :post)
                                      (plist-get item :id)
                                      (plist-get item :depth)))
                   (lambda (item) (plist-get item :id))
                   :spacing 1)
       (unless loading
         (vui-text "No thread posts loaded." :face 'bluesky-muted))))))

(defun bluesky (&optional username password host)
  "Connect to a Bluesky server and render the user's feed.

USERNAME is the username to connect with.  It should not include the @
prefix.  This can be nil, and if so, the user will be found via
`auth-source-search'.  Otherwise, the user will be prompted for the
username.

PASSWORD is the password to connect with.  This can be nil, and if so,
the password will be found via `auth-source-search'.  Otherwise, the
user will be prompted for the password."
  (interactive)
  (let* ((host (or host bluesky-default-host))
         (authinfo (car (auth-source-search :host host)))
         (username (or username
                       (when authinfo
                         (plist-get authinfo :user))
                       (read-string "Username: ")))
         (password (or password
                       (when authinfo
                         (plist-get authinfo :secret))
                       (read-passwd "Password: ")))
         (cached-session (and username
                              (bluesky-conn-get-session host username))))
    (unless (and username password)
      (error "Username and password are required"))
    (message "Bluesky: using %s for %s"
             (cond
              (cached-session "cached session")
              (authinfo "auth-source credentials")
              (t "prompted credentials"))
             host)
    (futur-bind
     (or cached-session
         (bluesky-conn-create-session host username password))
     (lambda (session)
       (message "Bluesky: connected as %s" (plist-get session :handle))
       (let ((buffer (get-buffer-create bluesky-timeline-buffer-name)))
         (with-current-buffer buffer
           (bluesky-mode))
         (let ((root (vui-mount
                      (vui-component 'bluesky-timeline
                        :host host
                        :handle (plist-get session :handle))
                      bluesky-timeline-buffer-name)))
           (with-current-buffer (vui-instance-buffer root)
             (setq-local vui--root-instance root)
             (setq-local bluesky--navigation-override-mode t)
             (setq-local bluesky-current-post-overlay nil)
             (setq-local bluesky-host host)
             (setq-local bluesky-feed-session session)
             (setq-local bluesky-feed-root root))
           root)))
     (lambda (err)
       (message "Unable to connect to Bluesky: %s"
                (bluesky--error-message err))
       (futur-failed err)))))

(provide 'bluesky)

;;; bluesky.el ends here
