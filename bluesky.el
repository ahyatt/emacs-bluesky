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
(require 'bluesky-post)
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

(defconst bluesky-author-timeline-buffer-name-format "*Bluesky Author: %s*"
  "Format string for Bluesky author timeline buffer names.")

(defconst bluesky-search-timeline-buffer-name-format "*Bluesky Search: %s*"
  "Format string for Bluesky search timeline buffer names.")

(defconst bluesky-tag-timeline-buffer-name-format "*Bluesky Tag: #%s*"
  "Format string for Bluesky tag timeline buffer names.")

(defconst bluesky-thread-buffer-name "*Bluesky Thread*"
  "The name of the Bluesky thread buffer.")

(defconst bluesky-thread-buffer-snippet-width 48
  "Maximum width of the snippet in Bluesky thread buffer names.")

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
  '((((class color) (background dark))
     :background "#2b3f4a" :extend t)
    (((class color) (background light))
     :background "#e5f4fb" :extend t)
    (t :inherit highlight :extend t))
  "Face for the currently selected Bluesky post.")

(defvar-local bluesky--navigation-override-mode nil
  "Non-nil when Bluesky navigation keys should override minor modes.")

(defvar bluesky--navigation-override-map
  (make-sparse-keymap)
  "High-precedence keymap for Bluesky navigation.")

(add-to-list 'emulation-mode-map-alists
             `((bluesky--navigation-override-mode
                . ,bluesky--navigation-override-map)))

(defvar bluesky-mode-map
  (make-sparse-keymap)
  "Keymap for Bluesky feed mode.")

(define-key bluesky--navigation-override-map (kbd "j") #'bluesky-feed-next-post)
(define-key bluesky--navigation-override-map (kbd "k") #'bluesky-feed-previous-post)
(define-key bluesky--navigation-override-map (kbd "n") #'bluesky-compose-post)
(define-key bluesky--navigation-override-map (kbd "o") #'bluesky-open-current)
(define-key bluesky--navigation-override-map (kbd "L") #'bluesky-toggle-like)
(define-key bluesky--navigation-override-map (kbd "R") #'bluesky-toggle-repost)
(define-key bluesky--navigation-override-map (kbd "b") #'bluesky-toggle-bookmark)
(define-key bluesky--navigation-override-map (kbd "r") #'bluesky-reply)
(define-key bluesky--navigation-override-map (kbd "RET") nil)

(define-key bluesky-mode-map (kbd "g") #'bluesky-feed-refresh)
(define-key bluesky-mode-map (kbd "j") #'bluesky-feed-next-post)
(define-key bluesky-mode-map (kbd "k") #'bluesky-feed-previous-post)
(define-key bluesky-mode-map (kbd "l") #'bluesky-feed-extend)
(define-key bluesky-mode-map (kbd "n") #'bluesky-compose-post)
(define-key bluesky-mode-map (kbd "o") #'bluesky-open-current)
(define-key bluesky-mode-map (kbd "L") #'bluesky-toggle-like)
(define-key bluesky-mode-map (kbd "R") #'bluesky-toggle-repost)
(define-key bluesky-mode-map (kbd "b") #'bluesky-toggle-bookmark)
(define-key bluesky-mode-map (kbd "r") #'bluesky-reply)
(define-key bluesky-mode-map (kbd "RET") #'bluesky-activate-or-open-thread)

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
  "Overlay or overlays highlighting the current Bluesky post.")

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

(defun bluesky--item-id (post &optional parent-id)
  "Return the navigable item id for POST, optionally quoted under PARENT-ID."
  (if parent-id
      (bluesky-ui--quoted-post-item-id parent-id post)
    (bluesky--post-id post)))

(defun bluesky--unique-item-id (base-id seen)
  "Return a unique item id from BASE-ID, updating SEEN."
  (let* ((count (1+ (or (gethash base-id seen) 0)))
         (id (if (= count 1)
                 base-id
               (format "%s#%d" base-id count))))
    (puthash base-id count seen)
    id))

(defun bluesky--record-view-as-post (record-view)
  "Return RECORD-VIEW as a post view plist, when possible."
  (when (and record-view
             (plist-get record-view :author)
             (plist-get record-view :value))
    (let ((post (copy-sequence record-view)))
      (setq post (plist-put post :record (plist-get record-view :value)))
      (if (plist-get post :embed)
          post
        (plist-put post :embed
                   (and (> (length (plist-get post :embeds)) 0)
                        (aref (plist-get post :embeds) 0)))))))

(defun bluesky--embedded-record-wrapper-p (record-view)
  "Return non-nil when RECORD-VIEW is a wrapper around another record view."
  (let ((type (plist-get record-view :$type)))
    (and (plist-get record-view :record)
         (not (bluesky--record-view-as-post record-view))
         (or (null type)
             (equal type "app.bsky.embed.record#view")))))

(defun bluesky--embedded-record-post (record-view)
  "Return the post view embedded in RECORD-VIEW, when possible."
  (when record-view
    (if (bluesky--embedded-record-wrapper-p record-view)
        (bluesky--embedded-record-post (plist-get record-view :record))
      (bluesky--record-view-as-post record-view))))

(defun bluesky--embedded-quote-posts (embed)
  "Return quoted post views embedded in EMBED."
  (let (posts)
    (when embed
      (when-let* ((media (plist-get embed :media)))
        (setq posts (append posts (bluesky--embedded-quote-posts media))))
      (when-let* ((record (plist-get embed :record))
                  (post (bluesky--embedded-record-post record)))
        (push post posts)))
    (nreverse posts)))

(defun bluesky--post-quote-posts (post)
  "Return quoted post views embedded in POST."
  (let ((record (plist-get post :record)))
    (bluesky--embedded-quote-posts
     (or (plist-get post :embed)
         (plist-get record :embed)))))

(defun bluesky--flatten-posts (posts &optional depth render parent-id seen)
  "Return a flat list of navigable POSTS.
RENDER is non-nil when POSTS should be rendered as top-level list entries."
  (let ((depth (or depth 0))
        (seen (or seen (make-hash-table :test 'equal)))
        items)
    (dolist (post posts (nreverse items))
      (let ((id (bluesky--unique-item-id
                 (bluesky--item-id post parent-id)
                 seen)))
        (push (list :id id :post post :depth depth :render render) items)
        (dolist (quote (bluesky--flatten-posts
                        (bluesky--post-quote-posts post)
                        (1+ depth)
                        nil
                        id
                        seen))
          (push quote items))))))

(defun bluesky--flatten-thread (thread &optional depth seen)
  "Return a flat, depth-first list of posts from THREAD.
THREAD is an `app.bsky.feed.defs#threadViewPost' shape."
  (let ((depth (or depth 0))
        (seen (or seen (make-hash-table :test 'equal)))
        (post (plist-get thread :post))
        items)
    (when post
      (dolist (item (bluesky--flatten-posts (list post) depth t nil seen))
        (push item items)))
    (dolist (reply (append (plist-get thread :replies) nil))
      (dolist (child (bluesky--flatten-thread reply (1+ depth) seen))
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
  (let ((seen (make-hash-table :test 'equal))
        items)
    (dolist (ancestor (bluesky--thread-ancestors thread))
      (let ((post (plist-get ancestor :post)))
        (when post
          (dolist (item (bluesky--flatten-posts (list post) 0 t nil seen))
            (push item items)))))
    (append (nreverse items) (bluesky--flatten-thread thread nil seen))))

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

(defun bluesky--clean-buffer-name-snippet (text)
  "Return TEXT normalized for use in a Bluesky buffer name."
  (let ((snippet (string-trim
                  (replace-regexp-in-string "[ \t\n\r]+" " " (or text "")))))
    (unless (string-empty-p snippet)
      snippet)))

(defun bluesky--post-buffer-name-snippet (post)
  "Return a readable buffer-name snippet for POST."
  (let* ((record (plist-get post :record))
         (author (plist-get post :author))
         (snippet (or (bluesky--clean-buffer-name-snippet
                       (plist-get record :text))
                      (bluesky--clean-buffer-name-snippet
                       (plist-get author :handle))
                      (bluesky--clean-buffer-name-snippet
                       (plist-get post :uri))
                      "post")))
    (if (> (string-width snippet) bluesky-thread-buffer-snippet-width)
        (truncate-string-to-width snippet
                                  bluesky-thread-buffer-snippet-width
                                  nil
                                  nil
                                  "...")
      snippet)))

(defun bluesky--thread-buffer-name (post)
  "Return a thread buffer name for POST."
  (format "*Bluesky Thread: %s*" (bluesky--post-buffer-name-snippet post)))

(defun bluesky--normalize-actor (actor)
  "Return ACTOR as an AT identifier without a leading at sign."
  (let ((actor (string-remove-prefix "@" (string-trim (or actor "")))))
    (when (string-empty-p actor)
      (user-error "Actor is required"))
    actor))

(defun bluesky--post-author-actor (post)
  "Return the best AT identifier for POST's author."
  (when-let* ((author (plist-get post :author)))
    (or (plist-get author :handle)
        (plist-get author :did))))

(defun bluesky--selected-author-actor ()
  "Return the selected post author's AT identifier, if available."
  (when-let* ((post (and (bound-and-true-p bluesky-feed-root)
                         (bluesky--selected-post))))
    (bluesky--post-author-actor post)))

(defun bluesky--read-actor (&optional prompt)
  "Read an AT actor identifier with PROMPT."
  (let* ((default (bluesky--selected-author-actor))
         (prompt (or prompt "Actor"))
         (input (read-string
                 (if default
                     (format "%s (default %s): " prompt default)
                   (format "%s: " prompt))
                 nil nil default)))
    (bluesky--normalize-actor input)))

(defun bluesky--author-timeline-buffer-name (actor)
  "Return an author timeline buffer name for ACTOR."
  (format bluesky-author-timeline-buffer-name-format actor))

(defun bluesky--normalize-search-query (query)
  "Return QUERY trimmed for Bluesky search."
  (let ((query (string-trim (or query ""))))
    (when (string-empty-p query)
      (user-error "Search query is required"))
    query))

(defun bluesky--normalize-tag (tag)
  "Return TAG without a leading hash."
  (let ((tag (string-remove-prefix "#" (string-trim (or tag "")))))
    (when (string-empty-p tag)
      (user-error "Tag is required"))
    tag))

(defun bluesky--read-search-query ()
  "Read a Bluesky search query."
  (bluesky--normalize-search-query (read-string "Search: ")))

(defun bluesky--read-tag ()
  "Read a Bluesky tag."
  (bluesky--normalize-tag (read-string "Tag: ")))

(defun bluesky--search-timeline-buffer-name (query)
  "Return a search timeline buffer name for QUERY."
  (format bluesky-search-timeline-buffer-name-format query))

(defun bluesky--tag-timeline-buffer-name (tag)
  "Return a tag timeline buffer name for TAG."
  (format bluesky-tag-timeline-buffer-name-format tag))

(defun bluesky--set-timeline-state (key value)
  "Set timeline component state KEY to VALUE in the current buffer."
  (unless bluesky-feed-root
    (user-error "No Bluesky feed is active in this buffer"))
  (let ((vui--root-instance bluesky-feed-root)
        (vui--current-instance bluesky-feed-root))
    (vui-set-state key value)))

(defun bluesky--item-bounds (item-id)
  "Return buffer bounds for each rendered range of navigable ITEM-ID."
  (let ((pos (point-min))
        bounds)
    (while (< pos (point-max))
      (let* ((next (next-single-property-change
                    pos 'bluesky-item-id nil (point-max)))
             (value (get-text-property pos 'bluesky-item-id)))
        (when (equal value item-id)
          (push (cons (save-excursion
                        (goto-char pos)
                        (line-beginning-position))
                      (save-excursion
                        (goto-char next)
                        (line-end-position)))
                bounds))
        (setq pos (max (1+ pos) next))))
    (let (coalesced)
      (dolist (bounds (nreverse bounds) (nreverse coalesced))
        (if-let* ((previous (car coalesced))
                  (_ (<= (car bounds) (cdr previous))))
            (setcdr previous (max (cdr previous) (cdr bounds)))
          (push bounds coalesced))))))

(defun bluesky--highlight-selected (item-id)
  "Highlight ITEM-ID in the current buffer."
  (dolist (overlay (if (listp bluesky-current-post-overlay)
                       bluesky-current-post-overlay
                     (list bluesky-current-post-overlay)))
    (when (and (overlayp overlay) (overlay-buffer overlay))
      (delete-overlay overlay)))
  (setq bluesky-current-post-overlay nil)
  (when-let* ((bounds-list (and item-id (bluesky--item-bounds item-id))))
    (setq bluesky-current-post-overlay
          (mapcar (lambda (bounds)
                    (let ((overlay (make-overlay (car bounds) (cdr bounds))))
                      (overlay-put overlay 'face 'bluesky-current-post)
                      (overlay-put overlay 'priority 10)
                      overlay))
                  bounds-list))
    (goto-char (caar bounds-list))))

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
         (host bluesky-host)
         (session bluesky-feed-session)
         (author (plist-get post :author))
         (actor (bluesky--post-author-actor post))
         (author-target
          (when actor
            (cons (format "Author timeline: @%s"
                          (or (plist-get author :handle) actor))
                  (lambda ()
                    (bluesky--open-author-timeline actor host session)))))
         (targets (append (bluesky--facet-open-targets record)
                          (bluesky--embed-open-targets (plist-get post :embed))
                          (bluesky--embed-open-targets (plist-get record :embed))
                          (when author-target
                            (list author-target)))))
    (seq-uniq targets (lambda (a b) (equal (cdr a) (cdr b))))))

(defun bluesky-open-current ()
  "Open a link, media URL, or author timeline from the selected post."
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
           (action (cdr target)))
      (if (functionp action)
          (funcall action)
        (browse-url action)))))

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

(defun bluesky-compose-post ()
  "Open a buffer to compose a new Bluesky post."
  (interactive)
  (bluesky-post-compose
   :host bluesky-host
   :session bluesky-feed-session
   :source-buffer (current-buffer)))

(defun bluesky-reply ()
  "Open a buffer to reply to the selected post."
  (interactive)
  (let ((post (or (bluesky--selected-post)
                  (user-error "No post selected"))))
    (when (bluesky-post-reply-disabled-p post)
      (user-error "Replies are disabled for this post"))
    (bluesky-post-compose
     :host bluesky-host
     :session bluesky-feed-session
     :reply-to post
     :source-buffer (current-buffer))))

(defun bluesky--mount-feed-buffer (buffer-name component host session &optional pop)
  "Mount COMPONENT in BUFFER-NAME using HOST and SESSION.
When POP is non-nil, display the resulting buffer."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (bluesky-mode))
    (let ((root (vui-mount component buffer-name)))
      (with-current-buffer (vui-instance-buffer root)
        (setq-local vui--root-instance root)
        (setq-local bluesky--navigation-override-mode t)
        (setq-local bluesky-current-post-overlay nil)
        (setq-local bluesky-host host)
        (setq-local bluesky-feed-session session)
        (setq-local bluesky-feed-root root))
      (when pop
        (pop-to-buffer (vui-instance-buffer root)))
      root)))

(defun bluesky--open-author-timeline (actor host session)
  "Open ACTOR's author timeline on HOST using SESSION."
  (let ((actor (bluesky--normalize-actor actor)))
    (bluesky--mount-feed-buffer
     (bluesky--author-timeline-buffer-name actor)
     (vui-component 'bluesky-author-timeline
       :host host
       :handle (plist-get session :handle)
       :actor actor)
     host
     session
     t)))

(defun bluesky--open-search-timeline (query host session &optional tags title buffer-name)
  "Open a search timeline for QUERY on HOST using SESSION.
TAGS is an optional vector of tag filters.  TITLE and BUFFER-NAME customize the
rendered heading and buffer."
  (let ((query (bluesky--normalize-search-query query)))
    (bluesky--mount-feed-buffer
     (or buffer-name (bluesky--search-timeline-buffer-name query))
     (vui-component 'bluesky-search-timeline
       :host host
       :handle (plist-get session :handle)
       :query query
       :tags tags
       :title (or title (format "Search for %s" query)))
     host
     session
     t)))

(defun bluesky--open-tag-timeline (tag host session)
  "Open a tag timeline for TAG on HOST using SESSION."
  (let* ((tag (bluesky--normalize-tag tag))
         (query (concat "#" tag)))
    (bluesky--open-search-timeline
     query
     host
     session
     (vector tag)
     (format "Tag #%s" tag)
     (bluesky--tag-timeline-buffer-name tag))))

(defun bluesky-open-thread ()
  "Open a thread view for the selected post."
  (interactive)
  (let* ((post (or (bluesky--selected-post)
                   (user-error "No post selected")))
         (uri (or (plist-get post :uri)
                  (user-error "Selected post does not have a URI")))
         (host bluesky-host)
         (session bluesky-feed-session)
         (handle (plist-get session :handle))
         (buffer-name (bluesky--thread-buffer-name post)))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (bluesky-mode))
      (let ((root (vui-mount
                   (vui-component 'bluesky-thread
                     :host host
                     :handle handle
                     :uri uri)
                   buffer-name)))
        (with-current-buffer (vui-instance-buffer root)
          (setq-local vui--root-instance root)
          (setq-local bluesky--navigation-override-mode t)
          (setq-local bluesky-current-post-overlay nil)
          (setq-local bluesky-host host)
          (setq-local bluesky-feed-session session)
          (setq-local bluesky-feed-root root))
        (pop-to-buffer (vui-instance-buffer root))))))

(defun bluesky-activate-or-open-thread ()
  "Activate the widget at point, or open the selected post thread."
  (interactive)
  (if (widget-at)
      (widget-button-press (point))
    (bluesky-open-thread)))

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
    (let ((current-items (bluesky--flatten-posts posts 0 t)))
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
         (vui-list (seq-filter (lambda (item) (plist-get item :render)) items)
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

(vui-defcomponent bluesky-author-timeline (host handle actor)
  "Render ACTOR's Bluesky author timeline."
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
    (let ((current-items (bluesky--flatten-posts posts 0 t)))
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
    (vui-use-effect (host handle actor refresh-requested)
      (vui-batch
       (vui-set-state :loading t)
       (vui-set-state :error nil))
      (bluesky--future-set-state
       (bluesky-conn-get-author-feed host handle actor nil 50)
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
         (bluesky-conn-get-author-feed host handle actor cursor 50)
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
      (vui-text (format "Author timeline for %s" actor) :face 'bluesky-heading)
      (vui-button "Refresh"
        :on-click (lambda ()
                    (vui-set-state :refresh-requested (current-time)))))
     (when error
       (vui-text (bluesky--error-message error) :face 'bluesky-error))
     (when loading
       (vui-text "Loading..." :face 'bluesky-muted))
     (if items
         (vui-list (seq-filter (lambda (item) (plist-get item :render)) items)
                   (lambda (item)
                     (bluesky-ui-post host
                                      (plist-get item :post)
                                      (plist-get item :id)
                                      (plist-get item :depth)))
                   (lambda (item) (plist-get item :id))
                   :spacing 1)
       (unless loading
         (vui-text "No author posts loaded." :face 'bluesky-muted)))
     (when cursor
       (vui-button "Load more"
         :on-click (lambda ()
                     (vui-set-state :extend-requested (current-time))))))))

(vui-defcomponent bluesky-search-timeline (host handle query tags title)
  "Render Bluesky search results for QUERY."
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
    (let ((current-items (bluesky--flatten-posts posts 0 t)))
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
    (vui-use-effect (host handle query tags refresh-requested)
      (vui-batch
       (vui-set-state :loading t)
       (vui-set-state :error nil))
      (bluesky--future-set-state
       (bluesky-conn-search-posts host handle query nil 50 "latest" tags)
       :posts
       (lambda (results)
         (vui-batch
          (vui-set-state :cursor (plist-get results :cursor)))
         (append (plist-get results :posts) nil))
       :error)
      nil)
    (vui-use-effect (extend-requested)
      (when (and extend-requested cursor (not loading))
        (vui-batch
         (vui-set-state :loading t)
         (vui-set-state :error nil))
        (bluesky--future-set-state
         (bluesky-conn-search-posts host handle query cursor 50 "latest" tags)
         :posts
         (lambda (results)
           (vui-batch
            (vui-set-state :cursor (plist-get results :cursor)))
           (append posts (append (plist-get results :posts) nil)))
         :error))
      nil)
    (vui-vstack
     (vui-hstack
      (vui-text title :face 'bluesky-heading)
      (vui-button "Refresh"
        :on-click (lambda ()
                    (vui-set-state :refresh-requested (current-time)))))
     (when error
       (vui-text (bluesky--error-message error) :face 'bluesky-error))
     (when loading
       (vui-text "Loading..." :face 'bluesky-muted))
     (if items
         (vui-list (seq-filter (lambda (item) (plist-get item :render)) items)
                   (lambda (item)
                     (bluesky-ui-post host
                                      (plist-get item :post)
                                      (plist-get item :id)
                                      (plist-get item :depth)))
                   (lambda (item) (plist-get item :id))
                   :spacing 1)
       (unless loading
         (vui-text "No search results loaded." :face 'bluesky-muted)))
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
         (vui-list (seq-filter (lambda (item) (plist-get item :render)) items)
                   (lambda (item)
                     (bluesky-ui-post host
                                      (plist-get item :post)
                                      (plist-get item :id)
                                      (plist-get item :depth)))
                   (lambda (item) (plist-get item :id))
                   :spacing 1)
       (unless loading
         (vui-text "No thread posts loaded." :face 'bluesky-muted))))))

(defun bluesky--authenticate (callback &optional username password host)
  "Authenticate and call CALLBACK with host and session.
USERNAME, PASSWORD, and HOST mirror `bluesky'."
  (let* ((host (or host bluesky-default-host))
         (authinfo (car (auth-source-search :host host
                                            :user (or username t)
                                            :require '(:user :secret)
                                            :max 1)))
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
       (funcall callback host session))
     (lambda (err)
       (message "Unable to connect to Bluesky: %s"
                (bluesky--error-message err))
       (futur-failed err)))))

(defun bluesky--with-session (callback &optional username password host)
  "Call CALLBACK with a usable host and session."
  (if (and (not username)
           (not password)
           (bound-and-true-p bluesky-feed-session))
      (funcall callback
               (or host
                   (and (boundp 'bluesky-host) bluesky-host)
                   bluesky-default-host)
               bluesky-feed-session)
    (bluesky--authenticate callback username password host)))

(defun bluesky-author (actor &optional username password host)
  "Open ACTOR's Bluesky author timeline.
ACTOR can be a handle or DID.  USERNAME, PASSWORD, and HOST mirror `bluesky'."
  (interactive (list (bluesky--read-actor)))
  (let ((actor (bluesky--normalize-actor actor)))
    (bluesky--with-session
     (lambda (host session)
       (bluesky--open-author-timeline actor host session))
     username
     password
     host)))

(defun bluesky-search (query &optional username password host)
  "Open a Bluesky search timeline for QUERY.
USERNAME, PASSWORD, and HOST mirror `bluesky'."
  (interactive (list (bluesky--read-search-query)))
  (let ((query (bluesky--normalize-search-query query)))
    (bluesky--with-session
     (lambda (host session)
       (bluesky--open-search-timeline query host session))
     username
     password
     host)))

(defun bluesky-tag (tag &optional username password host)
  "Open a Bluesky tag timeline for TAG.
TAG may include a leading hash.  USERNAME, PASSWORD, and HOST mirror `bluesky'."
  (interactive (list (bluesky--read-tag)))
  (let ((tag (bluesky--normalize-tag tag)))
    (bluesky--with-session
     (lambda (host session)
       (bluesky--open-tag-timeline tag host session))
     username
     password
     host)))

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
  (bluesky--authenticate
   (lambda (host session)
     (bluesky--mount-feed-buffer
      bluesky-timeline-buffer-name
      (vui-component 'bluesky-timeline
        :host host
        :handle (plist-get session :handle))
      host
      session))
   username
   password
   host))

(provide 'bluesky)

;;; bluesky.el ends here
