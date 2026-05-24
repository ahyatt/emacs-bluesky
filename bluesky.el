;;; bluesky.el --- A Bluesky client -*- lexical-binding: t; -*-

;; Copyright (c) 2024, 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Assisted-by: ChatGPT:chatgpt-5.5
;; Homepage: https://github.com/ahyatt/emacs-bluesky
;; Package-Requires: ((emacs "30.1") (plz "0.9.0") (futur "1.7") (vui "1.0.0"))
;; Keywords: outlines, hypermedia
;; Version: 0.1.0
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

;;; Code:

(require 'bluesky-conn)
(require 'bluesky-model)
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

(defconst bluesky-feed-timeline-buffer-name-format "*Bluesky Feed: %s*"
  "Format string for Bluesky custom feed timeline buffer names.")

(defconst bluesky-notifications-buffer-name "*Bluesky Notifications*"
  "The name of the Bluesky notifications buffer.")

(defconst bluesky-notifications-buffer-name-format "*Bluesky Notifications: %s*"
  "Format string for filtered Bluesky notifications buffer names.")

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
  (add-hook 'post-command-hook #'bluesky--sync-selection-from-point nil t)
  (add-hook 'bluesky-ui-after-rerender-hook
            #'bluesky--highlight-current-after-ui-rerender
            nil t)
  (visual-line-mode 1))

(defvar-local bluesky-host bluesky-default-host
  "Host used in a particular feed.")

(defvar-local bluesky-feed-session nil
  "The Bluesky feed session associated with the buffer's feed.")

(defvar-local bluesky-feed-root nil
  "The VUI root instance for the Bluesky feed.")

(defvar-local bluesky-current-post-overlay nil
  "Overlay or overlays highlighting the current Bluesky post.")

(defvar-local bluesky--syncing-selection-from-point nil
  "Non-nil while point movement is updating Bluesky selection state.")

(defvar-local bluesky--selection-from-point-preserve-next-highlight nil
  "Non-nil when the next selection highlight should preserve point.")

(defvar bluesky-known-authors nil
  "Hash table of author identifiers seen in Bluesky app-view records.
Keys are actor identifiers suitable for `app.bsky.feed.getAuthorFeed', normally
handles and falling back to DIDs.  Values are the most recent author app-view
objects seen for those identifiers.")

(defun bluesky--known-authors-table ()
  "Return the known authors table, creating it if needed."
  (unless (hash-table-p bluesky-known-authors)
    (setq bluesky-known-authors (make-hash-table :test #'equal)))
  bluesky-known-authors)

(defun bluesky--author-actor (author)
  "Return the best AT actor identifier for AUTHOR."
  (when author
    (or (plist-get author :handle)
        (plist-get author :did))))

(defun bluesky--remember-author (author)
  "Remember AUTHOR for later completion."
  (when-let* ((actor (bluesky--author-actor author)))
    (puthash actor author (bluesky--known-authors-table))))

(defun bluesky--remember-post-authors (posts)
  "Remember authors from POSTS and their embedded quote posts."
  (dolist (item (bluesky-model-flatten-posts posts))
    (bluesky--remember-author (plist-get (plist-get item :post) :author))))

(defun bluesky--known-author-actors ()
  "Return known author identifiers sorted for completion."
  (let (actors)
    (when (hash-table-p bluesky-known-authors)
      (maphash (lambda (actor _author)
                 (push actor actors))
               bluesky-known-authors))
    (sort actors #'string-lessp)))

(defun bluesky--known-author-label (actor author)
  "Return a completion label for ACTOR using AUTHOR."
  (let ((name (string-trim (or (plist-get author :displayName) ""))))
    (if (string-empty-p name)
        actor
      (format "%s (%s)" name actor))))

(defun bluesky--known-author-choices ()
  "Return completion choices for known authors as (LABEL . ACTOR)."
  (let (choices)
    (when (hash-table-p bluesky-known-authors)
      (maphash
       (lambda (actor author)
         (push (cons (bluesky--known-author-label actor author) actor)
               choices))
       bluesky-known-authors))
    (sort choices (lambda (a b) (string-lessp (car a) (car b))))))

(defun bluesky--read-known-author (prompt default)
  "Read an author using PROMPT, DEFAULT, and known author completion."
  (let* ((choices (bluesky--known-author-choices))
         (input (completing-read
                 (if default
                     (format "%s (default %s): " prompt default)
                   (format "%s: " prompt))
                 choices
                 nil
                 nil
                 nil
                 nil
                 default)))
    (bluesky--normalize-actor (or (cdr (assoc input choices))
                                  input))))

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

(defun bluesky--selected-notification ()
  "Return the selected notification in the current buffer."
  (plist-get (bluesky--selected-item) :notification))

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
  "Return a readable `buffer-name' snippet for POST."
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
  (bluesky--author-actor (plist-get post :author)))

(defun bluesky--selected-author-actor ()
  "Return the selected post author's AT identifier, if available."
  (when-let* ((post (and (bound-and-true-p bluesky-feed-root)
                         (bluesky--selected-post))))
    (bluesky--post-author-actor post)))

(defun bluesky--read-actor (&optional prompt)
  "Read an AT actor identifier with PROMPT."
  (let* ((default (bluesky--selected-author-actor))
         (prompt (or prompt "Actor")))
    (bluesky--read-known-author prompt default)))

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

(defun bluesky--feed-timeline-buffer-name (title)
  "Return a custom feed timeline buffer name for TITLE."
  (format bluesky-feed-timeline-buffer-name-format
          (or (bluesky--clean-buffer-name-snippet title) "feed")))

(defun bluesky--notifications-buffer-name (title)
  "Return a notifications buffer name for TITLE."
  (if title
      (format bluesky-notifications-buffer-name-format title)
    bluesky-notifications-buffer-name))

(defun bluesky--feed-generator-title (generator)
  "Return a display title for feed GENERATOR."
  (if (stringp generator)
      generator
    (or (plist-get generator :displayName)
        (plist-get generator :uri)
        "Custom feed")))

(defun bluesky--feed-generator-uri (generator)
  "Return the AT URI for feed GENERATOR."
  (or (and (stringp generator) generator)
      (plist-get generator :uri)))

(defun bluesky--normalize-feed-uri (feed)
  "Return FEED as a non-empty feed generator AT URI."
  (let ((feed (string-trim (or feed ""))))
    (when (string-empty-p feed)
      (user-error "Feed URI is required"))
    (unless (string-prefix-p "at://" feed)
      (user-error "Feed must be an at:// URI"))
    feed))

(defun bluesky--feed-generator-label (generator)
  "Return a `completing-read' label for feed GENERATOR."
  (let* ((creator (plist-get generator :creator))
         (handle (plist-get creator :handle))
         (likes (plist-get generator :likeCount))
         (description (bluesky--clean-buffer-name-snippet
                       (plist-get generator :description))))
    (string-join
     (delq nil
           (list (bluesky--feed-generator-title generator)
                 (when handle (format "@%s" handle))
                 (when likes (format "%s likes" likes))
                 description))
     " - ")))

(defun bluesky--feed-generator-choices (generators)
  "Return a `completing-read' alist for GENERATORS."
  (let ((seen (make-hash-table :test 'equal)))
    (mapcar
     (lambda (generator)
       (let* ((label (bluesky--feed-generator-label generator))
              (count (1+ (or (gethash label seen) 0))))
         (puthash label count seen)
         (cons (if (= count 1)
                   label
                 (format "%s [%s]" label (plist-get generator :uri)))
               generator)))
     generators)))

(defun bluesky--select-feed-generator (generators)
  "Prompt for one feed generator from GENERATORS."
  (let ((choices (bluesky--feed-generator-choices (append generators nil))))
    (unless choices
      (user-error "No feed generators found"))
    (cdr (assoc (completing-read "Feed: " choices nil t) choices))))

(defun bluesky--feed-discovery-future (host handle input)
  "Return a future for feed discovery INPUT using HANDLE at HOST.
INPUT may be an actor identifier, a search query, or an empty string for popular
feed generators."
  (let ((input (string-trim (or input ""))))
    (cond
     ((string-empty-p input)
      (bluesky-conn-get-popular-feed-generators host handle nil 50))
     ((or (string-prefix-p "@" input)
          (string-prefix-p "did:" input))
      (bluesky-conn-get-actor-feeds
       host handle (bluesky--normalize-actor input) nil 50))
     (t
      (bluesky-conn-get-popular-feed-generators host handle nil 50 input)))))

(defun bluesky--read-feed-input ()
  "Read custom feed input."
  (read-string "Feed URI, search, @actor, or empty for popular: "))

(defun bluesky--set-timeline-state (key value)
  "Set timeline component state KEY to VALUE in the current buffer."
  (unless bluesky-feed-root
    (user-error "No Bluesky feed is active in this buffer"))
  (let ((vui--root-instance bluesky-feed-root)
        (vui--current-instance bluesky-feed-root))
    (vui-set-state key value)))

(defun bluesky--item-id-at-point ()
  "Return the Bluesky item id at point, if any."
  (get-text-property (point) 'bluesky-item-id))

(defun bluesky--item-id-near-point (direction)
  "Return the closest Bluesky item id near point in DIRECTION.
DIRECTION should be positive for the next item and negative for the previous
item.  If point is already on an item, return that item."
  (or (bluesky--item-id-at-point)
      (let ((pos (point))
            (limit (if (> direction 0) (point-max) (point-min)))
            item-id)
        (while (and (not item-id)
                    (if (> direction 0) (< pos limit) (> pos limit)))
          (setq pos (if (> direction 0)
                        (next-single-property-change
                         pos 'bluesky-item-id nil limit)
                      (previous-single-property-change
                       pos 'bluesky-item-id nil limit)))
          (when pos
            (setq item-id
                  (if (> direction 0)
                      (get-text-property pos 'bluesky-item-id)
                    (get-text-property (max (point-min) (1- pos))
                                       'bluesky-item-id)))))
        item-id)))

(defun bluesky--sync-selection-from-point ()
  "Update selected Bluesky item from point without moving point."
  (when-let* ((item-id (and (not bluesky--syncing-selection-from-point)
                            (not (memq this-command
                                       '(bluesky-feed-next-post
                                         bluesky-feed-previous-post)))
                            (bluesky--item-id-at-point))))
    (unless (equal item-id (bluesky--timeline-state :selected-id))
      (let ((bluesky--syncing-selection-from-point t))
        (setq bluesky--selection-from-point-preserve-next-highlight t)
        (bluesky--set-timeline-state :selected-id item-id)
        (bluesky--highlight-selected item-id t)))))

(defun bluesky--item-bounds (item-id)
  "Return buffer bounds for each rendered range of navigable ITEM-ID."
  (let ((pos (point-min))
        bounds)
    (while (< pos (point-max))
      (let* ((next (next-single-property-change
                    pos 'bluesky-item-id nil (point-max)))
             (value (get-text-property pos 'bluesky-item-id)))
        (when (equal value item-id)
          (push (cons pos next) bounds))
        (setq pos (max (1+ pos) next))))
    (let (coalesced)
      (dolist (bounds (nreverse bounds) (nreverse coalesced))
        (if-let* ((previous (car coalesced))
                  (_ (<= (car bounds) (cdr previous))))
            (setcdr previous (max (cdr previous) (cdr bounds)))
          (push bounds coalesced))))))

(defun bluesky--highlight-selected (item-id &optional preserve-point)
  "Highlight ITEM-ID in the current buffer.
PRESERVE-POINT non-nil means do not move point to ITEM-ID."
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
    (unless preserve-point
      (goto-char (caar bounds-list)))))

(defun bluesky--schedule-highlight (item-id &optional preserve-point)
  "Highlight ITEM-ID after the current render cycle settles.
PRESERVE-POINT non-nil means do not move point to ITEM-ID."
  (let ((buffer (current-buffer)))
    (run-with-timer
     0.05 nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (bluesky--highlight-selected item-id preserve-point)))))))

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

(defun bluesky--highlight-current-after-ui-rerender ()
  "Reapply selection highlight after a UI-driven rerender."
  (when (and (bound-and-true-p bluesky-feed-root)
             (buffer-live-p (current-buffer)))
    (bluesky--highlight-selected
     (plist-get (vui-instance-state bluesky-feed-root) :selected-id)
     t)))

(defun bluesky--move-selection (delta)
  "Move current timeline selection by DELTA."
  (let* ((items (bluesky--timeline-state :items))
         (ids (mapcar (lambda (item) (plist-get item :id)) items))
         (point-id (bluesky--item-id-near-point delta))
         (selected-id (or point-id (bluesky--timeline-state :selected-id)))
         (index (cl-position selected-id ids :test #'equal))
         (next-index
          (max 0
               (min (1- (length ids))
                    (if index
                        (if (bluesky--item-id-at-point)
                            (+ index delta)
                          index)
                      0)))))
    (unless ids
      (user-error "No posts loaded"))
    (let ((next-id (nth next-index ids)))
      (setq bluesky--selection-from-point-preserve-next-highlight nil)
      (bluesky--set-timeline-state :selected-id next-id)
      (bluesky--highlight-selected next-id)
      (bluesky--schedule-highlight next-id))))

(defun bluesky-feed-refresh ()
  "Refresh the Bluesky feed."
  (interactive nil bluesky-mode)
  (if bluesky-feed-root
      (let ((vui--root-instance bluesky-feed-root)
            (vui--current-instance bluesky-feed-root))
        (vui-set-state :refresh-requested (current-time)))
    (user-error "No Bluesky feed is active in this buffer")))

(defun bluesky-feed-extend ()
  "Load the Bluesky feed."
  (interactive nil bluesky-mode)
  (if bluesky-feed-root
      (let ((vui--root-instance bluesky-feed-root)
            (vui--current-instance bluesky-feed-root))
        (vui-set-state :extend-requested (current-time)))
    (user-error "No Bluesky feed is active in this buffer")))

(defun bluesky-feed-next-post ()
  "Move to the next post in the timeline."
  (interactive nil bluesky-mode)
  (bluesky--move-selection 1))

(defun bluesky-feed-previous-post ()
  "Move to the previous post in the timeline."
  (interactive nil bluesky-mode)
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

(defun bluesky--notification-open-targets (notification)
  "Return openable targets from NOTIFICATION."
  (let* ((host bluesky-host)
         (session bluesky-feed-session)
         (author (plist-get notification :author))
         (actor (or (plist-get author :handle)
                    (plist-get author :did)))
         (subject (plist-get notification :reasonSubject)))
    (delq nil
          (list
           (when actor
             (cons (format "Author timeline: @%s"
                           (or (plist-get author :handle) actor))
                   (lambda ()
                     (bluesky--open-author-timeline actor host session))))
           (when subject
             (cons (format "Thread: %s" subject)
                   (lambda ()
                     (bluesky--open-thread-uri subject host session))))))))

(defun bluesky-open-current ()
  "Open a link, media URL, or author timeline from the selected post."
  (interactive nil bluesky-mode)
  (let* ((post (bluesky--selected-post))
         (notification (and (not post) (bluesky--selected-notification)))
         (targets (if post
                      (bluesky--post-open-targets post)
                    (bluesky--notification-open-targets notification))))
    (unless targets
      (user-error "Selected item has no links or media to open"))
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
  (interactive nil bluesky-mode)
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
  (interactive nil bluesky-mode)
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
  (interactive nil bluesky-mode)
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

(defun bluesky-compose-post (&optional username password host)
  "Open a buffer to compose a new Bluesky post.
USERNAME, PASSWORD, and HOST mirror `bluesky'."
  (interactive)
  (let ((source-buffer (and (derived-mode-p 'bluesky-mode)
                            (current-buffer))))
    (bluesky--with-session
     (lambda (host session)
       (bluesky-post-compose
        :host host
        :session session
        :source-buffer source-buffer))
     username
     password
     host)))

(defun bluesky-reply ()
  "Open a buffer to reply to the selected post."
  (interactive nil bluesky-mode)
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

(defun bluesky--open-custom-feed-timeline (feed host session &optional title)
  "Open custom FEED on HOST using SESSION.
FEED can be a feed generator view or an AT URI.  TITLE overrides the rendered
heading and buffer label."
  (let* ((feed-uri (bluesky--normalize-feed-uri
                    (bluesky--feed-generator-uri feed)))
         (title (or title (bluesky--feed-generator-title feed))))
    (bluesky--mount-feed-buffer
     (bluesky--feed-timeline-buffer-name title)
     (vui-component 'bluesky-custom-feed-timeline
       :host host
       :handle (plist-get session :handle)
       :feed feed-uri
       :title title)
     host
     session
     t)))

(defun bluesky--open-notifications (host session &optional reasons title)
  "Open notifications on HOST using SESSION.
REASONS is a vector of notification reason strings.  TITLE customizes the
rendered heading and buffer label."
  (let ((handle (plist-get session :handle)))
    (bluesky--mount-feed-buffer
     (bluesky--notifications-buffer-name title)
     (vui-component 'bluesky-notifications-view
       :host host
       :handle handle
       :reasons reasons
       :title (or title "Notifications"))
     host
     session
     t)))

(defun bluesky--discover-and-open-feed (input host session)
  "Discover a feed from INPUT on HOST using SESSION, then open it."
  (futur-bind
   (bluesky--feed-discovery-future host (plist-get session :handle) input)
   (lambda (response)
     (let ((generator (bluesky--select-feed-generator
                       (plist-get response :feeds))))
       (bluesky--open-custom-feed-timeline generator host session)))
   (lambda (err)
     (message "Unable to discover Bluesky feeds: %s"
              (bluesky--error-message err))
     (futur-failed err))))

(defun bluesky--open-thread-uri (uri host session &optional post)
  "Open a thread buffer for URI on HOST using SESSION.
POST, when present, is used to build a friendlier buffer name."
  (let* ((handle (plist-get session :handle))
         (buffer-name (if post
                          (bluesky--thread-buffer-name post)
                        (format "*Bluesky Thread: %s*"
                                (or (bluesky--clean-buffer-name-snippet uri)
                                    "post")))))
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

(defun bluesky-open-thread ()
  "Open a thread view for the selected post or notification subject."
  (interactive nil bluesky-mode)
  (let* ((post (bluesky--selected-post))
         (notification (and (not post) (bluesky--selected-notification)))
         (uri (or (plist-get post :uri)
                  (plist-get notification :reasonSubject)
                  (user-error "Selected item does not have a post thread"))))
    (bluesky--open-thread-uri uri bluesky-host bluesky-feed-session post)))

(defun bluesky-activate-or-open-thread ()
  "Activate the widget at point, or open the selected post thread."
  (interactive nil bluesky-mode)
  (if (widget-at)
      (widget-button-press (point))
    (bluesky-open-thread)))

(defun bluesky--feed-response-posts (response)
  "Return post views from a feed RESPONSE."
  (mapcar (lambda (entry) (plist-get entry :post))
          (append (plist-get response :feed) nil)))

(defun bluesky--post-record-notification-p (notification)
  "Return non-nil when NOTIFICATION contains an app.bsky.feed.post record."
  (equal (plist-get (plist-get notification :record) :$type)
         "app.bsky.feed.post"))

(defun bluesky--notification-post (notification)
  "Return NOTIFICATION as a post-like app-view object, if possible."
  (when (bluesky--post-record-notification-p notification)
    (list :uri (plist-get notification :uri)
          :cid (plist-get notification :cid)
          :author (plist-get notification :author)
          :record (plist-get notification :record)
          :labels (plist-get notification :labels))))

(defun bluesky--notification-item-id (notification)
  "Return a stable item id for NOTIFICATION."
  (string-join
   (delq nil
         (list (plist-get notification :uri)
               (plist-get notification :cid)
               (plist-get notification :reason)))
   "#"))

(defun bluesky--notification-items (notifications)
  "Return navigable items for NOTIFICATIONS."
  (mapcar (lambda (notification)
            (let* ((id (bluesky--notification-item-id notification))
                   (post (bluesky--notification-post notification)))
              (list :id id
                    :notification notification
                    :post post
                    :render t)))
          notifications))

(defun bluesky--render-paged-feed
    (host title empty-message fetch-key fetch-page page-posts posts cursor
          loading error items selected-id refresh-requested extend-requested
          &optional preserve-next-highlight)
  "Render a paged Bluesky feed.
HOST is the Bluesky host for the feed.
TITLE is the heading text for the feed buffer.
EMPTY-MESSAGE is shown when there are no posts.
FETCH-KEY identifies the feed inputs for VUI effects.  FETCH-PAGE is called with
a cursor, or nil for the first page.  PAGE-POSTS extracts post views from a
response.  POSTS and CURSOR are the currently loaded posts and next-page
cursor.  LOADING, ERROR, ITEMS, SELECTED-ID, REFRESH-REQUESTED, and
EXTEND-REQUESTED are VUI state values.  PRESERVE-NEXT-HIGHLIGHT non-nil means
the next selected-post highlight should not move point."
  (let ((current-items (bluesky-model-flatten-posts posts 0 t)))
    (vui-use-effect (posts)
      (bluesky--remember-post-authors posts)
      nil)
    (vui-use-effect (posts selected-id)
      (let ((ids (mapcar (lambda (item) (plist-get item :id)) current-items)))
        (vui-batch
         (vui-set-state :items current-items)
         (when (and ids (not (member selected-id ids)))
           (vui-set-state :selected-id (car ids)))))
      nil))
  (vui-use-effect (selected-id items)
    (bluesky--schedule-highlight selected-id preserve-next-highlight)
    (when preserve-next-highlight
      (setq bluesky--selection-from-point-preserve-next-highlight nil))
    nil)
  (vui-use-effect (fetch-key refresh-requested)
    (vui-batch
     (vui-set-state :loading t)
     (vui-set-state :error nil))
    (bluesky--future-set-state
     (funcall fetch-page nil)
     :posts
     (lambda (response)
       (vui-batch
        (vui-set-state :cursor (plist-get response :cursor)))
       (funcall page-posts response))
     :error)
    nil)
  (vui-use-effect (extend-requested)
    (when (and extend-requested cursor (not loading))
      (vui-batch
       (vui-set-state :loading t)
       (vui-set-state :error nil))
      (bluesky--future-set-state
       (funcall fetch-page cursor)
       :posts
       (lambda (response)
         (vui-batch
          (vui-set-state :cursor (plist-get response :cursor)))
         (append posts (funcall page-posts response)))
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
       (vui-text empty-message :face 'bluesky-muted)))
   (when cursor
     (vui-button "Load more"
       :on-click (lambda ()
                   (vui-set-state :extend-requested (current-time)))))))

(defun bluesky--render-paged-notifications
    (host title empty-message fetch-key fetch-page notifications cursor
          loading error items selected-id refresh-requested extend-requested)
  "Render paged Bluesky NOTIFICATIONS.
HOST, TITLE, EMPTY-MESSAGE, FETCH-KEY, FETCH-PAGE, and the remaining arguments
mirror `bluesky--render-paged-feed'."
  (let ((current-items (bluesky--notification-items notifications)))
    (vui-use-effect (notifications)
      (dolist (notification notifications)
        (bluesky--remember-author (plist-get notification :author)))
      nil)
    (vui-use-effect (notifications selected-id)
      (let ((ids (mapcar (lambda (item) (plist-get item :id)) current-items)))
        (vui-batch
         (vui-set-state :items current-items)
         (when (and ids (not (member selected-id ids)))
           (vui-set-state :selected-id (car ids)))))
      nil))
  (vui-use-effect (selected-id items)
    (bluesky--schedule-highlight
     selected-id
     bluesky--selection-from-point-preserve-next-highlight)
    (setq bluesky--selection-from-point-preserve-next-highlight nil)
    nil)
  (vui-use-effect (fetch-key refresh-requested)
    (vui-batch
     (vui-set-state :loading t)
     (vui-set-state :error nil))
    (bluesky--future-set-state
     (funcall fetch-page nil)
     :notifications
     (lambda (response)
       (vui-batch
        (vui-set-state :cursor (plist-get response :cursor)))
       (append (plist-get response :notifications) nil))
     :error)
    nil)
  (vui-use-effect (extend-requested)
    (when (and extend-requested cursor (not loading))
      (vui-batch
       (vui-set-state :loading t)
       (vui-set-state :error nil))
      (bluesky--future-set-state
       (funcall fetch-page cursor)
       :notifications
       (lambda (response)
         (vui-batch
          (vui-set-state :cursor (plist-get response :cursor)))
         (append notifications
                 (append (plist-get response :notifications) nil)))
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
       (vui-list items
                 (lambda (item)
                   (let ((notification (plist-get item :notification)))
                     (vui-vstack
                      (bluesky-ui-notification host
                                               notification
                                               (plist-get item :id))
                      (when-let* ((post (plist-get item :post)))
                        (bluesky-ui-post host
                                         post
                                         (plist-get item :id))))))
                 (lambda (item) (plist-get item :id))
                 :spacing 1)
     (unless loading
       (vui-text empty-message :face 'bluesky-muted)))
   (when cursor
     (vui-button "Load more"
       :on-click (lambda ()
                   (vui-set-state :extend-requested (current-time)))))))

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
  (bluesky--render-paged-feed
   host
   (format "Timeline for %s" handle)
   "No posts loaded."
   (list 'timeline host handle)
   (lambda (cursor)
     (bluesky-conn-get-timeline host handle cursor 50))
   #'bluesky--feed-response-posts
   posts cursor loading error items selected-id refresh-requested
   extend-requested
   bluesky--selection-from-point-preserve-next-highlight))

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
  (bluesky--render-paged-feed
   host
   (format "Author timeline for %s" actor)
   "No author posts loaded."
   (list 'author host handle actor)
   (lambda (cursor)
     (bluesky-conn-get-author-feed host handle actor cursor 50))
   #'bluesky--feed-response-posts
   posts cursor loading error items selected-id refresh-requested
   extend-requested))

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
  (bluesky--render-paged-feed
   host
   title
   "No search results loaded."
   (list 'search host handle query tags)
   (lambda (cursor)
     (bluesky-conn-search-posts host handle query cursor 50 "latest" tags))
   (lambda (results)
     (append (plist-get results :posts) nil))
   posts cursor loading error items selected-id refresh-requested
   extend-requested))

(vui-defcomponent bluesky-custom-feed-timeline (host handle feed title)
  "Render custom Bluesky FEED."
  :state ((posts nil)
          (cursor nil)
          (loading nil)
          (error nil)
          (items nil)
          (selected-id nil)
          (refresh-requested nil)
          (extend-requested nil))
  :render
  (bluesky--render-paged-feed
   host
   title
   "No feed posts loaded."
   (list 'custom-feed host handle feed)
   (lambda (cursor)
     (bluesky-conn-get-feed host handle feed cursor 50))
   #'bluesky--feed-response-posts
   posts cursor loading error items selected-id refresh-requested
   extend-requested))

(vui-defcomponent bluesky-notifications-view (host handle reasons title)
  "Render Bluesky notifications for HANDLE."
  :state ((notifications nil)
          (cursor nil)
          (loading nil)
          (error nil)
          (items nil)
          (selected-id nil)
          (refresh-requested nil)
          (extend-requested nil))
  :render
  (bluesky--render-paged-notifications
   host
   (format "%s for %s" title handle)
   "No notifications loaded."
   (list 'notifications host handle reasons)
   (lambda (cursor)
     (bluesky-conn-list-notifications host handle cursor 50 reasons))
   notifications cursor loading error items selected-id refresh-requested
   extend-requested))

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
    (let ((current-items (and thread (bluesky-model-thread-items thread))))
      (vui-use-effect (thread)
        (dolist (item current-items)
          (bluesky--remember-author (plist-get (plist-get item :post) :author)))
        nil)
      (vui-use-effect (thread selected-id)
        (let ((ids (mapcar (lambda (item) (plist-get item :id)) current-items)))
          (vui-batch
           (vui-set-state :items current-items)
           (when (and ids (not (member selected-id ids)))
             (vui-set-state :selected-id (car ids)))))
        nil))
    (vui-use-effect (selected-id items)
      (bluesky--schedule-highlight
       selected-id
       bluesky--selection-from-point-preserve-next-highlight)
      (setq bluesky--selection-from-point-preserve-next-highlight nil)
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
       (bluesky--remember-author session)
       (funcall callback host session))
     (lambda (err)
       (message "Unable to connect to Bluesky: %s"
                (bluesky--error-message err))
       (futur-failed err)))))

(defun bluesky--with-session (callback &optional username password host)
  "Call CALLBACK with a usable host and session.
USERNAME, PASSWORD, and HOST are optional login details."
  (if (and (not username)
           (not password)
           (bound-and-true-p bluesky-feed-session))
      (progn
        (bluesky--remember-author bluesky-feed-session)
        (funcall callback
                 (or host
                     (and (boundp 'bluesky-host) bluesky-host)
                     bluesky-default-host)
                 bluesky-feed-session))
    (bluesky--authenticate callback username password host)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun bluesky-feed (feed &optional username password host)
  "Open a Bluesky custom feed timeline.
FEED can be an at:// feed generator URI, a search query, an @actor handle, or an
empty string to discover popular feeds.  USERNAME, PASSWORD, and HOST mirror
`bluesky'."
  (interactive (list (bluesky--read-feed-input)))
  (let ((feed (string-trim (or feed ""))))
    (bluesky--with-session
     (lambda (host session)
       (if (string-prefix-p "at://" feed)
           (bluesky--open-custom-feed-timeline feed host session)
         (bluesky--discover-and-open-feed feed host session)))
     username
     password
     host)))

;;;###autoload
(defun bluesky-notifications (&optional username password host)
  "Open notifications for the authenticated account.
USERNAME, PASSWORD, and HOST mirror `bluesky'."
  (interactive)
  (bluesky--with-session
   (lambda (host session)
     (bluesky--open-notifications host session))
   username
   password
   host))

;;;###autoload
(defun bluesky-likes (&optional username password host)
  "Open like notifications for the authenticated account.
USERNAME, PASSWORD, and HOST mirror `bluesky'."
  (interactive)
  (bluesky--with-session
   (lambda (host session)
     (bluesky--open-notifications
      host session (vector "like" "like-via-repost") "Likes"))
   username
   password
   host))

;;;###autoload
(defun bluesky-replies (&optional username password host)
  "Open reply notifications for the authenticated account.
USERNAME, PASSWORD, and HOST mirror `bluesky'."
  (interactive)
  (bluesky--with-session
   (lambda (host session)
     (bluesky--open-notifications host session (vector "reply") "Replies"))
   username
   password
   host))

;;;###autoload
(defun bluesky (&optional username password host)
  "Connect to a Bluesky server and render the user's feed.

USERNAME is the username to connect with.  It should not include the @
prefix.  This can be nil, and if so, the user will be found via
`auth-source-search'.  Otherwise, the user will be prompted for the
username.

PASSWORD is the password to connect with.  This can be nil, and if so,
the password will be found via `auth-source-search'.  Otherwise, the
user will be prompted for the password.

HOST is the Bluesky server to connect to."
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
