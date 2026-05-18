;;; bluesky-ui.el --- Bluesky UI functions -*- lexical-binding: t; -*-

;; Copyright (c) 2024, 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/emacs-bluesky
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
;; This file renders atproto objects.  Rendering is done by inserting directly
;; into the buffer.  Overlays are used to both store the underlying objects when
;; needed, but also to stylize text.

(require 'bluesky-conn)
(require 'plz)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'vui)

;;; Code:

(defgroup bluesky nil
  "Bluesky client for Emacs."
  :group 'applications)

(defcustom bluesky-image-max-width 250
  "The maximum width of images in the Bluesky UI."
  :type 'integer)

(defcustom bluesky-ui-render-images t
  "Whether to render remote images in Bluesky buffers.
Images are loaded asynchronously through `bluesky-ui--image-queue'."
  :type 'boolean)

(defcustom bluesky-ui-image-loads-per-second 2
  "Maximum number of image downloads to start per second."
  :type 'number)

(defface bluesky-author-name
  '((t :inherit (bold font-lock-keyword-face)))
  "Face for author names in Bluesky.")

(defface bluesky-author-handle
  '((t :inherit shadow))
  "Face for author handles in Bluesky.")

(defface bluesky-author-attribute
  '((t :inherit font-lock-comment-face))
  "Face for author attributes in Bluesky.")

(defface bluesky-label
  '((t :inherit warning))
  "Face for labels and content warnings in Bluesky.")

(defface bluesky-time
  '((t :inherit shadow))
  "Face for time in Bluesky UI")

(defface bluesky-post-separator
  '((t :inherit shadow))
  "Face for separators between Bluesky posts.")

(defface bluesky-post-stats
  '((t :inherit shadow))
  "Face for Bluesky post engagement counts.")

(defface bluesky-quote-label
  '((t :inherit (bold shadow)))
  "Face for quoted-post labels.")

(defface bluesky-quote-bar
  '((t :inherit shadow))
  "Face for the vertical quote indentation marker.")

(defface bluesky-mention
  '((t :inherit link))
  "Face for mentions in Bluesky.")

(defface bluesky-hashtag
  '((t :inherit link))
  "Face for hashtags in Bluesky.")

(defface bluesky-link
  '((t :inherit link))
  "Face for links in Bluesky.")

(defface bluesky-external-title
  '((t :inherit bold))
  "Face for external website titles.")

(defface bluesky-external-description
  '((t :inherit default))
  "Face for external website descriptions.")

(defface bluesky-media
  '((t :inherit font-lock-doc-face))
  "Face for media metadata.")

(defface bluesky-image-placeholder
  '((t :inherit shadow))
  "Face for image placeholders.")

(defvar bluesky-ui--item-id nil
  "Current navigable Bluesky item id while rendering.")

(defvar bluesky-ui--line-prefix nil
  "Display prefix applied to text nodes in the current render scope.")

(defvar bluesky-ui--quoted-post nil
  "Non-nil while rendering a quoted post.")

(defvar bluesky-ui--image-cache (make-hash-table :test 'equal)
  "Image cache keyed by URL.
Values are plists with :status, :image, :url, :buffers, and :error.")

(defvar bluesky-ui--image-queue nil
  "Queue of image keys waiting to be loaded.")

(defvar bluesky-ui--image-timer nil
  "Timer used to drain `bluesky-ui--image-queue'.")

(defun bluesky-ui--text (content &rest props)
  "Return a VUI text node for CONTENT with PROPS.
When `bluesky-ui--item-id' is non-nil, mark the text as belonging to
that navigable item."
  (apply #'vui-text content
         (append props
                 (when bluesky-ui--line-prefix
                   (list 'line-prefix bluesky-ui--line-prefix
                         'wrap-prefix bluesky-ui--line-prefix))
                 (when bluesky-ui--item-id
                   (list 'bluesky-item-id bluesky-ui--item-id)))))

(defun bluesky-ui-relative-time (timestr)
  "Transform TIMESTR to a relative time string for showing users.
TIMESTR is a string such as 2024-11-29T22:31:30.465Z."
  (let* ((time (date-to-time timestr))
         (now (current-time))
         (diff (time-subtract now time))
         (diff-seconds (time-to-seconds diff)))
    (cond
     ((< diff-seconds 60) "just now")
     ((< diff-seconds 3600) (format "%d minutes ago" (/ diff-seconds 60)))
     ((< diff-seconds 86400) (format "%d hours ago" (/ diff-seconds 3600)))
     ((< diff-seconds 604800) (format "%d days ago" (/ diff-seconds 86400)))
     (t (format-time-string "%Y-%m-%d" time)))))

(defun bluesky-ui--nodes (&rest items)
  "Return a flat list of non-nil VUI nodes from ITEMS."
  (let (result)
    (dolist (item items (nreverse result))
      (cond
       ((null item) nil)
       ((and (listp item) (not (vui-vnode-p item)))
        (dolist (node item)
          (when node
            (push node result))))
       (t
        (push item result))))))

(defun bluesky-ui--fragment (&rest children)
  "Return a VUI fragment with nil CHILDREN removed."
  (apply #'vui-fragment (apply #'bluesky-ui--nodes children)))

(defun bluesky-ui--image-interval ()
  "Return the image queue polling interval."
  (/ 1.0 (max 0.1 bluesky-ui-image-loads-per-second)))

(defun bluesky-ui--image-entry-put (key &rest props)
  "Update image cache entry KEY with PROPS."
  (let ((entry (copy-sequence (or (gethash key bluesky-ui--image-cache)
                                  (list :status 'queued :url key)))))
    (while props
      (setq entry (plist-put entry (pop props) (pop props))))
    (puthash key entry bluesky-ui--image-cache)
    entry))

(defun bluesky-ui--image-rerender-buffers (buffers)
  "Rerender live VUI BUFFERS that requested an image."
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (bound-and-true-p vui--root-instance)
          (vui-rerender vui--root-instance)
          (cond
           ((fboundp 'bluesky--highlight-current-now)
            (bluesky--highlight-current-now))
           ((fboundp 'bluesky--highlight-current)
            (bluesky--highlight-current))))))))

(defun bluesky-ui--image-queue-running-p ()
  "Return non-nil if the image queue timer is active."
  (and bluesky-ui--image-timer
       (memq bluesky-ui--image-timer timer-list)))

(defun bluesky-ui--ensure-image-timer ()
  "Ensure the image queue timer is running."
  (unless (bluesky-ui--image-queue-running-p)
    (setq bluesky-ui--image-timer
          (run-with-timer 0
                          (bluesky-ui--image-interval)
                          #'bluesky-ui--image-queue-tick))))

(defun bluesky-ui--image-queue-tick ()
  "Start the next queued image download, respecting the timer rate."
  (let (key entry)
    (while (and bluesky-ui--image-queue (not key))
      (let* ((candidate (pop bluesky-ui--image-queue))
             (candidate-entry (gethash candidate bluesky-ui--image-cache)))
        (when (eq (plist-get candidate-entry :status) 'queued)
          (setq key candidate
                entry candidate-entry))))
    (if (not key)
        (when (and bluesky-ui--image-timer
                   (not bluesky-ui--image-queue))
          (cancel-timer bluesky-ui--image-timer)
          (setq bluesky-ui--image-timer nil))
      (bluesky-ui--image-entry-put key :status 'loading)
      (plz 'get (plist-get entry :url)
        :as 'binary
        :then (lambda (data)
                (let* ((image (ignore-errors (create-image data nil 'data)))
                       (updated (bluesky-ui--image-entry-put
                                 key
                                 :status (if image 'ready 'error)
                                 :image image
                                 :error (unless image "Unable to create image"))))
                  (bluesky-ui--image-rerender-buffers
                   (plist-get updated :buffers))))
        :else (lambda (err)
                (let ((updated (bluesky-ui--image-entry-put
                                key
                                :status 'error
                                :error err)))
                  (bluesky-ui--image-rerender-buffers
                   (plist-get updated :buffers))))))))

(defun bluesky-ui--enqueue-image (key url)
  "Record that the current buffer wants image KEY from URL."
  (let* ((buffer (current-buffer))
         (entry (or (gethash key bluesky-ui--image-cache)
                    (bluesky-ui--image-entry-put key
                                                 :status 'queued
                                                 :url url
                                                 :buffers nil)))
         (buffers (plist-get entry :buffers)))
    (unless (memq buffer buffers)
      (setq entry (bluesky-ui--image-entry-put key :buffers (cons buffer buffers))))
    (when (eq (plist-get entry :status) 'queued)
      (unless (member key bluesky-ui--image-queue)
        (setq bluesky-ui--image-queue
              (append bluesky-ui--image-queue (list key))))
      (bluesky-ui--ensure-image-timer))
    entry))

(defun bluesky-ui--blob-url (host did cid)
  "Return the Bluesky blob URL for CID from DID on HOST."
  (format "https://%s/xrpc/com.atproto.sync.getBlob?did=%s&cid=%s"
          host
          (url-hexify-string did)
          (url-hexify-string cid)))

(defun bluesky-ui--image-node (image &rest properties)
  "Return a VUI text node displaying IMAGE with PROPERTIES."
  (when image
    (bluesky-ui--text " " 'display (append image properties))))

(defun bluesky-ui--async-image-node (key url &rest properties)
  "Return a VUI node for image KEY loaded from URL."
  (when (and bluesky-ui-render-images key url)
    (let ((entry (bluesky-ui--enqueue-image key url)))
      (pcase (plist-get entry :status)
        ('ready
         (apply #'bluesky-ui--image-node (plist-get entry :image) properties))
        ('error
         (bluesky-ui--text "[image failed]" :face 'bluesky-image-placeholder))
        (_
         (bluesky-ui--text "[image]" :face 'bluesky-image-placeholder))))))

(defun bluesky-ui--image-by-url-node (url &rest properties)
  "Return a VUI image node for URL, or nil if URL is missing."
  (when url
    (apply #'bluesky-ui--async-image-node url url properties)))

(defun bluesky-ui-author (author)
  "Return a VUI node for AUTHOR."
  (let* ((viewer (plist-get author :viewer))
         (muted (and viewer (not (eq (plist-get viewer :muted) :json-false))
                     (plist-get viewer :muted)))
         (blocked (and viewer (not (eq (plist-get viewer :blockedBy) :json-false))
                       (plist-get viewer :blockedBy)))
         (following (plist-get viewer :following))
         (attributes (append
                      (when muted '("Muted"))
                      (when blocked '("Blocked"))
                      (when following '("Following")))))
    (bluesky-ui--fragment
     (bluesky-ui--image-by-url-node (plist-get author :avatar)
                                    :width '(1 . ch) :height '(1 . ch)
                                    :margin 2)
     (bluesky-ui--text (or (plist-get author :displayName) (plist-get author :handle) "")
                       :face 'bluesky-author-name)
     (vui-space)
     (bluesky-ui--text (concat "@" (or (plist-get author :handle) ""))
                       :face 'bluesky-author-handle)
     (when attributes
       (bluesky-ui--fragment
        (vui-space)
        (bluesky-ui--text (format "[%s]" (string-join attributes ", "))
                          :face 'bluesky-author-attribute))))))

(defun bluesky-ui-byte-to-char-index (text byte-offset)
  "Convert UTF-8 BYTE-OFFSET into a zero-based character index in TEXT."
  (if (eq (length text) (string-bytes text))
      byte-offset
    (length
     (decode-coding-string
      (substring (encode-coding-string text 'utf-8) 0 byte-offset)
      'utf-8 t))))

(defun bluesky-ui--facet-face (facet)
  "Return the face for FACET."
  (when-let* ((features (plist-get facet :features))
              (feature (and (> (length features) 0) (aref features 0)))
              (type (cadr (split-string (plist-get feature :$type) "#"))))
    (pcase type
      ("mention" 'bluesky-mention)
      ("hashtag" 'bluesky-hashtag)
      ("link" 'bluesky-link))))

(defun bluesky-ui-text-with-facets (text facets)
  "Return VUI nodes for TEXT styled according to FACETS."
  (let ((pos 0)
        nodes)
    (dolist (facet (sort (append facets nil)
                         (lambda (a b)
                           (< (plist-get (plist-get a :index) :byteStart)
                              (plist-get (plist-get b :index) :byteStart)))))
      (let* ((index (plist-get facet :index))
             (start (bluesky-ui-byte-to-char-index text
                                                   (plist-get index :byteStart)))
             (end (bluesky-ui-byte-to-char-index text
                                                 (plist-get index :byteEnd))))
        (when (< pos start)
          (push (bluesky-ui--text (substring text pos start)) nodes))
        (push (bluesky-ui--text (substring text start end)
                                :face (bluesky-ui--facet-face facet)
                                'bluesky facet)
              nodes)
        (setq pos end)))
    (when (< pos (length text))
      (push (bluesky-ui--text (substring text pos)) nodes))
    (nreverse nodes)))

(defun bluesky-ui--image-url-for-reference (host ref author-did)
  "Return image URL for REF from HOST.
REF can be a URL or a blob reference.  AUTHOR-DID is the uploading DID."
  (cond
   ((null ref) nil)
   ((stringp ref) ref)
   (t (bluesky-ui--blob-url
       host author-did
       (plist-get (plist-get ref :ref) :$link)))))

(defun bluesky-ui-external (host external author-did)
  "Return a VUI node for EXTERNAL on HOST.
AUTHOR-DID is the DID of the post author."
  (let* ((thumb-url (bluesky-ui--image-url-for-reference
                     host (plist-get external :thumb) author-did))
         (thumb-node (when thumb-url
                       (bluesky-ui--async-image-node
                        thumb-url thumb-url
                        :max-width bluesky-image-max-width))))
    (bluesky-ui--fragment
     thumb-node
     (when thumb-node
       (vui-space))
     (bluesky-ui--text (or (plist-get external :title) "")
                       :face 'bluesky-external-title
                       'bluesky external)
     (vui-newline)
     (bluesky-ui--text (or (plist-get external :description) "")
                       :face 'bluesky-external-description
                       'bluesky external))))

(defun bluesky-ui--aspect-ratio-text (aspect-ratio)
  "Return a readable string for ASPECT-RATIO."
  (when aspect-ratio
    (let ((width (plist-get aspect-ratio :width))
          (height (plist-get aspect-ratio :height)))
      (when (and width height)
        (format "%sx%s" width height)))))

(defun bluesky-ui--json-truthy-p (value)
  "Return non-nil when VALUE represents true in Bluesky JSON data."
  (and value (not (eq value :json-false))))

(defun bluesky-ui--viewer-state-text (viewer)
  "Return readable action state text for VIEWER."
  (let ((states (delq nil
                      (list (when (plist-get viewer :like) "liked")
                            (when (plist-get viewer :repost) "reposted")
                            (when (bluesky-ui--json-truthy-p
                                   (plist-get viewer :bookmarked))
                              "bookmarked")))))
    (when states
      (string-join states ", "))))

(defun bluesky-ui--viewer-flags (viewer)
  "Return readable moderation/action flags from VIEWER."
  (delq nil
        (list (when (bluesky-ui--json-truthy-p
                     (plist-get viewer :threadMuted))
                "thread muted")
              (when (bluesky-ui--json-truthy-p
                     (plist-get viewer :replyDisabled))
                "replies disabled")
              (when (bluesky-ui--json-truthy-p
                     (plist-get viewer :embeddingDisabled))
                "embedding disabled"))))

(defun bluesky-ui--label-text (label)
  "Return readable text for LABEL."
  (or (plist-get label :val)
      (plist-get label :uri)
      (plist-get label :cid)
      (plist-get label :src)
      "unknown"))

(defun bluesky-ui-labels (post)
  "Return a VUI node for POST labels and moderation flags."
  (let* ((labels (seq-filter #'identity (append (plist-get post :labels) nil)))
         (label-texts (mapcar #'bluesky-ui--label-text labels))
         (flags (bluesky-ui--viewer-flags (plist-get post :viewer)))
         (all (append label-texts flags)))
    (when all
      (bluesky-ui--fragment
       (bluesky-ui--text (format "[%s]" (string-join all ", "))
                         :face 'bluesky-label)
       (vui-newline)))))

(defun bluesky-ui--count-label (count singular &optional plural)
  "Return COUNT followed by SINGULAR or PLURAL."
  (format "%d %s" count (if (= count 1) singular (or plural (concat singular "s")))))

(defun bluesky-ui--stats-text (post)
  "Return a compact, readable stats string for POST."
  (let ((stats (string-join
                (list
                 (bluesky-ui--count-label (or (plist-get post :replyCount) 0) "reply" "replies")
                 (bluesky-ui--count-label (or (plist-get post :repostCount) 0) "repost")
                 (bluesky-ui--count-label (or (plist-get post :quoteCount) 0) "quote")
                 (bluesky-ui--count-label (or (plist-get post :likeCount) 0) "like"))
                "  |  "))
        (viewer (bluesky-ui--viewer-state-text (plist-get post :viewer))))
    (if viewer
        (concat stats "  |  " viewer)
      stats)))

(defun bluesky-ui--separator (&optional depth)
  "Return a post separator for DEPTH."
  (bluesky-ui--text "----------------------------------------------------------------"
                    :face 'bluesky-post-separator))

(defun bluesky-ui-video (video)
  "Return a VUI node for VIDEO embed view."
  (let ((thumbnail (plist-get video :thumbnail))
        (presentation (plist-get video :presentation))
        (aspect (bluesky-ui--aspect-ratio-text
                 (plist-get video :aspectRatio))))
    (bluesky-ui--fragment
     (when thumbnail
       (bluesky-ui--async-image-node thumbnail thumbnail
                                     :max-width bluesky-image-max-width))
     (vui-newline)
     (bluesky-ui--text
      (string-join (delq nil (list "[video]"
                                    presentation
                                    aspect))
                   " ")
      :face 'bluesky-media
      'bluesky video))))

(defun bluesky-ui--record-view-as-post (record-view)
  "Return RECORD-VIEW as a post view plist, when possible."
  (when (and record-view
             (plist-get record-view :author)
             (plist-get record-view :value))
    (let ((post (copy-sequence record-view)))
      (plist-put post :record (plist-get record-view :value)))))

(defun bluesky-ui-embedded-record (host record-view depth)
  "Return a VUI node for embedded RECORD-VIEW on HOST."
  (let ((type (plist-get record-view :$type)))
    (cond
     ((and (equal type "app.bsky.embed.record#view")
           (plist-get record-view :record))
      (bluesky-ui-embedded-record host (plist-get record-view :record) depth))
     ((bluesky-ui--record-view-as-post record-view)
      (bluesky-ui-quoted-post host
                              (bluesky-ui--record-view-as-post record-view)
                              (or depth 0)))
     ((equal type "app.bsky.embed.record#viewBlocked")
      (bluesky-ui--text "[blocked quoted record]"
                        :face 'bluesky-author-attribute))
     ((equal type "app.bsky.embed.record#viewNotFound")
      (bluesky-ui--text "[quoted record not found]"
                        :face 'bluesky-author-attribute))
     ((equal type "app.bsky.embed.record#viewDetached")
      (bluesky-ui--text "[quoted record detached]"
                        :face 'bluesky-author-attribute))
     (record-view
      (bluesky-ui--text (format "[unsupported quoted record: %s]"
                                (or type "unknown"))
                        :face 'bluesky-author-attribute)))))

(defun bluesky-ui-quoted-post (host post depth)
  "Return a quoted POST from HOST with a visible indentation marker."
  (let ((bluesky-ui--line-prefix
         (concat (make-string (* 2 (1+ depth)) ?\s)
                 (propertize "| " 'face 'bluesky-quote-bar)))
        (bluesky-ui--quoted-post t))
    (bluesky-ui-post host post nil 0)))

(defun bluesky-ui-embed (host embed author-did &optional depth)
  "Return VUI nodes for EMBED from HOST.
AUTHOR-DID is the DID of the author of the post."
  (when embed
    (let ((type (plist-get embed :$type)))
      (bluesky-ui--fragment
       (when (or (equal type "app.bsky.embed.images#view")
                 (equal type "app.bsky.embed.images"))
         (mapcar
          (lambda (image)
            (when-let* ((image-url
                         (or (plist-get image :thumb)
                             (bluesky-ui--image-url-for-reference
                              host (plist-get image :image) author-did))))
              (bluesky-ui--async-image-node image-url image-url
                                            :max-width bluesky-image-max-width)))
          (append (plist-get embed :images) nil)))
       (when-let* ((external (plist-get embed :external)))
         (bluesky-ui-external host external author-did))
       (when (or (equal type "app.bsky.embed.video#view")
                 (equal type "app.bsky.embed.video"))
         (bluesky-ui-video embed))
       (when-let* ((media (plist-get embed :media)))
         (bluesky-ui-embed host media author-did depth))
       (when-let* ((record (plist-get embed :record)))
         (bluesky-ui-embedded-record host record depth))))))

(defun bluesky-ui-record (host record author-did &optional depth view-embed)
  "Return a VUI node for RECORD on HOST."
  (bluesky-ui--fragment
   (bluesky-ui-text-with-facets (or (plist-get record :text) "")
                                (plist-get record :facets))
   (vui-newline)
   (bluesky-ui-embed host (or view-embed (plist-get record :embed))
                     author-did depth)))

(defun bluesky-ui-post (host post &optional item-id depth)
  "Return a VUI node for POST from HOST."
  (let* ((record (plist-get post :record))
         (author (plist-get post :author))
         (author-did (plist-get author :did))
         (depth (or depth 0)))
    (let ((bluesky-ui--item-id item-id))
      (vui-vstack
       :indent (* 2 depth)
       (bluesky-ui--separator depth)
       (bluesky-ui--fragment
        (when (or bluesky-ui--quoted-post (> depth 0))
          (bluesky-ui--fragment
           (bluesky-ui--text "Quoted post" :face 'bluesky-quote-label)
           (vui-newline)))
        (bluesky-ui-author author)
        (vui-space)
        (bluesky-ui--text "|" :face 'bluesky-time)
        (vui-space)
        (bluesky-ui--text (bluesky-ui-relative-time (plist-get record :createdAt))
                          :face 'bluesky-time))
       (bluesky-ui-labels post)
       (bluesky-ui-record host record author-did depth (plist-get post :embed))
       (bluesky-ui--text (bluesky-ui--stats-text post)
                         :face 'bluesky-post-stats)
       (bluesky-ui--text "")))))

(provide 'bluesky-ui)
;;; bluesky-ui.el ends here
