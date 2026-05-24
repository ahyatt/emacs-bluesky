;;; Bluesky-post.el --- Bluesky post composer -*- lexical-binding: t; -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Assisted-by: ChatGPT:chatgpt-5.5
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
;; Compose and submit Bluesky posts from an Emacs buffer.

;;; Code:

(require 'bluesky-conn)
(require 'cl-lib)
(require 'futur)
(require 'image)
(require 'mailcap)
(require 'subr-x)

(declare-function image-size "image.c" (spec &optional pixels frame))

(defgroup bluesky nil
  "Bluesky client for Emacs."
  :group 'applications)

(defcustom bluesky-post-character-limit 300
  "Maximum number of characters allowed in a Bluesky post."
  :type 'integer
  :group 'bluesky)

(defcustom bluesky-post-byte-limit 3000
  "Maximum number of UTF-8 bytes allowed in a Bluesky post."
  :type 'integer
  :group 'bluesky)

(defface bluesky-post-context
  '((t :inherit shadow))
  "Face for the post context shown in compose buffers.")

(defvar bluesky-post-mode-map
  (make-sparse-keymap)
  "Keymap for composing Bluesky posts.")

(define-key bluesky-post-mode-map (kbd "C-c C-c") #'bluesky-post-submit)
(define-key bluesky-post-mode-map (kbd "C-c C-k") #'bluesky-post-cancel)
(define-key bluesky-post-mode-map (kbd "C-c C-f") #'bluesky-post-cycle-format)
(define-key bluesky-post-mode-map (kbd "C-c C-r") #'bluesky-post-cycle-reply-policy)
(define-key bluesky-post-mode-map (kbd "C-c C-e") #'bluesky-post-toggle-embedding)
(define-key bluesky-post-mode-map (kbd "C-c C-a") #'bluesky-post-add-media)
(define-key bluesky-post-mode-map (kbd "C-c C-d") #'bluesky-post-clear-media)

(defvar-local bluesky-post-host nil
  "Host used by the current compose buffer.")

(defvar-local bluesky-post-session nil
  "Session used by the current compose buffer.")

(defvar-local bluesky-post-reply-to nil
  "Post being replied to in the current compose buffer, or nil.")

(defvar-local bluesky-post-source-buffer nil
  "Buffer that opened the current compose buffer.")

(defvar-local bluesky-post-content-start nil
  "Marker for the first editable character in the compose buffer.")

(defvar-local bluesky-post-context-overlay nil
  "Overlay covering the reply context in the compose buffer.")

(defvar-local bluesky-post-source-format 'plain
  "Markup format used by the current compose buffer.")

(defvar-local bluesky-post-reply-policy 'everyone
  "Reply policy for the post being composed.")

(defvar-local bluesky-post-allow-embedding t
  "Non-nil when the post may be embedded by other posts.")

(defvar-local bluesky-post-media nil
  "Media attachments for the current compose buffer.")

(defvar-local bluesky-post-submitting nil
  "Non-nil while the compose buffer is submitting.")

(defconst bluesky-post--source-formats '(plain markdown org)
  "Supported compose source formats.")

(defconst bluesky-post--reply-policies '(everyone mentions following followers nobody)
  "Supported reply policies for new posts.")

(defconst bluesky-post--image-size-limit 2000000
  "Maximum image blob size accepted by app.bsky.embed.images.")

(defconst bluesky-post--video-size-limit 100000000
  "Maximum video blob size accepted by app.bsky.embed.video.")

(define-derived-mode bluesky-post-mode text-mode "Bluesky-Post"
  "Major mode for composing Bluesky posts.
Use \\<bluesky-post-mode-map>\\[bluesky-post-submit] to submit and
\\[bluesky-post-cancel] to cancel."
  (setq-local header-line-format '(:eval (bluesky-post--header-line)))
  (setq-local bluesky-post-content-start (copy-marker (point-min) nil))
  (add-hook 'after-change-functions #'bluesky-post--after-change nil t)
  (visual-line-mode 1))

(defun bluesky-post--json-truthy-p (value)
  "Return non-nil when VALUE represents true in Bluesky JSON data."
  (and value (not (eq value :json-false))))

(defun bluesky-post-reply-disabled-p (post)
  "Return non-nil when POST does not allow replies."
  (bluesky-post--json-truthy-p
   (plist-get (plist-get post :viewer) :replyDisabled)))

(defun bluesky-post--format-label (symbol)
  "Return a display label for SYMBOL."
  (capitalize (symbol-name symbol)))

(defun bluesky-post--content-string ()
  "Return the editable post content."
  (buffer-substring-no-properties
   (or bluesky-post-content-start (point-min))
   (point-max)))

(defun bluesky-post--combining-char-p (char)
  "Return non-nil when CHAR is a combining mark."
  (memq (get-char-code-property char 'general-category) '(Mn Mc Me)))

(defun bluesky-post--variation-selector-p (char)
  "Return non-nil when CHAR is a Unicode variation selector."
  (or (and (>= char #xfe00) (<= char #xfe0f))
      (and (>= char #xe0100) (<= char #xe01ef))))

(defun bluesky-post--emoji-modifier-p (char)
  "Return non-nil when CHAR is an emoji modifier."
  (and (>= char #x1f3fb) (<= char #x1f3ff)))

(defun bluesky-post--regional-indicator-p (char)
  "Return non-nil when CHAR is a regional indicator symbol."
  (and (>= char #x1f1e6) (<= char #x1f1ff)))

(defun bluesky-post--character-count (text)
  "Return an approximate extended grapheme cluster count for TEXT."
  (let ((count 0)
        join-next
        (regional-indicators 0))
    (dolist (char (string-to-list text) count)
      (cond
       ((= char #x200d)
        (setq join-next t
              regional-indicators 0))
       ((or (bluesky-post--combining-char-p char)
            (bluesky-post--variation-selector-p char)
            (bluesky-post--emoji-modifier-p char))
        nil)
       (join-next
        (setq join-next nil
              regional-indicators 0))
       ((bluesky-post--regional-indicator-p char)
        (if (= (mod regional-indicators 2) 0)
            (setq count (1+ count)
                  regional-indicators 1)
          (setq regional-indicators 0)))
       (t
        (setq count (1+ count)
              regional-indicators 0))))))

(defun bluesky-post--byte-count (text)
  "Return the UTF-8 byte count for TEXT."
  (length (encode-coding-string text 'utf-8)))

(defun bluesky-post--over-limit-p (&optional text)
  "Return non-nil when TEXT, or the current content, exceeds post limits."
  (let ((text (or text (bluesky-post--content-string))))
    (or (> (bluesky-post--character-count text) bluesky-post-character-limit)
        (> (bluesky-post--byte-count text) bluesky-post-byte-limit))))

(defun bluesky-post--header-line ()
  "Return the compose buffer header line."
  (let* ((text (bluesky-post--content-string))
         (chars (bluesky-post--character-count text))
         (bytes (bluesky-post--byte-count text))
         (over (bluesky-post--over-limit-p text))
         (media (bluesky-post--media-summary))
         (count (format "%d/%d chars %d/%d bytes"
                        chars bluesky-post-character-limit
                        bytes bluesky-post-byte-limit))
         (count-face (if over 'error 'mode-line-emphasis))
         (status (cond
                  (bluesky-post-submitting "Submitting")
                  (over "Too long")
                  ((and (string-empty-p text) (not bluesky-post-media)) "Empty")
                  (t "Ready"))))
    (concat
     "C-c C-c Post  C-c C-k Cancel  "
     "C-c C-f Format:" (bluesky-post--format-label bluesky-post-source-format)
     "  C-c C-r Replies:" (bluesky-post--format-label bluesky-post-reply-policy)
     "  C-c C-e Embeds:" (if bluesky-post-allow-embedding "On" "Off")
     "  C-c C-a Media:" media
     "  "
     (propertize count 'face count-face)
     "  "
     (propertize status 'face (if over 'error 'mode-line-emphasis)))))

(defun bluesky-post--after-change (&rest _args)
  "Refresh compose feedback after edits."
  (force-mode-line-update))

(defun bluesky-post--cycle (value values)
  "Return the item after VALUE in VALUES."
  (or (cadr (memq value values))
      (car values)))

(defun bluesky-post-cycle-format ()
  "Cycle the source markup format."
  (interactive nil bluesky-post-mode)
  (setq bluesky-post-source-format
        (bluesky-post--cycle bluesky-post-source-format
                             bluesky-post--source-formats))
  (force-mode-line-update)
  (message "Bluesky source format: %s"
           (bluesky-post--format-label bluesky-post-source-format)))

(defun bluesky-post-cycle-reply-policy ()
  "Cycle the reply policy for the post being composed."
  (interactive nil bluesky-post-mode)
  (setq bluesky-post-reply-policy
        (bluesky-post--cycle bluesky-post-reply-policy
                             bluesky-post--reply-policies))
  (force-mode-line-update)
  (message "Bluesky replies: %s"
           (bluesky-post--format-label bluesky-post-reply-policy)))

(defun bluesky-post-toggle-embedding ()
  "Toggle whether the post may be embedded by other posts."
  (interactive nil bluesky-post-mode)
  (setq bluesky-post-allow-embedding (not bluesky-post-allow-embedding))
  (force-mode-line-update)
  (message "Bluesky embeds: %s" (if bluesky-post-allow-embedding "on" "off")))

(defun bluesky-post--media-summary ()
  "Return a short summary of media attachments."
  (if bluesky-post-media
      (pcase (plist-get (car bluesky-post-media) :kind)
        ('image (format "%d image%s"
                        (length bluesky-post-media)
                        (if (= (length bluesky-post-media) 1) "" "s")))
        ('video "1 video")
        (_ "Attached"))
    "None"))

(defun bluesky-post--file-size (file)
  "Return FILE size in bytes."
  (file-attribute-size (file-attributes file)))

(defun bluesky-post--mime-type (file)
  "Return FILE's MIME type."
  (or (mailcap-file-name-to-mime-type file)
      "application/octet-stream"))

(defun bluesky-post--media-kind (mime-type)
  "Return the Bluesky media kind for MIME-TYPE."
  (cond
   ((string-prefix-p "image/" mime-type) 'image)
   ((equal mime-type "video/mp4") 'video)
   (t nil)))

(defun bluesky-post--image-aspect-ratio (file)
  "Return app.bsky.embed.defs aspectRatio for image FILE, if available."
  (when-let* ((size (ignore-errors
                      (image-size (create-image file) t)))
              (width (car size))
              (height (cdr size)))
    (when (and (integerp width) (integerp height) (> width 0) (> height 0))
      (list :width width :height height))))

(defun bluesky-post--validate-new-media (kind file mime-type)
  "Signal a user error unless KIND FILE MIME-TYPE can be attached."
  (let ((size (bluesky-post--file-size file)))
    (pcase kind
      ('image
       (when (and bluesky-post-media
                  (not (eq (plist-get (car bluesky-post-media) :kind) 'image)))
         (user-error "Cannot mix images and video in one Bluesky post"))
       (when (>= (length bluesky-post-media) 4)
         (user-error "Bluesky image posts can include at most 4 images"))
       (when (> size bluesky-post--image-size-limit)
         (user-error "Image exceeds Bluesky's 2 MB image limit")))
      ('video
       (when bluesky-post-media
         (user-error "Bluesky video posts can include only one video and no images"))
       (unless (equal mime-type "video/mp4")
         (user-error "Bluesky video embeds require an MP4 file"))
       (when (> size bluesky-post--video-size-limit)
         (user-error "Video exceeds Bluesky's 100 MB video limit")))
      (_
       (user-error "Unsupported media type: %s" mime-type)))))

(defun bluesky-post-add-media (file alt)
  "Attach media FILE with ALT text to the current post."
  (interactive
   (let* ((file (read-file-name "Attach media: " nil nil t))
          (mime-type (bluesky-post--mime-type file))
          (kind (bluesky-post--media-kind mime-type)))
     (unless kind
       (user-error "Unsupported media type: %s" mime-type))
     (list file
           (read-string
            (if (eq kind 'image) "Alt text: " "Video alt text: "))))
   bluesky-post-mode)
  (let* ((file (expand-file-name file))
         (mime-type (bluesky-post--mime-type file))
         (kind (bluesky-post--media-kind mime-type)))
    (unless (file-readable-p file)
      (user-error "File is not readable: %s" file))
    (bluesky-post--validate-new-media kind file mime-type)
    (setq bluesky-post-media
          (append bluesky-post-media
                  (list (append (list :kind kind
                                      :file file
                                      :mime-type mime-type
                                      :alt alt)
                                (when (eq kind 'image)
                                  (list :aspectRatio
                                        (bluesky-post--image-aspect-ratio file)))))))
    (force-mode-line-update)
    (message "Bluesky media: %s" (bluesky-post--media-summary))))

(defun bluesky-post-clear-media ()
  "Remove all media attachments from the current post."
  (interactive nil bluesky-post-mode)
  (setq bluesky-post-media nil)
  (force-mode-line-update)
  (message "Bluesky media cleared"))

(defun bluesky-post--utf-8-bytes (text)
  "Return the UTF-8 byte length of TEXT."
  (length (encode-coding-string text 'utf-8)))

(defun bluesky-post--link-facet (start end uri)
  "Return a link facet for UTF-8 byte range START to END pointing at URI."
  (list :index (list :byteStart start :byteEnd end)
        :features (vector (list :$type "app.bsky.richtext.facet#link"
                                :uri uri))))

(defun bluesky-post--tag-facet (start end tag)
  "Return a hashtag facet for UTF-8 byte range START to END naming TAG."
  (list :index (list :byteStart start :byteEnd end)
        :features (vector (list :$type "app.bsky.richtext.facet#tag"
                                :tag tag))))

(defun bluesky-post--url-looking-p (text)
  "Return non-nil if TEXT looks like an absolute URL."
  (string-match-p "\\`https?://" text))

(defun bluesky-post--facet-range (facet)
  "Return FACET's byte range as a cons cell."
  (let ((index (plist-get facet :index)))
    (cons (plist-get index :byteStart)
          (plist-get index :byteEnd))))

(defun bluesky-post--range-overlaps-facet-p (start end facet)
  "Return non-nil when byte range START to END overlaps FACET."
  (pcase-let ((`(,facet-start . ,facet-end)
               (bluesky-post--facet-range facet)))
    (and (< start facet-end)
         (< facet-start end))))

(defun bluesky-post--range-overlaps-facets-p (start end facets)
  "Return non-nil when byte range START to END overlaps any FACETS."
  (cl-some (lambda (facet)
             (bluesky-post--range-overlaps-facet-p start end facet))
           facets))

(defun bluesky-post--plain-url-facets (text &optional existing-facets)
  "Return link facets for plain URLs in TEXT.
URLs overlapping EXISTING-FACETS are ignored."
  (let ((start 0)
        facets)
    (while (string-match "\\(https?://[^[:space:]<>()]+\\)" text start)
      (let* ((url (match-string 1 text))
             (char-start (match-beginning 1))
             (char-end (match-end 1))
             (byte-start (bluesky-post--utf-8-bytes
                          (substring text 0 char-start)))
             (byte-end (+ byte-start
                          (bluesky-post--utf-8-bytes
                           (substring text char-start char-end)))))
        (unless (bluesky-post--range-overlaps-facets-p
                 byte-start byte-end existing-facets)
          (push (bluesky-post--link-facet byte-start byte-end url) facets))
        (setq start char-end)))
    (nreverse facets)))

(defun bluesky-post--hashtag-facets (text &optional existing-facets)
  "Return tag facets for hashtags in TEXT.
Hashtags overlapping EXISTING-FACETS are ignored."
  (let ((start 0)
        facets)
    (while (string-match "\\(?:\\`\\|[^[:alnum:]_]\\)\\(#+\\([[:alnum:]_]+\\)\\)" text start)
      (let* ((hashes-and-tag (match-string 1 text))
             (tag-body (match-string 2 text))
             (char-start (match-beginning 1))
             (char-end (match-end 1))
             (tag (concat (substring hashes-and-tag 1 (- (length tag-body)))
                          tag-body))
             (byte-start (bluesky-post--utf-8-bytes
                          (substring text 0 char-start)))
             (byte-end (+ byte-start
                          (bluesky-post--utf-8-bytes
                           (substring text char-start char-end)))))
        (unless (or (> (bluesky-post--character-count tag) 64)
                    (> (bluesky-post--utf-8-bytes tag) 640)
                    (bluesky-post--range-overlaps-facets-p
                     byte-start byte-end existing-facets))
          (push (bluesky-post--tag-facet byte-start byte-end tag) facets))
        (setq start char-end)))
    (nreverse facets)))

(defun bluesky-post--replace-links (text regexp label-index uri-index)
  "Replace links in TEXT matching REGEXP.
LABEL-INDEX is the regexp group for replacement text, and URI-INDEX is the group
for the link URI.  Return a plist with :text and :facets."
  (let ((start 0)
        (out "")
        facets)
    (while (string-match regexp text start)
      (let* ((before (substring text start (match-beginning 0)))
             (label (match-string label-index text))
             (uri (match-string uri-index text))
             (byte-start (+ (bluesky-post--utf-8-bytes out)
                            (bluesky-post--utf-8-bytes before)))
             (byte-end (+ byte-start (bluesky-post--utf-8-bytes label))))
        (setq out (concat out before label))
        (when (bluesky-post--url-looking-p uri)
          (push (bluesky-post--link-facet byte-start byte-end uri) facets))
        (setq start (match-end 0))))
    (setq out (concat out (substring text start)))
    (list :text out :facets (nreverse facets))))

(defun bluesky-post--rich-text ()
  "Return a plist with converted :text and :facets for the current buffer."
  (let* ((source (bluesky-post--content-string))
         (converted
          (pcase bluesky-post-source-format
            ('markdown
             (bluesky-post--replace-links
              source "\\[\\([^]\n]+\\)\\](\\(https?://[^)\n]+\\))" 1 2))
            ('org
             (let ((with-label
                    (bluesky-post--replace-links
                     source "\\[\\[\\(https?://[^]\n]+\\)\\]\\[\\([^]\n]+\\)\\]\\]" 2 1)))
               (with-temp-buffer
                 (insert (plist-get with-label :text))
                 (goto-char (point-min))
                 (while (re-search-forward "\\[\\[\\(https?://[^]\n]+\\)\\]\\]" nil t)
                   (replace-match "\\1" t))
                 (list :text (buffer-string)
                       :facets (plist-get with-label :facets)))))
            (_
             (list :text source :facets nil)))))
    (let* ((text (plist-get converted :text))
           (converted-facets (plist-get converted :facets))
           (link-facets (append converted-facets
                                (bluesky-post--plain-url-facets
                                 text converted-facets)))
           (facets (append link-facets
                           (bluesky-post--hashtag-facets
                            text link-facets))))
      (list :text text
            :facets (and facets (vconcat facets))))))

(defun bluesky-post--reply-ref (post)
  "Return the reply ref for replying to POST."
  (let* ((record (plist-get post :record))
         (existing-reply (plist-get record :reply))
         (parent (bluesky-conn--subject post))
         (root (or (plist-get existing-reply :root) parent)))
    (list :root root :parent parent)))

(defun bluesky-post--threadgate-allow (reply-policy)
  "Return the threadgate allow vector for REPLY-POLICY."
  (pcase reply-policy
    ('everyone nil)
    ('nobody [])
    ('mentions (vector (list :$type "app.bsky.feed.threadgate#mentionRule")))
    ('following (vector (list :$type "app.bsky.feed.threadgate#followingRule")))
    ('followers (vector (list :$type "app.bsky.feed.threadgate#followerRule")))))

(defun bluesky-post--postgate-rules (allow-embedding)
  "Return postgate embedding rules when ALLOW-EMBEDDING is nil."
  (unless allow-embedding
    (vector (list :$type "app.bsky.feed.postgate#disableRule"))))

(defun bluesky-post--uploaded-image (media blob)
  "Return an app.bsky.embed.images image item for MEDIA and uploaded BLOB."
  (append (list :image blob
                :alt (or (plist-get media :alt) ""))
          (when-let* ((aspect-ratio (plist-get media :aspectRatio)))
            (list :aspectRatio aspect-ratio))))

(defun bluesky-post--uploaded-video (media blob)
  "Return an app.bsky.embed.video record for MEDIA and uploaded BLOB."
  (append (list :$type "app.bsky.embed.video"
                :video blob)
          (when-let* ((alt (plist-get media :alt)))
            (unless (string-empty-p alt)
              (list :alt alt)))
          (when-let* ((aspect-ratio (plist-get media :aspectRatio)))
            (list :aspectRatio aspect-ratio))))

(defun bluesky-post--uploaded-media-embed (media blobs)
  "Return an app.bsky embed for MEDIA using uploaded BLOBS."
  (when media
    (pcase (plist-get (car media) :kind)
      ('image
       (list :$type "app.bsky.embed.images"
             :images (vconcat (cl-mapcar #'bluesky-post--uploaded-image
                                          media blobs))))
      ('video
       (bluesky-post--uploaded-video (car media) (car blobs))))))

(defun bluesky-post--upload-media-future (host handle media)
  "Return a future resolving to an embed after uploading MEDIA."
  (if media
      (futur-bind
       (apply #'futur-list
              (mapcar (lambda (item)
                        (bluesky-conn-upload-blob
                         host handle
                         (plist-get item :file)
                         (plist-get item :mime-type)))
                      media))
       (lambda (blobs)
         (bluesky-post--uploaded-media-embed media blobs)))
    (futur-done nil)))

(defun bluesky-post--refresh-source-buffer (buffer)
  "Refresh Bluesky source BUFFER, when possible."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (fboundp 'bluesky-feed-refresh)
        (ignore-errors (bluesky-feed-refresh))))))

(defun bluesky-post--create-postgate-if-needed
    (created host handle allow-embedding)
  "Create postgate for CREATED post when the current options require it.
HOST and HANDLE identify the posting account.  ALLOW-EMBEDDING controls whether
embedding remains enabled."
  (let ((rules (bluesky-post--postgate-rules allow-embedding)))
    (if rules
        (futur-bind
         (bluesky-conn-create-postgate host handle
                                       (plist-get created :uri)
                                       rules)
         (lambda (_postgate) created))
      (futur-done created))))

(defun bluesky-post--create-threadgate-if-needed
    (created host handle reply-policy allow-embedding)
  "Create threadgate for CREATED post when the current options require it.
HOST and HANDLE identify the posting account.  REPLY-POLICY controls the
threadgate allow rules.  ALLOW-EMBEDDING is passed through to postgate handling."
  (let ((allow (bluesky-post--threadgate-allow reply-policy)))
    (if (eq reply-policy 'everyone)
        (bluesky-post--create-postgate-if-needed
         created host handle allow-embedding)
      (futur-bind
       (bluesky-conn-create-threadgate host handle
                                       (plist-get created :uri)
                                       allow)
       (lambda (_threadgate)
         (bluesky-post--create-postgate-if-needed
          created host handle allow-embedding))))))

(defun bluesky-post--submit-future ()
  "Return a future that submits the current compose buffer."
  (let* ((rich (bluesky-post--rich-text))
         (text (plist-get rich :text))
         (facets (plist-get rich :facets))
         (reply (and bluesky-post-reply-to
                     (bluesky-post--reply-ref bluesky-post-reply-to)))
         (host bluesky-post-host)
         (handle (plist-get bluesky-post-session :handle))
         (reply-policy bluesky-post-reply-policy)
         (allow-embedding bluesky-post-allow-embedding)
         (media bluesky-post-media))
    (futur-let* ((embed <- (bluesky-post--upload-media-future
                            host handle media))
                 (created <- (bluesky-conn-create-post
                              host handle
                              "app.bsky.feed.post"
                              (bluesky-conn-record text nil facets reply embed))))
      (bluesky-post--create-threadgate-if-needed
       created host handle reply-policy allow-embedding))))

(defun bluesky-post-submit ()
  "Submit the current Bluesky post."
  (interactive nil bluesky-post-mode)
  (let ((text (bluesky-post--content-string)))
    (cond
     (bluesky-post-submitting
      (user-error "Already submitting"))
     ((and (string-blank-p text) (not bluesky-post-media))
      (user-error "Cannot post empty text without media"))
     ((bluesky-post--over-limit-p text)
      (user-error "Post exceeds Bluesky length limits"))
     ((and bluesky-post-reply-to
           (bluesky-post-reply-disabled-p bluesky-post-reply-to))
      (user-error "Replies are disabled for this post"))
     ((not (and bluesky-post-host bluesky-post-session))
      (user-error "No Bluesky session for this compose buffer"))
     (t
      (setq bluesky-post-submitting t)
      (force-mode-line-update)
      (let ((buffer (current-buffer))
            (source-buffer bluesky-post-source-buffer))
        (futur-bind
         (bluesky-post--submit-future)
         (lambda (_created)
           (message "Bluesky: posted")
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer))
           (bluesky-post--refresh-source-buffer source-buffer))
         (lambda (err)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (setq bluesky-post-submitting nil)
               (force-mode-line-update)))
           (message "Bluesky: post failed: %s"
                    (if (fboundp 'bluesky--error-message)
                        (bluesky--error-message err)
                      (error-message-string err)))
           (futur-failed err))))))))

(defun bluesky-post-cancel ()
  "Cancel the current Bluesky post."
  (interactive nil bluesky-post-mode)
  (unless (or (not (buffer-modified-p))
              (yes-or-no-p "Discard this Bluesky post? "))
    (user-error "Canceled"))
  (kill-buffer (current-buffer)))

(defun bluesky-post--reply-context (post)
  "Return short context text for POST."
  (let* ((author (plist-get post :author))
         (handle (plist-get author :handle))
         (name (or (plist-get author :displayName) handle ""))
         (text (or (plist-get (plist-get post :record) :text) "")))
    (format "Replying to %s @%s\n%s\n\n"
            name handle text)))

(defun bluesky-post--insert-context (reply-to)
  "Insert read-only context for REPLY-TO."
  (when reply-to
    (let ((start (point)))
      (insert (bluesky-post--reply-context reply-to))
      (setq bluesky-post-context-overlay (make-overlay start (point)))
      (overlay-put bluesky-post-context-overlay 'face 'bluesky-post-context)
      (overlay-put bluesky-post-context-overlay 'read-only t)
      (overlay-put bluesky-post-context-overlay 'front-sticky t)
      (overlay-put bluesky-post-context-overlay 'rear-nonsticky t))))

;;;###autoload
(cl-defun bluesky-post-compose (&key host session reply-to source-buffer)
  "Open a Bluesky compose buffer.
HOST and SESSION identify the account to post with.  REPLY-TO, when non-nil, is
the post being replied to.  SOURCE-BUFFER is refreshed after a successful post."
  (when (and reply-to (bluesky-post-reply-disabled-p reply-to))
    (user-error "Replies are disabled for this post"))
  (let ((buffer (generate-new-buffer
                 (if reply-to "*Bluesky Reply*" "*Bluesky Post*"))))
    (with-current-buffer buffer
      (bluesky-post-mode)
      (setq-local bluesky-post-host host)
      (setq-local bluesky-post-session session)
      (setq-local bluesky-post-reply-to reply-to)
      (setq-local bluesky-post-source-buffer source-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (bluesky-post--insert-context reply-to)
        (setq-local bluesky-post-content-start (copy-marker (point) nil)))
      (set-buffer-modified-p nil)
      (goto-char (point-max)))
    (pop-to-buffer buffer)))

(provide 'bluesky-post)
;;; bluesky-post.el ends here
