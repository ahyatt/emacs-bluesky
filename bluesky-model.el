;;; bluesky-model.el --- Bluesky app-view model helpers -*- lexical-binding: t; -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Assisted-by: ChatGPT:chatgpt-5.5
;; Homepage: https://github.com/ahyatt/emacs-bluesky
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Shared helpers for interpreting Bluesky app-view records.  These functions
;; keep navigation and rendering on the same understanding of post ids,
;; embedded records, quote posts, and flattened thread items.

;;; Code:

(require 'cl-lib)

(defun bluesky-model-post-id (post)
  "Return a stable id for POST."
  (or (plist-get post :uri)
      (plist-get post :cid)
      (secure-hash 'sha1 (prin1-to-string post))))

(defun bluesky-model-quoted-post-item-id (parent-id post)
  "Return the render item id for POST quoted under PARENT-ID."
  (if parent-id
      (format "%s/quote/%s" parent-id (bluesky-model-post-id post))
    (bluesky-model-post-id post)))

(defun bluesky-model-item-id (post &optional parent-id)
  "Return the navigable item id for POST, optionally quoted under PARENT-ID."
  (if parent-id
      (bluesky-model-quoted-post-item-id parent-id post)
    (bluesky-model-post-id post)))

(defun bluesky-model--unique-item-id (base-id seen)
  "Return a unique item id from BASE-ID, updating SEEN."
  (let* ((count (1+ (or (gethash base-id seen) 0)))
         (id (if (= count 1)
                 base-id
               (format "%s#%d" base-id count))))
    (puthash base-id count seen)
    id))

(defun bluesky-model-record-view-as-post (record-view)
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

(defun bluesky-model-embedded-record-wrapper-p (record-view)
  "Return non-nil when RECORD-VIEW is a wrapper around another record view."
  (let ((type (plist-get record-view :$type)))
    (and (plist-get record-view :record)
         (not (bluesky-model-record-view-as-post record-view))
         (or (null type)
             (equal type "app.bsky.embed.record#view")))))

(defun bluesky-model-embedded-record-post (record-view)
  "Return the post view embedded in RECORD-VIEW, when possible."
  (when record-view
    (if (bluesky-model-embedded-record-wrapper-p record-view)
        (bluesky-model-embedded-record-post (plist-get record-view :record))
      (bluesky-model-record-view-as-post record-view))))

(defun bluesky-model-embedded-quote-posts (embed)
  "Return quoted post views embedded in EMBED."
  (let (posts)
    (when embed
      (when-let* ((media (plist-get embed :media)))
        (setq posts (append posts
                            (bluesky-model-embedded-quote-posts media))))
      (when-let* ((record (plist-get embed :record))
                  (post (bluesky-model-embedded-record-post record)))
        (push post posts)))
    (nreverse posts)))

(defun bluesky-model-post-quote-posts (post)
  "Return quoted post views embedded in POST."
  (let ((record (plist-get post :record)))
    (bluesky-model-embedded-quote-posts
     (or (plist-get post :embed)
         (plist-get record :embed)))))

(defun bluesky-model-flatten-posts (posts &optional depth render parent-id seen)
  "Return a flat list of navigable POSTS.
RENDER is non-nil when POSTS should be rendered as top-level list entries."
  (let ((depth (or depth 0))
        (seen (or seen (make-hash-table :test 'equal)))
        items)
    (dolist (post posts (nreverse items))
      (let ((id (bluesky-model--unique-item-id
                 (bluesky-model-item-id post parent-id)
                 seen)))
        (push (list :id id :post post :depth depth :render render) items)
        (dolist (quote (bluesky-model-flatten-posts
                        (bluesky-model-post-quote-posts post)
                        (1+ depth)
                        nil
                        id
                        seen))
          (push quote items))))))

(defun bluesky-model-flatten-thread (thread &optional depth seen)
  "Return a flat, depth-first list of posts from THREAD.
THREAD is an `app.bsky.feed.defs#threadViewPost' shape."
  (let ((depth (or depth 0))
        (seen (or seen (make-hash-table :test 'equal)))
        (post (plist-get thread :post))
        items)
    (when post
      (dolist (item (bluesky-model-flatten-posts (list post) depth t nil seen))
        (push item items)))
    (dolist (reply (append (plist-get thread :replies) nil))
      (dolist (child (bluesky-model-flatten-thread reply (1+ depth) seen))
        (push child items)))
    (nreverse items)))

(defun bluesky-model-thread-ancestors (thread)
  "Return THREAD's ancestor chain as root-first thread nodes."
  (let (ancestors)
    (while (plist-get thread :parent)
      (setq thread (plist-get thread :parent))
      (push thread ancestors))
    ancestors))

(defun bluesky-model-thread-items (thread)
  "Return a flat list of renderable items for THREAD, including ancestors."
  (let ((seen (make-hash-table :test 'equal))
        items)
    (dolist (ancestor (bluesky-model-thread-ancestors thread))
      (let ((post (plist-get ancestor :post)))
        (when post
          (dolist (item (bluesky-model-flatten-posts
                         (list post) 0 t nil seen))
            (push item items)))))
    (append (nreverse items)
            (bluesky-model-flatten-thread thread nil seen))))

(provide 'bluesky-model)

;;; bluesky-model.el ends here
