;;; bluesky-post-test.el --- Tests for Bluesky post composition -*- lexical-binding: t; -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Assisted-by: ChatGPT:chatgpt-5.5
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

;;; Code:

(require 'ert)
(setq load-prefer-newer t)
(require 'bluesky-post)

(defun bluesky-post-test--facet-feature (facet)
  "Return FACET's first feature."
  (aref (plist-get facet :features) 0))

(ert-deftest bluesky-post-character-count-uses-graphemes ()
  (should (= (bluesky-post--character-count (string ?e #x0301)) 1))
  (should (= (bluesky-post--character-count "🇺🇸") 1))
  (should (= (bluesky-post--character-count "👨‍👩‍👧‍👦") 1)))

(ert-deftest bluesky-post-rich-text-does-not-duplicate-markdown-url-label ()
  (with-temp-buffer
    (bluesky-post-mode)
    (setq-local bluesky-post-source-format 'markdown)
    (insert "[https://example.com](https://example.com)")
    (let ((rich (bluesky-post--rich-text)))
      (should (equal (plist-get rich :text) "https://example.com"))
      (should (= (length (plist-get rich :facets)) 1)))))

(ert-deftest bluesky-post-rich-text-facets-plain-org-links ()
  (with-temp-buffer
    (bluesky-post-mode)
    (setq-local bluesky-post-source-format 'org)
    (insert "[[https://example.com]]")
    (let ((rich (bluesky-post--rich-text)))
      (should (equal (plist-get rich :text) "https://example.com"))
      (should (= (length (plist-get rich :facets)) 1)))))

(ert-deftest bluesky-post-rich-text-facets-hashtags ()
  (with-temp-buffer
    (bluesky-post-mode)
    (insert "é #emacs")
    (let* ((rich (bluesky-post--rich-text))
           (facets (plist-get rich :facets))
           (facet (aref facets 0))
           (index (plist-get facet :index))
           (feature (bluesky-post-test--facet-feature facet)))
      (should (equal (plist-get rich :text) "é #emacs"))
      (should (= (length facets) 1))
      (should (equal (plist-get feature :$type)
                     "app.bsky.richtext.facet#tag"))
      (should (equal (plist-get feature :tag) "emacs"))
      (should (= (plist-get index :byteStart) 3))
      (should (= (plist-get index :byteEnd) 9)))))

(ert-deftest bluesky-post-rich-text-ignores-hashtags-inside-urls ()
  (with-temp-buffer
    (bluesky-post-mode)
    (insert "https://example.com/#frag #emacs")
    (let* ((rich (bluesky-post--rich-text))
           (facets (plist-get rich :facets))
           (link (bluesky-post-test--facet-feature (aref facets 0)))
           (tag (bluesky-post-test--facet-feature (aref facets 1))))
      (should (= (length facets) 2))
      (should (equal (plist-get link :$type)
                     "app.bsky.richtext.facet#link"))
      (should (equal (plist-get tag :$type)
                     "app.bsky.richtext.facet#tag"))
      (should (equal (plist-get tag :tag) "emacs")))))

(ert-deftest bluesky-post-rich-text-facets-double-hash-tags ()
  (with-temp-buffer
    (bluesky-post-mode)
    (insert "##topic")
    (let* ((rich (bluesky-post--rich-text))
           (facet (aref (plist-get rich :facets) 0))
           (feature (bluesky-post-test--facet-feature facet)))
      (should (equal (plist-get feature :tag) "#topic")))))

(ert-deftest bluesky-post-media-embed-builds-images ()
  (let* ((blob-a (list :$type "blob" :ref "a" :mimeType "image/png" :size 10))
         (blob-b (list :$type "blob" :ref "b" :mimeType "image/png" :size 20))
         (media (list (list :kind 'image :alt "first" :aspectRatio
                            (list :width 10 :height 5))
                      (list :kind 'image :alt "second")))
         (embed (bluesky-post--uploaded-media-embed media
                                                    (list blob-a blob-b)))
         (images (plist-get embed :images)))
    (should (equal (plist-get embed :$type) "app.bsky.embed.images"))
    (should (= (length images) 2))
    (should (equal (plist-get (aref images 0) :image) blob-a))
    (should (equal (plist-get (aref images 0) :alt) "first"))
    (should (equal (plist-get (aref images 0) :aspectRatio)
                   (list :width 10 :height 5)))
    (should (equal (plist-get (aref images 1) :image) blob-b))))

(ert-deftest bluesky-post-media-embed-builds-video ()
  (let* ((blob (list :$type "blob" :ref "video" :mimeType "video/mp4" :size 10))
         (media (list (list :kind 'video :alt "clip")))
         (embed (bluesky-post--uploaded-media-embed media (list blob))))
    (should (equal (plist-get embed :$type) "app.bsky.embed.video"))
    (should (equal (plist-get embed :video) blob))
    (should (equal (plist-get embed :alt) "clip"))))

(ert-deftest bluesky-post-image-aspect-ratio-is-best-effort ()
  (let ((original-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (symbol)
                 (and (not (eq symbol 'image-size))
                      (funcall original-fboundp symbol)))))
      (should-not (bluesky-post--image-aspect-ratio "/tmp/image.png")))))

(ert-deftest bluesky-post-submit-text-builds-and-submits-post ()
  (let (create-args)
    (cl-letf (((symbol-function 'bluesky-conn-create-post)
               (lambda (&rest args)
                 (setq create-args args)
                 (futur-done (list :uri "at://did/app.bsky.feed.post/rkey"
                                   :cid "cid")))))
      (should
       (equal
        (futur-blocking-wait-to-get-result
         (bluesky-post-submit-text
          "[Example](https://example.com) #emacs"
          :host "bsky.social"
          :session (list :handle "user.test")
          :source-format 'markdown))
        (list :uri "at://did/app.bsky.feed.post/rkey"
              :cid "cid")))
      (should (equal (nth 0 create-args) "bsky.social"))
      (should (equal (nth 1 create-args) "user.test"))
      (should (equal (nth 2 create-args) "app.bsky.feed.post"))
      (let* ((record (nth 3 create-args))
             (facets (plist-get record :facets)))
        (should (equal (plist-get record :text) "Example #emacs"))
        (should (= (length facets) 2))))))

(ert-deftest bluesky-post-commands-are-mode-scoped ()
  (dolist (command '(bluesky-post-cycle-format
                     bluesky-post-cycle-reply-policy
                     bluesky-post-toggle-embedding
                     bluesky-post-add-media
                     bluesky-post-clear-media
                     bluesky-post-submit
                     bluesky-post-cancel))
    (should (equal (command-modes command) '(bluesky-post-mode)))))

(provide 'bluesky-post-test)

;;; bluesky-post-test.el ends here
