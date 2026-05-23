;;; bluesky-post-test.el --- Tests for Bluesky post composition -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

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

(ert-deftest bluesky-post-commands-are-mode-scoped ()
  (dolist (command '(bluesky-post-cycle-format
                     bluesky-post-cycle-reply-policy
                     bluesky-post-toggle-embedding
                     bluesky-post-submit
                     bluesky-post-cancel))
    (should (equal (command-modes command) '(bluesky-post-mode)))))

(provide 'bluesky-post-test)

;;; bluesky-post-test.el ends here
