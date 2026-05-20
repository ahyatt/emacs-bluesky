;;; bluesky-post-test.el --- Tests for Bluesky post composition -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(setq load-prefer-newer t)
(require 'bluesky-post)

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

(provide 'bluesky-post-test)

;;; bluesky-post-test.el ends here
