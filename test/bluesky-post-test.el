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
