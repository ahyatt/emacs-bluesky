;;; bluesky-ui-test.el --- Tests for Bluesky UI helpers -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(setq load-prefer-newer t)
(require 'bluesky-ui)

(defun bluesky-ui-test--post (uri text)
  "Return a minimal post view with URI and TEXT."
  (list :uri uri
        :cid (concat uri "-cid")
        :author (list :did "did:plc:author"
                      :handle "author.test"
                      :displayName "Author")
        :record (list :text text :createdAt "2026-05-20T00:00:00Z")))

(defun bluesky-ui-test--render-string (node)
  "Render NODE into a temp buffer and return its text."
  (with-temp-buffer
    (vui-render node (current-buffer))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest bluesky-ui-rerender-preserves-point-after-hook ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (goto-char 6)
    (let ((original-point (point))
          (hook-ran nil)
          (vui--root-instance :root))
      (setq-local bluesky-ui-after-rerender-hook
                  (list (lambda ()
                          (setq hook-ran t)
                          (goto-char (point-min)))))
      (cl-letf (((symbol-function 'vui-rerender)
                 (lambda (_root)
                   (erase-buffer)
                   (insert "one\ntwo\nthree\n"))))
        (bluesky-ui--rerender-preserving-position))
      (should hook-ran)
      (should (= (point) original-point)))))

(ert-deftest bluesky-ui-thread-depth-does-not-render-quote-label ()
  (let ((rendered (bluesky-ui-test--render-string
                   (bluesky-ui-post nil
                                    (bluesky-ui-test--post
                                     "at://did/reply/post"
                                     "nested reply")
                                    nil
                                    1))))
    (should (string-match-p "nested reply" rendered))
    (should-not (string-match-p "Quoted post" rendered))))

(ert-deftest bluesky-ui-quoted-post-renders-quote-label ()
  (let ((rendered (bluesky-ui-test--render-string
                   (let ((bluesky-ui--item-id "parent"))
                     (bluesky-ui-quoted-post
                      nil
                      (bluesky-ui-test--post
                       "at://did/quote/post"
                       "embedded quote")
                      0)))))
    (should (string-match-p "embedded quote" rendered))
    (should (string-match-p "Quoted post" rendered))))

(provide 'bluesky-ui-test)

;;; bluesky-ui-test.el ends here
