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

(defun bluesky-ui-test--record-view (uri text)
  "Return a minimal embedded record view with URI and TEXT."
  (list :$type "app.bsky.embed.record#viewRecord"
        :uri uri
        :cid (concat uri "-cid")
        :author (list :did "did:plc:quoted"
                      :handle "quoted.test"
                      :displayName "Quoted")
        :value (list :text text
                     :createdAt "2026-05-20T00:00:00Z")))

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

(ert-deftest bluesky-ui-relative-time-handles-missing-time ()
  (should (equal (bluesky-ui-relative-time nil) "unknown time")))

(ert-deftest bluesky-ui-blocked-post-renders-placeholder ()
  (let* ((post (list :$type "app.bsky.feed.defs#blockedPost"
                     :uri "at://did/blocked/post"
                     :author (list :did "did:plc:blocked")))
         (rendered (bluesky-ui-test--render-string
                    (bluesky-ui-post nil post))))
    (should (string-match-p "\\[blocked post\\]" rendered))))

(ert-deftest bluesky-ui-post-stats-render-before-quoted-record ()
  (let* ((quote (bluesky-ui-test--record-view
                 "at://did/quote/post"
                 "quoted child"))
         (post (append
                (bluesky-ui-test--post "at://did/root/post" "parent text")
                (list :replyCount 1
                      :repostCount 2
                      :quoteCount 3
                      :likeCount 4
                      :embed (list :$type "app.bsky.embed.record#view"
                                   :record quote))))
         (rendered (bluesky-ui-test--render-string
                    (bluesky-ui-post nil post))))
    (should (string-match-p "parent text" rendered))
    (should (string-match-p "Quoted post" rendered))
    (should (< (string-match-p "1 reply  |  2 reposts" rendered)
               (string-match-p "Quoted post" rendered)))))

(provide 'bluesky-ui-test)

;;; bluesky-ui-test.el ends here
