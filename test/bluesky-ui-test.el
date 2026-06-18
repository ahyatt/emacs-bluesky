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

(ert-deftest bluesky-ui-thread-depth-sets-line-prefix ()
  (with-temp-buffer
    (vui-render
     (bluesky-ui-post nil
                      (bluesky-ui-test--post
                       "at://did/reply/post"
                       "first paragraph\n\nsecond paragraph")
                      nil
                      2)
     (current-buffer))
    (goto-char (point-min))
    (search-forward "first paragraph")
    (should (equal (get-text-property (match-beginning 0) 'line-prefix)
                   "    "))
    (search-forward "second paragraph")
    (should (equal (get-text-property (match-beginning 0) 'line-prefix)
                   "    "))))

(ert-deftest bluesky-ui-post-sets-thread-fold-properties ()
  (with-temp-buffer
    (vui-render
     (bluesky-ui-post nil
                      (bluesky-ui-test--post
                       "at://did/reply/post"
                       "nested reply")
                      "item-1"
                      2)
     (current-buffer))
    (goto-char (point-min))
    (search-forward "nested reply")
    (should (equal (get-text-property (match-beginning 0)
                                      'bluesky-item-id)
                   "item-1"))
    (should (equal (get-text-property (match-beginning 0)
                                      'bluesky-thread-block-id)
                   "item-1"))
    (should (= (get-text-property (match-beginning 0)
                                  'bluesky-thread-depth)
               2))))

(ert-deftest bluesky-ui-quoted-post-keeps-containing-fold-block ()
  (with-temp-buffer
    (vui-render
     (let ((bluesky-ui--item-id "parent")
           (bluesky-ui--thread-block-id "parent"))
       (bluesky-ui-quoted-post
        nil
        (bluesky-ui-test--post "at://did/quote/post" "embedded quote")
        0))
     (current-buffer))
    (goto-char (point-min))
    (search-forward "embedded quote")
    (should (equal (get-text-property (match-beginning 0)
                                      'bluesky-thread-block-id)
                   "parent"))
    (should-not (equal (get-text-property (match-beginning 0)
                                          'bluesky-item-id)
                       "parent"))))

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

(ert-deftest bluesky-ui-post-media-renders-before-stats ()
  (cl-letf (((symbol-function 'bluesky-ui--async-image-node)
             (lambda (&rest _args)
               (bluesky-ui--text "[image]"))))
    (let* ((post (append
                  (bluesky-ui-test--post "at://did/root/post" "parent text")
                  (list :replyCount 1
                        :repostCount 2
                        :quoteCount 3
                        :likeCount 4
                        :embed (list :$type "app.bsky.embed.images#view"
                                     :images
                                     (vector (list :thumb "https://example.test/image.jpg"))))))
           (rendered (bluesky-ui-test--render-string
                      (bluesky-ui-post nil post))))
      (should (string-match-p "parent text" rendered))
      (should (string-match-p "\\[image\\]" rendered))
      (should (< (string-match-p "\\[image\\]" rendered)
                 (string-match-p "1 reply  |  2 reposts" rendered))))))

(ert-deftest bluesky-ui-post-record-with-media-splits-around-stats ()
  (cl-letf (((symbol-function 'bluesky-ui--async-image-node)
             (lambda (&rest _args)
               (bluesky-ui--text "[image]"))))
    (let* ((quote (bluesky-ui-test--record-view
                   "at://did/quote/post"
                   "quoted child"))
           (post (append
                  (bluesky-ui-test--post "at://did/root/post" "parent text")
                  (list :replyCount 1
                        :repostCount 2
                        :quoteCount 3
                        :likeCount 4
                        :embed
                        (list :$type "app.bsky.embed.recordWithMedia#view"
                              :media (list :$type "app.bsky.embed.images#view"
                                           :images
                                           (vector (list :thumb "https://example.test/image.jpg")))
                              :record quote))))
           (rendered (bluesky-ui-test--render-string
                      (bluesky-ui-post nil post))))
      (should (string-match-p "\\[image\\]" rendered))
      (should (string-match-p "Quoted post" rendered))
      (should (< (string-match-p "\\[image\\]" rendered)
                 (string-match-p "1 reply  |  2 reposts" rendered)))
      (should (< (string-match-p "1 reply  |  2 reposts" rendered)
                 (string-match-p "Quoted post" rendered))))))

(provide 'bluesky-ui-test)

;;; bluesky-ui-test.el ends here
