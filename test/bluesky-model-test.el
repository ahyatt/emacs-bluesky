;;; bluesky-model-test.el --- Tests for Bluesky app-view model helpers -*- lexical-binding: t; -*-

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
(require 'bluesky-model)

(defun bluesky-model-test--post (uri &optional embed)
  "Return a minimal post view with URI and EMBED."
  (append (list :uri uri
                :cid (concat uri "-cid")
                :author (list :did "did:plc:author")
                :record (list :text uri :createdAt "2026-05-20T00:00:00Z"))
          (when embed
            (list :embed embed))))

(defun bluesky-model-test--record-view (uri)
  "Return a minimal embedded record view for URI."
  (list :$type "app.bsky.embed.record#viewRecord"
        :uri uri
        :cid (concat uri "-cid")
        :author (list :did "did:plc:quoted")
        :value (list :text uri :createdAt "2026-05-20T00:00:00Z")))

(ert-deftest bluesky-model-unwraps-embedded-record-view ()
  (let* ((record (bluesky-model-test--record-view "at://did/one/post"))
         (wrapper (list :$type "app.bsky.embed.record#view"
                        :record record))
         (post (bluesky-model-embedded-record-post wrapper)))
    (should (equal (plist-get post :uri) "at://did/one/post"))
    (should (equal (plist-get (plist-get post :record) :text)
                   "at://did/one/post"))))

(ert-deftest bluesky-model-flattens-quoted-posts-as-navigation-items ()
  (let* ((quote (bluesky-model-test--record-view "at://did/quote/post"))
         (post (bluesky-model-test--post
                "at://did/root/post"
                (list :$type "app.bsky.embed.record#view"
                      :record quote)))
         (items (bluesky-model-flatten-posts (list post) 0 t)))
    (should (= (length items) 2))
    (should (plist-get (nth 0 items) :render))
    (should-not (plist-get (nth 1 items) :render))
    (should (equal (plist-get (nth 1 items) :depth) 1))
    (should (string-match-p "/quote/at://did/quote/post"
                            (plist-get (nth 1 items) :id)))))

(ert-deftest bluesky-model-thread-items-include-ancestors ()
  (let* ((ancestor-post (bluesky-model-test--post "at://did/root/post"))
         (child-post (bluesky-model-test--post "at://did/child/post"))
         (thread (list :post child-post
                       :parent (list :post ancestor-post)))
         (items (bluesky-model-thread-items thread)))
    (should (equal (mapcar (lambda (item)
                             (plist-get (plist-get item :post) :uri))
                           items)
                   '("at://did/root/post" "at://did/child/post")))))

(provide 'bluesky-model-test)

;;; bluesky-model-test.el ends here
