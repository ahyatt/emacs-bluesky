;;; bluesky-conn-test.el --- Tests for Bluesky connection helpers -*- lexical-binding: t; -*-

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
(require 'bluesky-conn)

(ert-deftest bluesky-conn-query-string-encodes-xrpc-scalars ()
  (should (equal (bluesky-conn--query-string
                  (bluesky-conn--clean-args
                   (list :q "hello world"
                         :includePins t
                         :exclude :json-false
                         :cursor nil)))
                 "q=hello%20world&includePins=true&exclude=false")))

(ert-deftest bluesky-conn-query-string-repeats-array-values ()
  (should (equal (bluesky-conn--query-string
                  (list :tag (vector "emacs" "at proto")))
                 "tag=emacs&tag=at%20proto")))

(ert-deftest bluesky-conn-list-notifications-uses-repeated-reasons ()
  (let (call)
    (cl-letf (((symbol-function 'bluesky-conn-call-authed)
               (lambda (&rest args)
                 (setq call args)
                 :future)))
      (should (eq (bluesky-conn-list-notifications
                   "bsky.social" "user.test" "cursor" 50
                   (vector "like" "reply"))
                  :future))
      (should (equal call
                     '("bsky.social" "user.test" get
                        "app.bsky.notification.listNotifications"
                        :cursor "cursor"
                        :limit 50
                        :reasons ["like" "reply"]
                        :priority nil
                        :seenAt nil))))))

(ert-deftest bluesky-conn-created-at-uses-utc ()
  (let (args)
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (&rest call-args)
                 (setq args call-args)
                 "timestamp")))
      (should (equal (bluesky-conn--created-at) "timestamp"))
      (should (equal args '("%FT%TZ" nil t))))))

(ert-deftest bluesky-conn-record-includes-embed ()
  (let ((record (bluesky-conn-record
                 "with media" nil nil nil
                 (list :$type "app.bsky.embed.images" :images []))))
    (should (equal (plist-get record :text) "with media"))
    (should (equal (plist-get (plist-get record :embed) :$type)
                   "app.bsky.embed.images"))))

(ert-deftest bluesky-conn-upload-blob-passes-file-to-plz ()
  (let (plz-args)
    (cl-letf (((symbol-function 'plz)
               (lambda (&rest args)
                 (setq plz-args args)
                 (funcall (plist-get args :then)
                          (list :blob (list :ref "uploaded"))))))
      (should (equal
               (futur-blocking-wait-to-get-result
                (bluesky-conn--upload-blob-with-token
                 "bsky.social" "token" "/tmp/image.png" "image/png"))
               (list :blob (list :ref "uploaded"))))
      (should (equal (plist-get plz-args :body)
                     '(file "/tmp/image.png")))
      (should (equal (plist-get plz-args :body-type) 'binary)))))

(provide 'bluesky-conn-test)

;;; bluesky-conn-test.el ends here
