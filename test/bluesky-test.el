;;; bluesky-test.el --- Tests for Bluesky commands -*- lexical-binding: t; -*-

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
(require 'bluesky)

(ert-deftest bluesky-compose-post-authenticates-outside-bluesky-buffer ()
  (let (with-session-called compose-args)
    (cl-letf (((symbol-function 'bluesky--with-session)
               (lambda (callback &optional username password host)
                 (setq with-session-called (list username password host))
                 (funcall callback
                          "https://bsky.social"
                          (list :handle "user.test"))))
              ((symbol-function 'bluesky-post-compose)
               (lambda (&rest args)
                 (setq compose-args args))))
      (with-temp-buffer
        (bluesky-compose-post "user.test" "password" "https://bsky.social")))
    (should (equal with-session-called
                   '("user.test" "password" "https://bsky.social")))
    (should (equal (plist-get compose-args :host) "https://bsky.social"))
    (should (equal (plist-get compose-args :session)
                   '(:handle "user.test")))
    (should-not (plist-get compose-args :source-buffer))))

(ert-deftest bluesky-compose-post-keeps-bluesky-source-buffer ()
  (let (compose-args)
    (cl-letf (((symbol-function 'bluesky--with-session)
               (lambda (callback &rest _args)
                 (funcall callback
                          "https://bsky.social"
                          (list :handle "user.test"))))
              ((symbol-function 'bluesky-post-compose)
               (lambda (&rest args)
                 (setq compose-args args))))
      (with-temp-buffer
        (bluesky-mode)
        (bluesky-compose-post)
        (should (eq (plist-get compose-args :source-buffer)
                    (current-buffer)))))))

(ert-deftest bluesky-buffer-commands-are-mode-scoped ()
  (dolist (command '(bluesky-feed-refresh
                     bluesky-feed-extend
                     bluesky-feed-next-post
                     bluesky-feed-previous-post
                     bluesky-open-current
                     bluesky-toggle-like
                     bluesky-toggle-repost
                     bluesky-toggle-bookmark
                     bluesky-reply
                     bluesky-open-thread
                     bluesky-activate-or-open-thread))
    (should (equal (command-modes command) '(bluesky-mode)))))

(ert-deftest bluesky-entry-commands-remain-global ()
  (dolist (command '(bluesky
                     bluesky-author
                     bluesky-search
                     bluesky-tag
                     bluesky-feed
                     bluesky-notifications
                     bluesky-likes
                     bluesky-replies
                     bluesky-compose-post))
    (should-not (command-modes command))))

(ert-deftest bluesky-remembers-post-authors-for-completion ()
  (let ((bluesky-known-authors nil))
    (bluesky--remember-post-authors
     (list (list :author (list :handle "author.test"
                               :did "did:plc:author")
                 :embed
                 (list :record
                       (list :author (list :handle "quoted.test"
                                           :did "did:plc:quoted")
                             :value (list :text "quoted"))))))
    (should (equal (bluesky--known-author-actors)
                   '("author.test" "quoted.test")))))

(ert-deftest bluesky-read-actor-completes-known-authors ()
  (let ((bluesky-known-authors nil)
        read-args)
    (bluesky--remember-author (list :handle "author.test"
                                    :displayName "Author Test"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq read-args args)
                 "Author Test (author.test)")))
      (should (equal (bluesky--read-actor) "author.test")))
    (should (equal (nth 1 read-args)
                   '(("Author Test (author.test)" . "author.test"))))))

(ert-deftest bluesky-read-actor-completes-authors-without-display-names ()
  (let ((bluesky-known-authors nil)
        read-args)
    (bluesky--remember-author (list :handle "author.test"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq read-args args)
                 "author.test")))
      (should (equal (bluesky--read-actor) "author.test")))
    (should (equal (nth 1 read-args)
                   '(("author.test" . "author.test"))))))

(ert-deftest bluesky-read-actor-strips-leading-at-from-completion-input ()
  (let ((bluesky-known-authors nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 "@author.test")))
      (should (equal (bluesky--read-actor) "author.test")))))

(provide 'bluesky-test)

;;; bluesky-test.el ends here
