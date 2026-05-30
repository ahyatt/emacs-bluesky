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

(ert-deftest bluesky-post-async-authenticates-and-submits-text ()
  (let (with-session-called submit-args)
    (cl-letf (((symbol-function 'bluesky--with-session)
               (lambda (callback &optional username password host)
                 (setq with-session-called (list username password host))
                 (funcall callback
                          "https://bsky.social"
                          (list :handle "user.test"))))
              ((symbol-function 'bluesky-post-submit-text)
               (lambda (&rest args)
                 (setq submit-args args)
                 'created-future)))
      (should (eq (bluesky-post-async "hello"
                                      :username "user.test"
                                      :password "password"
                                      :host "https://bsky.social"
                                      :source-format 'markdown
                                      :reply-policy 'followers
                                      :allow-embedding nil)
                  'created-future)))
    (should (equal with-session-called
                   '("user.test" "password" "https://bsky.social")))
    (should (equal (car submit-args) "hello"))
    (should (equal (plist-get (cdr submit-args) :host) "https://bsky.social"))
    (should (equal (plist-get (cdr submit-args) :session)
                   '(:handle "user.test")))
    (should (eq (plist-get (cdr submit-args) :source-format) 'markdown))
    (should (eq (plist-get (cdr submit-args) :reply-policy) 'followers))
    (should-not (plist-get (cdr submit-args) :allow-embedding))))

(ert-deftest bluesky-post-blocks-for-created-post ()
  (let (async-args)
    (cl-letf (((symbol-function 'bluesky-post-async)
               (lambda (&rest args)
                 (setq async-args args)
                 (futur-done (list :uri "at://did/app.bsky.feed.post/rkey"
                                   :cid "cid")))))
      (should
       (equal
        (bluesky-post "hello" :username "user.test")
        (list :uri "at://did/app.bsky.feed.post/rkey"
              :cid "cid"))))
    (should (equal (car async-args) "hello"))
    (should (equal (plist-get (cdr async-args) :username) "user.test"))))

(ert-deftest bluesky-post-async-calls-callback ()
  (let (callback-value)
    (cl-letf (((symbol-function 'bluesky--with-session)
               (lambda (callback &rest _args)
                 (funcall callback
                          "https://bsky.social"
                          (list :handle "user.test"))))
              ((symbol-function 'bluesky-post-submit-text)
               (lambda (&rest _args)
                 (futur-done (list :uri "at://did/app.bsky.feed.post/rkey"
                                   :cid "cid")))))
      (should
       (equal
        (futur-blocking-wait-to-get-result
         (bluesky-post-async
          "hello"
          :callback (lambda (created)
                      (setq callback-value created)
                      (plist-get created :uri))))
        "at://did/app.bsky.feed.post/rkey")))
    (should (equal callback-value
                   (list :uri "at://did/app.bsky.feed.post/rkey"
                         :cid "cid")))))

(ert-deftest bluesky-post-fresh-session-returns-created-post ()
  (let (create-session-args create-post-args)
    (cl-letf (((symbol-function 'bluesky-conn-create-session)
               (lambda (&rest args)
                 (setq create-session-args args)
                 (futur-done (list :handle "user.test"
                                   :did "did:plc:user"))))
              ((symbol-function 'bluesky-conn-get-session)
               (lambda (&rest _args) nil))
              ((symbol-function 'auth-source-search)
               (lambda (&rest _args) nil))
              ((symbol-function 'bluesky-conn-create-post)
               (lambda (&rest args)
                 (setq create-post-args args)
                 (futur-done (list :uri "at://did/app.bsky.feed.post/rkey"
                                   :cid "cid")))))
      (should
       (equal
        (bluesky-post "hello"
                      :username "user.test"
                      :password "password"
                      :host "bsky.social")
        (list :uri "at://did/app.bsky.feed.post/rkey"
              :cid "cid"))))
    (should (equal create-session-args
                   '("bsky.social" "user.test" "password")))
    (should (equal (nth 0 create-post-args) "bsky.social"))
    (should (equal (nth 1 create-post-args) "user.test"))))

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

(ert-deftest bluesky-post-action-update-adjusts-viewer-and-counts ()
  (let* ((post (list :uri "at://did/post/1"
                     :likeCount 2
                     :repostCount 3
                     :viewer (list :like nil
                                   :repost nil
                                   :bookmarked :json-false)))
         (liked (bluesky--post-with-updated-action
                 post 'like t "at://did/like/1"))
         (unliked (bluesky--post-with-updated-action
                   liked 'like nil nil))
         (reposted (bluesky--post-with-updated-action
                    post 'repost t "at://did/repost/1"))
         (bookmarked (bluesky--post-with-updated-action
                      post 'bookmark t nil)))
    (should (equal (plist-get liked :likeCount) 3))
    (should (equal (plist-get (plist-get liked :viewer) :like)
                   "at://did/like/1"))
    (should (equal (plist-get unliked :likeCount) 2))
    (should-not (plist-get (plist-get unliked :viewer) :like))
    (should (equal (plist-get reposted :repostCount) 4))
    (should (equal (plist-get (plist-get reposted :viewer) :repost)
                   "at://did/repost/1"))
    (should (eq (plist-get (plist-get bookmarked :viewer) :bookmarked) t))))

(ert-deftest bluesky-post-action-update-finds-quoted-posts ()
  (let* ((quoted (list :uri "at://did/post/quoted"
                      :cid "quoted-cid"
                      :author (list :did "did:quoted")
                      :value (list :text "quoted")
                      :likeCount 4
                      :viewer nil))
         (post (list :uri "at://did/post/parent"
                     :cid "parent-cid"
                     :author (list :did "did:parent")
                     :record (list :text "parent")
                     :embed (list :record quoted)))
         (updated (bluesky--update-post-view
                   post
                   "at://did/post/quoted"
                   (lambda (current-post)
                     (bluesky--post-with-updated-action
                      current-post 'like t "at://did/like/quoted"))))
         (updated-quote (plist-get (plist-get updated :embed) :record)))
    (should (equal (plist-get updated-quote :likeCount) 5))
    (should (equal (plist-get (plist-get updated-quote :viewer) :like)
                   "at://did/like/quoted"))))

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

(defun bluesky-test--post-view (uri)
  "Return a minimal post view for URI."
  (list :uri uri
        :cid (concat uri "-cid")
        :author (list :did "did:plc:author")
        :record (list :text uri
                      :createdAt "2026-05-25T00:00:00Z")))

(ert-deftest bluesky-timeline-response-hides-replies-when-configured ()
  (let* ((bluesky-timeline-reply-display 'hide)
         (root (bluesky-test--post-view "at://did/root/post"))
         (reply (bluesky-test--post-view "at://did/reply/post"))
         (normal (bluesky-test--post-view "at://did/normal/post"))
         (response (list :feed
                         (vector
                          (list :post reply
                                :reply (list :root root :parent root))
                          (list :post normal)))))
    (should (equal (mapcar #'bluesky--post-uri
                           (bluesky--timeline-response-posts response))
                   '("at://did/normal/post")))))

(ert-deftest bluesky-timeline-response-deduplicates-repeated-feed-posts ()
  (let* ((bluesky-timeline-reply-display 'context)
         (post (bluesky-test--post-view "at://did/repeated/post"))
         (response (list :feed
                         (vector
                          (list :post post)
                          (list :post post)
                          (list :post post)))))
    (should (equal (mapcar #'bluesky--post-uri
                           (bluesky--timeline-response-posts response))
                   '("at://did/repeated/post")))))

(ert-deftest bluesky-append-unique-posts-deduplicates-loaded-pages ()
  (let ((first (bluesky-test--post-view "at://did/first/post"))
        (second (bluesky-test--post-view "at://did/second/post")))
    (should (equal (mapcar #'bluesky--post-uri
                           (bluesky--append-unique-posts
                            (list first)
                            (list first second)))
                   '("at://did/first/post"
                     "at://did/second/post")))))

(ert-deftest bluesky-append-unique-posts-recomputes-depths-after-skips ()
  (let* ((root (bluesky-test--post-view "at://did/root/post"))
         (parent (bluesky-test--post-view "at://did/parent/post"))
         (reply (bluesky-test--post-view "at://did/reply/post"))
         (posts (bluesky--append-unique-posts
                 (list (bluesky--post-with-timeline-depth root 0)
                       (bluesky--post-with-timeline-depth parent 1))
                 (list (bluesky--post-with-timeline-depth root 0)
                       (bluesky--post-with-timeline-depth parent 1)
                       (bluesky--post-with-timeline-depth reply 2)))))
    (should (equal (mapcar #'bluesky--post-uri posts)
                   '("at://did/root/post"
                     "at://did/parent/post"
                     "at://did/reply/post")))
    (should (equal (mapcar #'bluesky--feed-post-depth posts)
                   '(0 1 0)))))

(ert-deftest bluesky-timeline-response-renders-available-reply-context ()
  (let* ((bluesky-timeline-reply-display 'context)
         (root (bluesky-test--post-view "at://did/root/post"))
         (parent (bluesky-test--post-view "at://did/parent/post"))
         (reply (bluesky-test--post-view "at://did/reply/post"))
         (response (list :feed
                         (vector
                          (list :post reply
                                :reply (list :root root :parent parent))))))
    (should (equal (mapcar #'bluesky--post-uri
                           (bluesky--timeline-response-posts response))
                   '("at://did/root/post"
                     "at://did/parent/post"
                     "at://did/reply/post")))))

(ert-deftest bluesky-timeline-response-renders-reply-context-as-chain ()
  (let* ((bluesky-timeline-reply-display 'context)
         (root (bluesky-test--post-view "at://did/root/post"))
         (parent (bluesky-test--post-view "at://did/parent/post"))
         (reply (bluesky-test--post-view "at://did/reply/post"))
         (response (list :feed
                         (vector
                          (list :post reply
                                :reply (list :root root :parent parent)))))
         (items (bluesky--flatten-feed-posts
                 (bluesky--timeline-response-posts response))))
    (should (equal (mapcar (lambda (item) (plist-get item :depth)) items)
                   '(0 1 2)))))

(ert-deftest bluesky-timeline-response-recomputes-depths-after-context-dedup ()
  (let* ((bluesky-timeline-reply-display 'context)
         (root (bluesky-test--post-view "at://did/root/post"))
         (parent (bluesky-test--post-view "at://did/parent/post"))
         (first-reply (bluesky-test--post-view "at://did/first-reply/post"))
         (second-reply (bluesky-test--post-view "at://did/second-reply/post"))
         (response (list :feed
                         (vector
                          (list :post first-reply
                                :reply (list :root root :parent parent))
                          (list :post second-reply
                                :reply (list :root root :parent parent)))))
         (posts (bluesky--timeline-response-posts response)))
    (should (equal (mapcar #'bluesky--post-uri posts)
                   '("at://did/root/post"
                     "at://did/parent/post"
                     "at://did/first-reply/post"
                     "at://did/second-reply/post")))
    (should (equal (mapcar #'bluesky--feed-post-depth posts)
                   '(0 1 2 0)))))

(ert-deftest bluesky-timeline-response-deduplicates-root-parent-reply-context ()
  (let* ((bluesky-timeline-reply-display 'context)
         (root (bluesky-test--post-view "at://did/root/post"))
         (reply (bluesky-test--post-view "at://did/reply/post"))
         (response (list :feed
                         (vector
                          (list :post reply
                                :reply (list :root root :parent root))))))
    (should (equal (mapcar #'bluesky--post-uri
                           (bluesky--timeline-response-posts response))
                   '("at://did/root/post"
                     "at://did/reply/post")))))

(provide 'bluesky-test)

;;; bluesky-test.el ends here
