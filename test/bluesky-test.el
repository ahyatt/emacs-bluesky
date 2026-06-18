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
               (lambda (callback &optional username password host discard-result)
                 (setq with-session-called
                       (list username password host discard-result))
                 (funcall callback
                          "https://bsky.social"
                          (list :handle "user.test"))))
              ((symbol-function 'bluesky-post-compose)
               (lambda (&rest args)
                 (setq compose-args args))))
      (with-temp-buffer
        (bluesky-compose-post "user.test" "password" "https://bsky.social")))
    (should (equal with-session-called
                   '("user.test" "password" "https://bsky.social" t)))
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

(ert-deftest bluesky-with-session-discards-ui-callback-result ()
  (let ((bluesky-feed-session (list :handle "user.test"))
        (bluesky-host "https://bsky.social"))
    (should-not
     (bluesky--with-session
      (lambda (_host _session)
        'mounted-ui)
      nil nil nil t))
    (should
     (eq (bluesky--with-session
          (lambda (_host _session)
            'created-future))
         'created-future))))

(ert-deftest bluesky-post-async-authenticates-and-submits-text ()
  (let (with-session-called submit-args)
    (cl-letf (((symbol-function 'bluesky--with-session)
               (lambda (callback &optional username password host discard-result)
                 (setq with-session-called
                       (list username password host discard-result))
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
                   '("user.test" "password" "https://bsky.social" nil)))
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
                     bluesky-toggle-thread-fold
                     bluesky-open-thread
                     bluesky-activate-or-open-thread))
    (should (equal (command-modes command) '(bluesky-mode)))))

(ert-deftest bluesky-navigation-override-map-takes-emulation-precedence ()
  (let* ((other-map (let ((map (make-sparse-keymap)))
                      (define-key map (kbd "j") #'ignore)
                      map))
         (emulation-mode-map-alists
          `(((other-mode . ,other-map))
            symbol-backed-emulation-alist
            ((bluesky--navigation-override-mode
              . ,bluesky--navigation-override-map)))))
    (bluesky--install-navigation-override-map)
    (should (eq (caaar emulation-mode-map-alists)
                'bluesky--navigation-override-mode))
    (should (eq (lookup-key (cdaar emulation-mode-map-alists) (kbd "j"))
                #'bluesky-feed-next-post))
    (should (memq 'symbol-backed-emulation-alist
                  emulation-mode-map-alists))))

(ert-deftest bluesky-feed-navigation-does-not-rerender ()
  (with-temp-buffer
    (let ((items '((:id "one") (:id "two")))
          (bluesky--selected-id "one")
          set-state-called)
      (insert (propertize "one\n" 'bluesky-item-id "one"))
      (insert (propertize "two\n" 'bluesky-item-id "two"))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'bluesky--timeline-state)
                 (lambda (key)
                   (pcase key
                     (:items items)
                     (:selected-id bluesky--selected-id))))
                ((symbol-function 'bluesky--set-timeline-state)
                 (lambda (&rest _args)
                   (setq set-state-called t))))
        (bluesky--move-selection 1)
        (should (equal bluesky--selected-id "two"))
        (should-not set-state-called)))))

(defun bluesky-test--insert-fold-block (item-id depth text &optional block-id)
  "Insert foldable TEXT for ITEM-ID at DEPTH and return its start."
  (let ((start (point)))
    (insert (propertize (concat text "\n")
                        'bluesky-item-id item-id
                        'bluesky-thread-depth depth
                        'bluesky-thread-block-id (or block-id item-id)))
    start))

(ert-deftest bluesky-toggle-thread-fold-hides-rendered-descendants ()
  (with-temp-buffer
    (bluesky-mode)
    (let* ((items '((:id "root" :depth 0 :render t)
                    (:id "quote" :depth 1 :render nil)
                    (:id "child" :depth 1 :render t)
                    (:id "grandchild" :depth 2 :render t)
                    (:id "sibling" :depth 0 :render t)))
           (root-pos (bluesky-test--insert-fold-block "root" 0 "root"))
           (quote-pos (bluesky-test--insert-fold-block "quote" 1 "quote" "root"))
           (child-pos (bluesky-test--insert-fold-block "child" 1 "child"))
           (grandchild-pos
            (bluesky-test--insert-fold-block "grandchild" 2 "grandchild"))
           (sibling-pos (bluesky-test--insert-fold-block "sibling" 0 "sibling")))
      (setq-local bluesky-feed-root
                  (vui-instance--create
                   :state (list :items items :selected-id "root")))
      (setq-local bluesky--selected-id "root")
      (goto-char root-pos)
      (bluesky-toggle-thread-fold)
      (should (member "root" bluesky-thread-folded-item-ids))
      (should-not (get-char-property quote-pos 'invisible))
      (should (eq (get-char-property child-pos 'invisible)
                  'bluesky-thread-fold))
      (should (eq (get-char-property grandchild-pos 'invisible)
                  'bluesky-thread-fold))
      (should-not (get-char-property sibling-pos 'invisible)))))

(ert-deftest bluesky-toggle-thread-fold-unfolds-rendered-descendants ()
  (with-temp-buffer
    (bluesky-mode)
    (let* ((items '((:id "root" :depth 0 :render t)
                    (:id "child" :depth 1 :render t)))
           (_root-pos (bluesky-test--insert-fold-block "root" 0 "root"))
           (child-pos (bluesky-test--insert-fold-block "child" 1 "child")))
      (setq-local bluesky-feed-root
                  (vui-instance--create
                   :state (list :items items :selected-id "root")))
      (setq-local bluesky--selected-id "root")
      (bluesky-toggle-thread-fold)
      (should (get-char-property child-pos 'invisible))
      (bluesky-toggle-thread-fold)
      (should-not bluesky-thread-folded-item-ids)
      (should-not (get-char-property child-pos 'invisible)))))

(ert-deftest bluesky-feed-navigation-skips-folded-descendants ()
  (with-temp-buffer
    (bluesky-mode)
    (let* ((items '((:id "root" :depth 0 :render t)
                    (:id "child" :depth 1 :render t)
                    (:id "grandchild" :depth 2 :render t)
                    (:id "sibling" :depth 0 :render t)))
           (root-pos (bluesky-test--insert-fold-block "root" 0 "root")))
      (bluesky-test--insert-fold-block "child" 1 "child")
      (bluesky-test--insert-fold-block "grandchild" 2 "grandchild")
      (bluesky-test--insert-fold-block "sibling" 0 "sibling")
      (setq-local bluesky-feed-root
                  (vui-instance--create
                   :state (list :items items :selected-id "root")))
      (setq-local bluesky--selected-id "root")
      (goto-char root-pos)
      (bluesky-toggle-thread-fold)
      (bluesky-feed-next-post)
      (should (equal bluesky--selected-id "sibling")))))

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

(ert-deftest bluesky-post-action-updates-rendered-stats-without-rerender ()
  (let* ((post (list :uri "at://did/post/1"
                     :replyCount 0
                     :repostCount 0
                     :quoteCount 0
                     :likeCount 2
                     :viewer nil))
         (old-stats (bluesky-ui--stats-text post))
         (root (vui-instance--create
                :state (list :posts (list post)
                             :items (list (list :id "item-1"
                                                :post post
                                                :render t)))))
         set-state-called
         rerender-called)
    (with-temp-buffer
      (setq-local bluesky-feed-root root)
      (insert (propertize old-stats
                          'bluesky-item-id "item-1"
                          'face 'bluesky-post-stats))
      (cl-letf (((symbol-function 'vui-set-state)
                 (lambda (&rest _args)
                   (setq set-state-called t)))
                ((symbol-function 'vui-rerender)
                 (lambda (&rest _args)
                   (setq rerender-called t))))
        (funcall (bluesky--local-post-action-callback post 'like t)
                 (list :uri "at://did/like/1")))
      (should-not set-state-called)
      (should-not rerender-called)
      (should-not (search-forward old-stats nil t))
      (goto-char (point-min))
      (should (search-forward
               "0 replies  |  0 reposts  |  0 quotes  |  3 likes  |  liked"
               nil t))
      (let* ((state (vui-instance-state root))
             (updated-post (car (plist-get state :posts)))
             (updated-item-post (plist-get (car (plist-get state :items)) :post)))
        (should (equal (plist-get updated-post :likeCount) 3))
        (should (equal (plist-get (plist-get updated-post :viewer) :like)
                       "at://did/like/1"))
        (should (equal (plist-get updated-item-post :likeCount) 3))))))

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

(defun bluesky-test--reply-post-view (uri root-uri parent-uri)
  "Return a minimal reply post view for URI."
  (let ((post (bluesky-test--post-view uri)))
    (plist-put
     post :record
     (plist-put (plist-get post :record)
                :reply
                (list :root (list :uri root-uri :cid (concat root-uri "-cid"))
                      :parent (list :uri parent-uri
                                    :cid (concat parent-uri "-cid")))))))

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

(ert-deftest bluesky-timeline-response-hides-record-level-replies ()
  (let* ((bluesky-timeline-reply-display 'hide)
         (reply (bluesky-test--reply-post-view
                 "at://did/reply/post"
                 "at://did/root/post"
                 "at://did/parent/post"))
         (normal (bluesky-test--post-view "at://did/normal/post"))
         (response (list :feed
                         (vector
                          (list :post reply)
                          (list :post normal)))))
    (should (equal (mapcar #'bluesky--post-uri
                           (bluesky--timeline-response-posts response))
                   '("at://did/normal/post")))))

(ert-deftest bluesky-timeline-response-does-not-top-level-missing-context-replies ()
  (let* ((bluesky-timeline-reply-display 'context)
         (reply (bluesky-test--reply-post-view
                 "at://did/reply/post"
                 "at://did/root/post"
                 "at://did/parent/post"))
         (response (list :feed (vector (list :post reply)))))
    (should-not (bluesky--timeline-response-posts response))))

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

(ert-deftest bluesky-timeline-response-deduplicates-context-root-after-standalone ()
  (let* ((bluesky-timeline-reply-display 'context)
         (root (bluesky-test--post-view "at://did/root/post"))
         (reply (bluesky-test--reply-post-view
                 "at://did/reply/post"
                 (bluesky--post-uri root)
                 (bluesky--post-uri root)))
         (response (list :feed
                         (vector
                          (list :post root)
                          (list :post reply
                                :reply (list :root root :parent root))))))
    (should (equal (mapcar #'bluesky--post-uri
                           (bluesky--timeline-response-posts response))
                   '("at://did/root/post"
                     "at://did/reply/post")))
    (should (equal (mapcar #'bluesky--feed-post-depth
                           (bluesky--timeline-response-posts response))
                   '(0 1)))))

(ert-deftest bluesky-timeline-response-attaches-context-to-earlier-ancestor ()
  (let* ((bluesky-timeline-reply-display 'context)
         (root (bluesky-test--post-view "at://did/root/post"))
         (unrelated (bluesky-test--post-view "at://did/unrelated/post"))
         (parent (bluesky-test--reply-post-view
                  "at://did/parent/post"
                  (bluesky--post-uri root)
                  (bluesky--post-uri root)))
         (child (bluesky-test--reply-post-view
                 "at://did/child/post"
                 (bluesky--post-uri root)
                 (bluesky--post-uri parent)))
         (grandchild (bluesky-test--reply-post-view
                      "at://did/grandchild/post"
                      (bluesky--post-uri root)
                      (bluesky--post-uri child)))
         (response (list :feed
                         (vector
                          (list :post root)
                          (list :post unrelated)
                          (list :post grandchild
                                :reply (list :root root :parent child))
                          (list :post parent
                                :reply (list :root root :parent root))))))
    (should (equal (mapcar #'bluesky--post-uri
                           (bluesky--timeline-response-posts response))
                   '("at://did/root/post"
                     "at://did/parent/post"
                     "at://did/child/post"
                     "at://did/grandchild/post"
                     "at://did/unrelated/post")))
    (should (equal (mapcar #'bluesky--feed-post-depth
                           (bluesky--timeline-response-posts response))
                   '(0 1 2 3 0)))))

(ert-deftest bluesky-append-unique-posts-deduplicates-loaded-pages ()
  (let ((first (bluesky-test--post-view "at://did/first/post"))
        (second (bluesky-test--post-view "at://did/second/post")))
    (should (equal (mapcar #'bluesky--post-uri
                           (bluesky--append-unique-posts
                            (list first)
                            (list first second)))
                   '("at://did/first/post"
                     "at://did/second/post")))))

(ert-deftest bluesky-append-unique-posts-marks-only-latest-additions-new ()
  (let* ((first (bluesky--post-with-new-marker
                 (bluesky-test--post-view "at://did/first/post")
                 t))
         (second (bluesky-test--post-view "at://did/second/post"))
         (third (bluesky-test--post-view "at://did/third/post"))
         (posts (bluesky--append-unique-posts
                 (list first second)
                 (list second third))))
    (should (equal (mapcar #'bluesky--post-uri posts)
                   '("at://did/first/post"
                     "at://did/second/post"
                     "at://did/third/post")))
    (should (equal (mapcar #'bluesky--feed-post-new-p posts)
                   '(nil nil t)))))

(ert-deftest bluesky-replace-posts-mark-new-skips-initial-load ()
  (let* ((first (bluesky-test--post-view "at://did/first/post"))
         (posts (bluesky--replace-posts-mark-new nil (list first))))
    (should (equal (mapcar #'bluesky--post-uri posts)
                   '("at://did/first/post")))
    (should (equal (mapcar #'bluesky--feed-post-new-p posts)
                   '(nil)))))

(ert-deftest bluesky-replace-posts-mark-new-compares-with-old-posts ()
  (let* ((first (bluesky-test--post-view "at://did/first/post"))
         (second (bluesky-test--post-view "at://did/second/post"))
         (posts (bluesky--replace-posts-mark-new
                 (list first)
                 (list first second))))
    (should (equal (mapcar #'bluesky--feed-post-new-p posts)
                   '(nil t)))))

(ert-deftest bluesky-append-unique-posts-deduplicates-context-roots ()
  (let* ((root (bluesky-test--post-view "at://did/root/post"))
         (parent (bluesky--post-with-context-parent
                  (bluesky-test--post-view "at://did/parent/post")
                  (bluesky--post-uri root)))
         (reply (bluesky--post-with-context-parent
                 (bluesky-test--post-view "at://did/reply/post")
                 (bluesky--post-uri parent)))
         (posts (bluesky--append-unique-posts
                 (list root parent)
                 (list root parent reply))))
    (should (equal (mapcar #'bluesky--post-uri posts)
                   '("at://did/root/post"
                     "at://did/parent/post"
                     "at://did/reply/post")))
    (should (equal (mapcar #'bluesky--feed-post-depth posts)
                   '(0 1 2)))))

(ert-deftest bluesky-timeline-response-resolves-record-level-reply-context ()
  (let* ((bluesky-timeline-reply-display 'context)
         (root (bluesky-test--post-view "at://did/root/post"))
         (parent (bluesky-test--post-view "at://did/parent/post"))
         (reply (bluesky-test--reply-post-view
                 "at://did/reply/post"
                 (bluesky--post-uri root)
                 (bluesky--post-uri parent)))
         (response (list :feed (vector (list :post reply))))
         requested-uri)
    (cl-letf (((symbol-function 'bluesky-conn-get-post-thread)
               (lambda (_host _handle uri depth parent-height)
                 (setq requested-uri uri)
                 (should (= depth 0))
                 (should (= parent-height 20))
                 (futur-done
                  (list :thread
                        (list :post reply
                              :parent (list :post parent
                                            :parent (list :post root))))))))
      (let* ((resolved
              (futur-blocking-wait-to-get-result
               (bluesky--timeline-response-resolve-reply-context
                "bsky.social" "user.test" response)))
             (posts (bluesky--timeline-response-posts resolved)))
        (should (equal requested-uri (bluesky--post-uri reply)))
        (should (equal (mapcar #'bluesky--post-uri posts)
                       '("at://did/root/post"
                         "at://did/parent/post"
                         "at://did/reply/post")))
        (should (equal (mapcar #'bluesky--feed-post-depth posts)
                       '(0 1 2)))))))

(ert-deftest bluesky-timeline-response-resolves-incomplete-app-view-context ()
  (let* ((bluesky-timeline-reply-display 'context)
         (root (bluesky-test--post-view "at://did/root/post"))
         (parent (bluesky-test--post-view "at://did/parent/post"))
         (reply (bluesky-test--reply-post-view
                 "at://did/reply/post"
                 (bluesky--post-uri root)
                 (bluesky--post-uri parent)))
         (response (list :feed
                         (vector
                          (list :post reply
                                :reply (list :root root)))))
         fetched)
    (cl-letf (((symbol-function 'bluesky-conn-get-post-thread)
               (lambda (&rest _args)
                 (setq fetched t)
                 (futur-done
                  (list :thread
                        (list :post reply
                              :parent (list :post parent
                                            :parent (list :post root))))))))
      (let* ((resolved
              (futur-blocking-wait-to-get-result
               (bluesky--timeline-response-resolve-reply-context
                "bsky.social" "user.test" response)))
             (posts (bluesky--timeline-response-posts resolved)))
        (should fetched)
        (should (equal (mapcar #'bluesky--post-uri posts)
                       '("at://did/root/post"
                         "at://did/parent/post"
                         "at://did/reply/post")))))))

(ert-deftest bluesky-timeline-response-resolves-context-that-skips-ancestors ()
  (let* ((bluesky-timeline-reply-display 'context)
         (root (bluesky-test--post-view "at://did/root/post"))
         (parent (bluesky-test--reply-post-view
                  "at://did/parent/post"
                  (bluesky--post-uri root)
                  (bluesky--post-uri root)))
         (child (bluesky-test--reply-post-view
                 "at://did/child/post"
                 (bluesky--post-uri root)
                 (bluesky--post-uri parent)))
         (reply (bluesky-test--reply-post-view
                 "at://did/reply/post"
                 (bluesky--post-uri root)
                 (bluesky--post-uri child)))
         (response (list :feed
                         (vector
                          (list :post reply
                                :reply (list :root root :parent child)))))
         fetched)
    (cl-letf (((symbol-function 'bluesky-conn-get-post-thread)
               (lambda (_host _handle uri depth parent-height)
                 (setq fetched uri)
                 (should (= depth 0))
                 (should (= parent-height 20))
                 (futur-done
                  (list :thread
                        (list :post reply
                              :parent (list :post child
                                            :parent (list :post parent
                                                          :parent (list :post root)))))))))
      (let* ((resolved
              (futur-blocking-wait-to-get-result
               (bluesky--timeline-response-resolve-reply-context
                "bsky.social" "user.test" response)))
             (posts (bluesky--timeline-response-posts resolved)))
        (should (equal fetched (bluesky--post-uri reply)))
        (should (equal (mapcar #'bluesky--post-uri posts)
                       '("at://did/root/post"
                         "at://did/parent/post"
                         "at://did/child/post"
                         "at://did/reply/post")))
        (should (equal (mapcar #'bluesky--feed-post-depth posts)
                       '(0 1 2 3)))))))

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

(ert-deftest bluesky-timeline-response-deduplicates-repeated-context-roots ()
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
                   '(0 1 2 2)))))

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
