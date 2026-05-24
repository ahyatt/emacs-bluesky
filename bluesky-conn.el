;;; bluesky-conn.el --- Bluesky API connection functions -*- lexical-binding: t; -*-

;; Copyright (c) 2024, 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Assisted-by: ChatGPT:chatgpt-5.5
;; Homepage: https://github.com/ahyatt/emacs-bluesky
;; SPDX-License-Identifier: GPL-3.0-or-later
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

;;; Commentary:
;; This file has various methods for calling into bluesky servers according to
;; the atproto protocol.  We use the atproto objects as is, as json objects that
;; use plists and arrays.

(require 'json)
(require 'cl-lib)
(require 'futur)
(require 'plz)
(require 'seq)
(require 'subr-x)

;;; Code:

(defvar bluesky-session nil
  "The session object for accessing the Bluesky API per host and handle.
This is an alist of host/handle strings and session objects returned by
the Bluesky API.")

(defun bluesky-conn-json-read ()
  "Read JSON from the current buffer and return it in as we expect."
  (let ((json-object-type 'plist))
    (json-read)))

(defun bluesky-conn-json-read-from-string (str)
  "Read JSON from STR and return it as we expect."
  (let ((json-object-type 'plist))
    (json-read-from-string str)))

(define-error 'bluesky-api-error "Bluesky API error")

(defun bluesky-conn--clean-args (args)
  "Return ARGS with nil-valued plist entries removed."
  (let (cleaned)
    (while args
      (let ((key (pop args))
            (value (pop args)))
        (unless (null value)
          (push key cleaned)
          (push (bluesky-conn--clean-value value) cleaned))))
    (nreverse cleaned)))

(defun bluesky-conn--plist-p (value)
  "Return non-nil when VALUE looks like a plist."
  (and (consp value)
       (keywordp (car value))))

(defun bluesky-conn--clean-value (value)
  "Return VALUE with nested nil-valued plist entries removed."
  (cond
   ((vectorp value)
    (vconcat (mapcar #'bluesky-conn--clean-value (append value nil))))
   ((bluesky-conn--plist-p value)
    (bluesky-conn--clean-args value))
   ((consp value)
    (mapcar #'bluesky-conn--clean-value value))
   (t value)))

(defun bluesky-conn--api-error-json (error-object)
  "Return the JSON error payload in ERROR-OBJECT, if present."
  (cadr error-object))

(defun bluesky-conn--parse-plz-error (resp)
  "Parse the Bluesky JSON error payload from RESP."
  (if-let* ((err-resp (plz-error-response resp))
            (body (plz-response-body err-resp)))
      (bluesky-conn-json-read-from-string body)
    (list :error "HTTPError"
          :message (format "No error response found in %S" resp))))

(defun bluesky-conn--query-string (args)
  "Return URL query string for plist ARGS."
  (mapconcat #'identity
             (apply #'append
                    (mapcar (lambda (pair)
                              (bluesky-conn--query-param
                               (car pair)
                               (cadr pair)))
                            (seq-partition args 2)))
             "&"))

(defun bluesky-conn--query-param (key value)
  "Return URL query parameter strings for KEY and VALUE.
Array values are encoded as repeated query parameters, as required by XRPC."
  (let ((key (url-hexify-string
              (substring-no-properties (symbol-name key) 1))))
    (mapcar (lambda (value)
              (format "%s=%s"
                      key
                      (url-hexify-string
                       (bluesky-conn--query-value-string value))))
            (bluesky-conn--query-values value))))

(defun bluesky-conn--query-values (value)
  "Return VALUE as a list of scalar query parameter values."
  (cond
   ((vectorp value) (append value nil))
   ((and (consp value) (not (bluesky-conn--plist-p value))) value)
   (t (list value))))

(defun bluesky-conn--query-value-string (value)
  "Return VALUE encoded as an XRPC query parameter string."
  (cond
   ((eq value t) "true")
   ((eq value :json-false) "false")
   (t (format "%s" value))))

(defun bluesky-conn-call (host http-method method auth-header &rest args)
  "Call METHOD on the Bluesky instance at HOST.

HTTP-METHOD is the HTTP method to use, such as `get' or `post'.

AUTH-HEADER is the value to use for the bearer authorization header; if
nil it is assumed this is a public endpoint.  ARGS is a plist, but any
values that are nil will be ignored.

Return a `futur' that resolves to the JSON response or fails with
`bluesky-api-error'."
  (let* ((args (bluesky-conn--clean-args args))
         (url (format "https://%s/xrpc/%s%s" host method
                      (if (and args (eq 'get http-method))
                          (concat "?" (bluesky-conn--query-string args))
                        "")))
         (headers (append
                   (when auth-header `(("Authorization" .
                                        ,(format "Bearer %s" auth-header))))
                   '(("Content-Type" . "application/json")))))
    (futur-new
     (lambda (futur)
       (condition-case err
           (apply #'plz http-method url
                  :as #'bluesky-conn-json-read
                  :headers headers
                  :then (lambda (resp)
                          (futur-deliver-value futur resp))
                  :else (lambda (resp)
                          (futur-deliver-failure
                           futur
                           (list 'bluesky-api-error
                                 (bluesky-conn--parse-plz-error resp))))
                  (when (and args (eq http-method 'post))
                    (list :body (json-encode args))))
         (plz-error
          (futur-deliver-failure
           futur
           (list 'bluesky-api-error
                 (bluesky-conn--parse-plz-error (nth 2 err)))))
         (error
          (futur-deliver-failure futur err)))
       nil))))

(defun bluesky-conn-call-authed (host handle http-method method &rest args)
  "Call METHOD on the Bluesky instance at HOST using HANDLE.
HTTP-METHOD is the HTTP method to use, such as `get' or `post'.
ARGS is a plist, but any values that are nil will be ignored.

This function assumes that a session has been created, and will handle
auth refreshes.  Return a `futur'."
  (let ((session (alist-get (format "%s/%s" host handle)
                            bluesky-session
                            nil nil #'equal)))
    (unless session
      (error "Unable to get the Bluesky authentication token, you may need to log in first"))
    (futur-bind
     (apply #'bluesky-conn-call host http-method method (plist-get session :accessJwt) args)
     #'futur-done
     (lambda (err)
       (let ((api-error (bluesky-conn--api-error-json err)))
         (if (equal "ExpiredToken" (plist-get api-error :error))
             (futur-let* ((_ <- (bluesky-conn-refresh-session host handle)))
               (apply #'bluesky-conn-call-authed host handle http-method method args))
           (futur-failed err)))))))

(defun bluesky-conn--binary-file-contents (file)
  "Return FILE contents as an unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'binary))
      (insert-file-contents-literally file))
    (buffer-string)))

(defun bluesky-conn--upload-blob-with-token (host access-jwt file mime-type)
  "Upload FILE with MIME-TYPE to HOST using ACCESS-JWT.
Return a future resolving to the `com.atproto.repo.uploadBlob' response."
  (let ((url (format "https://%s/xrpc/com.atproto.repo.uploadBlob" host))
        (headers `(("Authorization" . ,(format "Bearer %s" access-jwt))
                   ("Content-Type" . ,mime-type)))
        (body (bluesky-conn--binary-file-contents file)))
    (futur-new
     (lambda (futur)
       (condition-case err
           (plz 'post url
             :as #'bluesky-conn-json-read
             :headers headers
             :body body
             :then (lambda (resp)
                     (futur-deliver-value futur resp))
             :else (lambda (resp)
                     (futur-deliver-failure
                      futur
                      (list 'bluesky-api-error
                            (bluesky-conn--parse-plz-error resp)))))
         (plz-error
          (futur-deliver-failure
           futur
           (list 'bluesky-api-error
                 (bluesky-conn--parse-plz-error (nth 2 err)))))
         (error
          (futur-deliver-failure futur err)))
       nil))))

(defun bluesky-conn-upload-blob (host handle file mime-type)
  "Upload FILE as MIME-TYPE using HANDLE at HOST.
Return a future resolving to the uploaded blob object."
  (let ((session (alist-get (format "%s/%s" host handle)
                            bluesky-session
                            nil nil #'equal)))
    (unless session
      (error "Unable to get the Bluesky authentication token, you may need to log in first"))
    (futur-bind
     (bluesky-conn--upload-blob-with-token host
                                           (plist-get session :accessJwt)
                                           file
                                           mime-type)
     (lambda (result)
       (plist-get result :blob))
     (lambda (err)
       (let ((api-error (bluesky-conn--api-error-json err)))
         (if (equal "ExpiredToken" (plist-get api-error :error))
             (futur-let* ((_ <- (bluesky-conn-refresh-session host handle)))
               (bluesky-conn-upload-blob host handle file mime-type))
           (futur-failed err)))))))

(defun bluesky-conn-create-session (host handle password)
  "Create a session with the Bluesky API at HOST using HANDLE and PASSWORD.
HANDLE is the user's handle on the Bluesky instance at HOST (without any
leading `@'), and PASSWORD is the user's password, or a function that
takes no arguments that produces it.  This function will store the
session object in `bluesky-session' for future use, and also return it."
  (futur-bind
   (bluesky-conn-call
    host 'post "com.atproto.server.createSession" nil
    :identifier handle :password (if (functionp password)
                                     (funcall password)
                                   password))
   (lambda (result)
     (setf (alist-get (format "%s/%s" host handle)
                      bluesky-session
                      nil nil #'equal)
           result)
     result)))

(defun bluesky-conn-get-session (host handle)
  "Get a session for HANDLE at HOST."
  (alist-get (format "%s/%s" host handle) bluesky-session nil nil #'equal))

(defun bluesky-conn-refresh-session (host handle)
  "Refresh the session for the Bluesky instance at HOST using HANDLE."
  (let ((session (bluesky-conn-get-session host handle)))
    (unless session
      (error "No session found to refresh for host %s" host))
    (futur-bind
     (bluesky-conn-call host 'post "com.atproto.server.refreshSession"
                        (plist-get session :refreshJwt))
     (lambda (result)
       (setf (alist-get (format "%s/%s" host handle)
                        bluesky-session
                        nil nil #'equal)
             result)
       result))))

(defun bluesky-conn-create-post (host handle collection record &optional rkey)
  "Create a post in the Bluesky instance at HOST using HANDLE.
COLLECTION is the collection to post to, and RECORD is the record, created by
`bluesky-conn-record', to post.  RKEY, when non-nil, is the record key to use."
  (bluesky-conn-call-authed
   host handle 'post "com.atproto.repo.createRecord"
   :repo (plist-get (bluesky-conn-get-session host handle) :did)
   :collection collection
   :rkey rkey
   :record record))

(defun bluesky-conn-delete-record (host handle record-uri)
  "Delete RECORD-URI using HANDLE at HOST."
  (pcase-let ((`(,repo ,collection ,rkey)
               (bluesky-conn--at-uri-parts record-uri)))
    (bluesky-conn-call-authed
     host handle 'post "com.atproto.repo.deleteRecord"
     :repo repo
     :collection collection
     :rkey rkey)))

(defun bluesky-conn--at-uri-parts (uri)
  "Return (REPO COLLECTION RKEY) parsed from at:// URI."
  (unless (string-match "\\`at://\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)\\'" uri)
    (error "Unsupported AT URI: %s" uri))
  (list (match-string 1 uri)
        (match-string 2 uri)
        (match-string 3 uri)))

(defun bluesky-conn--subject (post)
  "Return a strong ref subject for POST."
  (list :uri (plist-get post :uri)
        :cid (plist-get post :cid)))

(defun bluesky-conn--record-rkey (record-uri)
  "Return the rkey parsed from RECORD-URI."
  (nth 2 (bluesky-conn--at-uri-parts record-uri)))

(defun bluesky-conn--created-at ()
  "Return the current time as an AT Protocol datetime in UTC."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

(defun bluesky-conn-record (text langs facets &optional reply embed)
  "Return an app.bsky.feed.post record for TEXT.
LANGS and FACETS are optional post metadata.  REPLY is a reply ref plist.
EMBED is an optional app.bsky post embed record."
  (append `(:text ,text :createdAt ,(bluesky-conn--created-at))
          (when langs `(:langs ,langs))
          (when facets `(:facets ,facets))
          (when reply `(:reply ,reply))
          (when embed `(:embed ,embed))))

(defun bluesky-conn-create-like (host handle post)
  "Like POST using HANDLE at HOST."
  (bluesky-conn-create-post
   host handle "app.bsky.feed.like"
   (list :$type "app.bsky.feed.like"
         :subject (bluesky-conn--subject post)
         :createdAt (bluesky-conn--created-at))))

(defun bluesky-conn-create-repost (host handle post)
  "Repost POST using HANDLE at HOST."
  (bluesky-conn-create-post
   host handle "app.bsky.feed.repost"
   (list :$type "app.bsky.feed.repost"
         :subject (bluesky-conn--subject post)
         :createdAt (bluesky-conn--created-at))))

(defun bluesky-conn-create-reply (host handle post text)
  "Reply to POST with TEXT using HANDLE at HOST."
  (let* ((record (plist-get post :record))
         (existing-reply (plist-get record :reply))
         (parent (bluesky-conn--subject post))
         (root (or (plist-get existing-reply :root) parent)))
    (bluesky-conn-create-post
     host handle "app.bsky.feed.post"
     (bluesky-conn-record text nil nil
                          (list :root root :parent parent)))))

(defun bluesky-conn-create-threadgate (host handle post-uri allow)
  "Create a threadgate for POST-URI using HANDLE at HOST.
ALLOW is a vector of app.bsky.feed.threadgate rule records."
  (bluesky-conn-create-post
   host handle "app.bsky.feed.threadgate"
   (list :$type "app.bsky.feed.threadgate"
         :post post-uri
         :allow allow
         :createdAt (bluesky-conn--created-at))
   (bluesky-conn--record-rkey post-uri)))

(defun bluesky-conn-create-postgate (host handle post-uri embedding-rules)
  "Create a postgate for POST-URI using HANDLE at HOST.
EMBEDDING-RULES is a vector of app.bsky.feed.postgate rule records."
  (bluesky-conn-create-post
   host handle "app.bsky.feed.postgate"
   (list :$type "app.bsky.feed.postgate"
         :post post-uri
         :embeddingRules embedding-rules
         :createdAt (bluesky-conn--created-at))
   (bluesky-conn--record-rkey post-uri)))

(defun bluesky-conn-create-bookmark (host handle post)
  "Bookmark POST using HANDLE at HOST."
  (bluesky-conn-call-authed host handle 'post "app.bsky.bookmark.createBookmark"
                            :uri (plist-get post :uri)
                            :cid (plist-get post :cid)))

(defun bluesky-conn-delete-bookmark (host handle post)
  "Delete the bookmark for POST using HANDLE at HOST."
  (bluesky-conn-call-authed host handle 'post "app.bsky.bookmark.deleteBookmark"
                            :uri (plist-get post :uri)))

(defun bluesky-conn--validate-feed-limit (limit)
  "Signal an error unless LIMIT is nil or a valid feed page size."
  (unless (or (null limit) (and (>= limit 1) (<= limit 100)))
    (error "Number of posts to retrieve must be between 1 and 100")))

(defun bluesky-conn-get-timeline (host handle &optional cursor limit)
  "Get the timeline for the user HANDLE at HOST.
The CURSOR defines where to start at, and LIMIT is the number of posts
to return."
  (bluesky-conn--validate-feed-limit limit)
  (bluesky-conn-call-authed host handle 'get "app.bsky.feed.getTimeline"
                            :cursor cursor :limit limit))

(defun bluesky-conn-get-author-feed (host handle actor &optional cursor limit filter include-pins)
  "Get ACTOR's author feed using HANDLE at HOST.
The CURSOR defines where to start, LIMIT is the number of posts to return,
FILTER narrows the feed, and INCLUDE-PINS requests pinned posts."
  (bluesky-conn--validate-feed-limit limit)
  (bluesky-conn-call-authed host handle 'get "app.bsky.feed.getAuthorFeed"
                            :actor actor
                            :cursor cursor
                            :limit limit
                            :filter filter
                            :includePins include-pins))

(defun bluesky-conn-search-posts (host handle query &optional cursor limit sort tags)
  "Search posts for QUERY using HANDLE at HOST.
CURSOR defines where to start, LIMIT is the number of posts to return, SORT is
the ranking order, and TAGS is a vector of tag filters without hash prefixes."
  (bluesky-conn--validate-feed-limit limit)
  (bluesky-conn-call-authed host handle 'get "app.bsky.feed.searchPosts"
                            :q query
                            :cursor cursor
                            :limit limit
                            :sort sort
                            :tag tags))

(defun bluesky-conn-get-feed (host handle feed &optional cursor limit)
  "Get a custom FEED using HANDLE at HOST.
FEED is the feed generator AT URI.  CURSOR defines where to start, and LIMIT is
the number of posts to return."
  (bluesky-conn--validate-feed-limit limit)
  (bluesky-conn-call-authed host handle 'get "app.bsky.feed.getFeed"
                            :feed feed
                            :cursor cursor
                            :limit limit))

(defun bluesky-conn-get-actor-feeds (host handle actor &optional cursor limit)
  "Get feed generators created by ACTOR using HANDLE at HOST.
CURSOR defines where to start, and LIMIT is the number of feeds to return."
  (bluesky-conn--validate-feed-limit limit)
  (bluesky-conn-call-authed host handle 'get "app.bsky.feed.getActorFeeds"
                            :actor actor
                            :cursor cursor
                            :limit limit))

(defun bluesky-conn-get-popular-feed-generators (host handle &optional cursor limit query)
  "Get popular feed generators using HANDLE at HOST.
CURSOR defines where to start, LIMIT is the number of feeds to return, and QUERY
filters the generator list."
  (bluesky-conn--validate-feed-limit limit)
  (bluesky-conn-call-authed host handle 'get "app.bsky.unspecced.getPopularFeedGenerators"
                            :cursor cursor
                            :limit limit
                            :query query))

(defun bluesky-conn-get-post-thread (host handle uri &optional depth parent-height)
  "Get the post thread for URI using HANDLE at HOST.
DEPTH controls how many descendant levels to fetch, and PARENT-HEIGHT controls
how many ancestor levels to fetch."
  (bluesky-conn-call-authed host handle 'get "app.bsky.feed.getPostThread"
                            :uri uri
                            :depth depth
                            :parentHeight parent-height))

(defvar bluesky-conn-cache (make-hash-table :test 'equal)
  "A cache of Bluesky API responses, keyed by URLs.
Anything in here is assumed to be cacheable indefinitely.")

(defun bluesky-conn-get-image-by-url (url)
  "Get an image at URL, using the cache if available."
  (or (gethash url bluesky-conn-cache)
      (puthash url (create-image (plz 'get url :as 'binary) nil 'data) bluesky-conn-cache)))

(defun bluesky-conn-get-blob (host did cid)
  "Get a blob with id CID from account DID on HOST."
  (plz 'get (format "https://%s/xrpc/com.atproto.sync.getBlob?did=%s&cid=%s"
                    host (url-hexify-string did) (url-hexify-string cid))
    :as 'binary))

(defun bluesky-conn-get-image-by-ref (host did cid)
  "Get an image with id CID from account DID on HOST."
  (create-image (bluesky-conn-get-blob host did cid) nil 'data))

(provide 'bluesky-conn)

;;; bluesky-conn.el ends here
