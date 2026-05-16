;;; bluesky-conn.el --- Bluesky API connection functions -*- lexical-binding: t; -*-

;; Copyright (c) 2024  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
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
  (flatten-list (funcall #'append
                         (seq-filter (lambda (double)
                                       (not (null (cadr double))))
                                     (seq-partition args 2)))))

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
  (mapconcat (lambda (pair)
               (format "%s=%s"
                       (url-hexify-string
                        (substring-no-properties (symbol-name (car pair)) 1))
                       (url-hexify-string (format "%s" (cadr pair)))))
             (seq-partition args 2)
             "&"))

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
      (error "Unable to get the Bluesky authentication token, you may need to log in first."))
    (futur-bind
     (apply #'bluesky-conn-call host http-method method (plist-get session :accessJwt) args)
     #'futur-done
     (lambda (err)
       (let ((api-error (bluesky-conn--api-error-json err)))
         (if (equal "ExpiredToken" (plist-get api-error :error))
             (futur-let* ((_ <- (bluesky-conn-refresh-session host handle)))
               (apply #'bluesky-conn-call-authed host handle http-method method args))
           (futur-failed err)))))))

(defun bluesky-conn-create-session (host handle password)
  "Create a session with the Bluesky API at HOST using HANDLE and PASSWORD.
HANDLE is the user's handle on the Bluesky instance at HOST (without any
leading `@'), and PASSWORD is the user's password, or a function that
takes no arguments that produces it. This function will store the
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

(defun bluesky-conn-create-post (host handle collection record)
  "Create a post in the Bluesky instance at HOST using HANDLE.
COLLECTION is the collection to post to, and RECORD is the record, created by
`bluesky-conn-record', to post."
  (bluesky-conn-call-authed
   host handle 'post "com.atproto.repo.createRecord"
   :repo (plist-get (bluesky-conn-get-session host handle) :did)
   :collection collection
   :record record))

(defun bluesky-conn-record (text langs facets)
  (append `(:text ,text :createdAt ,(format-time-string "%Y-%m-%dT%H:%M:%SZ"))
          (when langs `(:langs ,langs))
          (when facets `(:facets ,facets))))

(defun bluesky-conn-get-timeline (host handle &optional cursor limit)
  "Get the timeline for the user HANDLE at HOST.
The CURSOR defines where to start at, and LIMIT is the number of posts
to return."
  (unless (or (null limit) (and (> limit 0) (< limit 100)))
    (error "Number of posts to retrieve must be between 0 and 100"))
  (bluesky-conn-call-authed host handle 'get "app.bsky.feed.getTimeline"
                            :cursor cursor :limit limit))

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
  "Get a blob with id CID from account DID."
  (plz 'get (format "https://%s/xrpc/com.atproto.sync.getBlob?did=%s&cid=%s"
                    host (url-hexify-string did) (url-hexify-string cid))
    :as 'binary))

(defun bluesky-conn-get-image-by-ref (host did cid)
  "Get an image with id CID from account DID."
  (create-image (bluesky-conn-get-blob host did cid) nil 'data))

(provide 'bluesky-conn)

;;; bluesky-conn.el ends here
