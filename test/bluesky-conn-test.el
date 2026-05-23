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

(ert-deftest bluesky-conn-created-at-uses-utc ()
  (let (args)
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (&rest call-args)
                 (setq args call-args)
                 "timestamp")))
      (should (equal (bluesky-conn--created-at) "timestamp"))
      (should (equal args '("%Y-%m-%dT%H:%M:%SZ" nil t))))))

(provide 'bluesky-conn-test)

;;; bluesky-conn-test.el ends here
