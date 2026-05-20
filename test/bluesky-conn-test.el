;;; bluesky-conn-test.el --- Tests for Bluesky connection helpers -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

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
