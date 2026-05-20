;;; bluesky-ui-test.el --- Tests for Bluesky UI helpers -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(require 'bluesky-ui)

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

(provide 'bluesky-ui-test)

;;; bluesky-ui-test.el ends here
