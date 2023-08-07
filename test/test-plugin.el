;;; test-plugin.el --- Test Plugin -*- lexical-binding: t -*-


;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2"))

;;; Commentary:

;;; Code:

(ert-deftest simple-test ()
  "A very simple test."
  (should (eq 2 (+ 1 1))))

(provide 'test-plugin)
;;; test-plugin.el ends here
