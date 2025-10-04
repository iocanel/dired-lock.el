;;; run-tests.el --- Run Tests -*- lexical-binding: t -*-


;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "27.2"))

;;; Commentary:

;;; Code:
(defvar root-test-path (file-name-directory (file-truename load-file-name)) "The path where the tests are located.")
(defvar root-code-path (file-name-parent-directory root-test-path) "The path where teh code is located.")
(defvar root-sandbox-path (make-temp-file "emacs-plugin-template-sandbox" t) "The temporary path / sandbox.")

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory root-sandbox-path))
     (when (file-directory-p root-sandbox-path)
       (dired-delete-file root-sandbox-path t))
     (make-directory root-sandbox-path t)
     ,@body))

(ert-deftest test-infra ()
  "Ensure that the test infrastructure is working as expected."
  (with-sandbox
   (should-not (eq default-directory root-test-path))
   (should-not (eq default-directory root-code-path))))

(add-to-list 'load-path root-code-path)
(load "test/test-dired-lock.el")

(provide 'run-tests)
;;; run-tests.el ends here
