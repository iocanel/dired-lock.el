;;; test-dired-lock.el --- Test dired-lock -*- lexical-binding: t -*-

;; Author: Ioannis Canellos
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;;; Code:

(require 'ert)
(require 'dired-lock)

(ert-deftest dired-lock-substitute-command-test ()
  "Test command pattern substitution."
  (should (string= (dired-lock--substitute-command "zip -P %p %o %i" 
                                                    :password "secret" 
                                                    :input "dir" 
                                                    :output "archive.zip")
                   "zip -P secret archive.zip dir")))

(ert-deftest dired-lock-is-pdf-test ()
  "Test PDF file detection."
  (with-temp-file "test.pdf" (insert "dummy"))
  (with-temp-file "test.txt" (insert "dummy"))
  (should (dired-lock--is-pdf-p "test.pdf"))
  (should-not (dired-lock--is-pdf-p "test.txt"))
  (should-not (dired-lock--is-pdf-p "test"))
  (delete-file "test.pdf")
  (delete-file "test.txt"))

(ert-deftest dired-lock-is-zip-test ()
  "Test ZIP file detection."
  (with-temp-file "test.zip" (insert "dummy"))
  (with-temp-file "test.txt" (insert "dummy"))
  (should (dired-lock--is-locked-zip-p "test.zip"))
  (should-not (dired-lock--is-locked-zip-p "test.txt"))
  (should-not (dired-lock--is-locked-zip-p "test"))
  (delete-file "test.zip")
  (delete-file "test.txt"))

(provide 'test-dired-lock)
;;; test-dired-lock.el ends here
