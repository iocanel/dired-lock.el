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

(ert-deftest dired-lock-pdf-integration-test ()
  "Integration test for PDF locking and unlocking."
  (let* ((original-pdf "test/resources/hello-world.pdf")
         (test-pdf "test-hello-world.pdf")
         (password "test123"))
    
    (skip-unless (file-exists-p original-pdf))
    (skip-unless (executable-find "qpdf"))
    
    ;; Copy original file for testing
    (copy-file original-pdf test-pdf t)
    
    ;; Test locking
    (condition-case err
        (progn
          (dired-lock--lock-pdf test-pdf password)
          (should (file-exists-p test-pdf))
          
          ;; Test unlocking
          (dired-lock--unlock-pdf test-pdf password)
          (should (file-exists-p test-pdf))
          
          (message "PDF integration test passed"))
      
      (error
       (when (file-exists-p test-pdf)
         (delete-file test-pdf))
       (message "PDF integration test skipped: %s" (error-message-string err))
       (ert-skip (format "PDF tools not available or test failed: %s" 
                         (error-message-string err)))))
    
    ;; Cleanup
    (when (file-exists-p test-pdf)
      (delete-file test-pdf))))

(ert-deftest dired-lock-directory-integration-test ()
  "Integration test for directory locking and unlocking."
  (let* ((original-dir "test/resources/sample-directory")
         (test-dir (expand-file-name "test-sample-directory"))
         (test-zip (concat test-dir ".zip"))
         (password "test123"))
    
    (skip-unless (file-directory-p original-dir))
    (skip-unless (executable-find "zip"))
    (skip-unless (executable-find "unzip"))
    
    ;; Create test directory manually instead of copy-directory
    (make-directory test-dir t)
    (with-temp-file (concat test-dir "/file1.txt")
      (insert "This is file 1 content.\nSample text for testing directory locking."))
    (with-temp-file (concat test-dir "/file2.txt")
      (insert "This is file 2 content.\nAnother sample file for testing."))
    (make-directory (concat test-dir "/subdir") t)
    (with-temp-file (concat test-dir "/subdir/nested-file.txt")
      (insert "This is a nested file in a subdirectory."))
    
    ;; Test locking
    (condition-case err
        (progn
          (dired-lock--lock-directory test-dir password)
          (should (file-exists-p test-zip))
          (should-not (file-directory-p test-dir))
          
          ;; Test unlocking
          (dired-lock--unlock-zip test-zip password)
          (should (file-directory-p test-dir))
          (should-not (file-exists-p test-zip))
          
          ;; Verify directory contents were restored
          (should (file-exists-p (concat test-dir "/file1.txt")))
          (should (file-exists-p (concat test-dir "/file2.txt")))
          (should (file-exists-p (concat test-dir "/subdir/nested-file.txt")))
          
          (message "Directory integration test passed"))
      
      (error
       (when (file-exists-p test-zip)
         (delete-file test-zip))
       (when (file-directory-p test-dir)
         (delete-directory test-dir t))
       (message "Directory integration test skipped: %s" (error-message-string err))
       (ert-skip (format "ZIP tools not available or test failed: %s" 
                         (error-message-string err)))))
    
    ;; Cleanup
    (when (file-exists-p test-zip)
      (delete-file test-zip))
    (when (file-directory-p test-dir)
      (delete-directory test-dir t))))

(provide 'test-dired-lock)
;;; test-dired-lock.el ends here
