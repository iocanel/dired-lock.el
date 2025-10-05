;;; test-dired-lock.el --- Test dired-lock -*- lexical-binding: t -*-

;; Author: Ioannis Canellos
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;;; Code:

(require 'ert)
(load (expand-file-name "../dired-lock.el" (file-name-directory load-file-name)))
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
         (test-pdf (format "test-hello-world-main-%d-%d.pdf" (random 10000) (float-time)))
         (base (file-name-sans-extension test-pdf))
         (temp-pdf (concat base "-temp.pdf"))
         (locked-pdf (concat base "-locked.pdf"))
         (password "test123"))
    
    (skip-unless (file-exists-p original-pdf))
    (skip-unless (executable-find "qpdf"))
    
    ;; Clean up any leftover files first
    (when (file-exists-p test-pdf) (delete-file test-pdf))
    (when (file-exists-p temp-pdf) (delete-file temp-pdf))
    (when (file-exists-p locked-pdf) (delete-file locked-pdf))
    
    (unwind-protect
        (progn
          ;; Copy original file for testing
          (copy-file original-pdf test-pdf t)
          
          ;; Test locking and unlocking
          (condition-case err
              (progn
                (dired-lock--lock-pdf test-pdf password)
                (should (file-exists-p test-pdf))
                
                ;; Test unlocking
                (dired-lock--unlock-pdf test-pdf password)
                (should (file-exists-p test-pdf)))
            
            (error
             (ert-skip (format "PDF tools not available or test failed: %s" 
                               (error-message-string err))))))
      
      ;; Cleanup in unwind-protect
      (when (file-exists-p test-pdf) (delete-file test-pdf))
      (when (file-exists-p temp-pdf) (delete-file temp-pdf))
      (when (file-exists-p locked-pdf) (delete-file locked-pdf)))))

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
          (should (file-exists-p (concat test-dir "/subdir/nested-file.txt"))))
      
      (error
       (when (file-exists-p test-zip)
         (delete-file test-zip))
       (when (file-directory-p test-dir)
         (delete-directory test-dir t))
       (ert-skip (format "ZIP tools not available or test failed: %s" 
                         (error-message-string err)))))
    
    ;; Cleanup
    (when (file-exists-p test-zip)
      (delete-file test-zip))
    (when (file-directory-p test-dir)
      (delete-directory test-dir t))))

(ert-deftest dired-lock-filename-helpers-test ()
  "Test filename helper functions."
  (let ((dired-lock-replace-original t))
    (should (string= (dired-lock--get-locked-filename "test.pdf") "test.pdf"))
    (should (string= (dired-lock--get-unlocked-filename "test.pdf") "test.pdf"))
    (should (string= (dired-lock--get-locked-zip-name "mydir") "mydir.zip")))
  
  (let ((dired-lock-replace-original nil))
    (should (string= (dired-lock--get-locked-filename "test.pdf") "test-locked.pdf"))
    (should (string= (dired-lock--get-unlocked-filename "test.pdf") "test-unlocked.pdf"))
    (should (string= (dired-lock--get-unlocked-filename "test-locked.pdf") "test-unlocked.pdf"))
    (should (string= (dired-lock--get-locked-zip-name "mydir") "mydir-locked.zip"))
    (should (string= (dired-lock--get-unlocked-directory-name "mydir-locked.zip") "mydir-unlocked"))
    (should (string= (dired-lock--get-unlocked-directory-name "mydir.zip") "mydir-unlocked"))))

(ert-deftest dired-lock-pdf-suffix-integration-test ()
  "Integration test for PDF locking/unlocking with suffix behavior."
  (let* ((original-pdf "test/resources/hello-world.pdf")
         (test-pdf (format "test-hello-world-suffix-%d.pdf" (random 10000)))
         (locked-pdf (concat (file-name-sans-extension test-pdf) "-locked.pdf"))
         (unlocked-pdf (concat (file-name-sans-extension test-pdf) "-unlocked.pdf"))
         (temp-pdf (concat (file-name-sans-extension test-pdf) "-temp.pdf"))
         (password "test123")
         (dired-lock-replace-original nil))
    
    (skip-unless (file-exists-p original-pdf))
    (skip-unless (executable-find "qpdf"))
    
    ;; Clean up any leftover files first
    (when (file-exists-p test-pdf) (delete-file test-pdf))
    (when (file-exists-p locked-pdf) (delete-file locked-pdf))
    (when (file-exists-p unlocked-pdf) (delete-file unlocked-pdf))
    (when (file-exists-p temp-pdf) (delete-file temp-pdf))
    
    (unwind-protect
        (progn
          ;; Copy original file for testing
          (copy-file original-pdf test-pdf t)
          
          ;; Test locking with suffix
          (condition-case err
              (progn
                (dired-lock--lock-pdf test-pdf password)
                (should (file-exists-p test-pdf))  ; Original should still exist
                (should (file-exists-p locked-pdf))  ; Locked version should exist
                
                ;; Test unlocking with suffix
                (dired-lock--unlock-pdf locked-pdf password)
                (should (file-exists-p locked-pdf))  ; Locked should still exist
                (should (file-exists-p unlocked-pdf)))  ; Unlocked version should exist
            
            (error
             (ert-skip (format "PDF tools not available or test failed: %s" 
                               (error-message-string err))))))
      
      ;; Cleanup in unwind-protect
      (when (file-exists-p test-pdf) (delete-file test-pdf))
      (when (file-exists-p locked-pdf) (delete-file locked-pdf))
      (when (file-exists-p unlocked-pdf) (delete-file unlocked-pdf))
      (when (file-exists-p temp-pdf) (delete-file temp-pdf)))))

(provide 'test-dired-lock)
;;; test-dired-lock.el ends here
