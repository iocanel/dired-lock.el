;;; dired-lock.el --- Lock/unlock archives or pdf files in dired mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ioannis Canellos

;; Author: Ioannis Canellos <iocanel@gmail.com>
;; Maintainer: Ioannis Canellos <iocanel@gmail.com>
;; URL: https://github.com/iocanel/dired-lock
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: dired lock unlock archive pdf

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A simple plugin to lock/unlock archives or pdf files in dired mode.
;;
;; Locking:
;; - `dired-lock-lock` to lock the selected files
;; - If the file is a directory, it replaces the directory with a locked zip
;; - If the file is a pdf, it replaces the pdf with a locked pdf
;;
;; Unlocking:
;; - `dired-lock-unlock` to unlock the selected files
;; - If the file is a locked zip, it replaces the locked zip with a directory
;; - If the file is a locked pdf, it replaces the locked pdf with a pdf
;;

;;; Code:

(require 'dired)
(require 'subr-x)

(defgroup dired-lock nil
  "Lock/unlock files and directories in dired mode."
  :group 'dired
  :prefix "dired-lock-")

(defcustom dired-lock-zip-lock-command "zip -r -P %p %o %i"
  "Command pattern to create password-protected zip archives.
%p = password, %i = input (directory name), %o = output (zip file)."
  :type 'string
  :group 'dired-lock)

(defcustom dired-lock-zip-unlock-command "unzip -o -P %p %i"
  "Command pattern to extract password-protected zip archives.
%p = password, %i = input (zip file)."
  :type 'string
  :group 'dired-lock)

(defcustom dired-lock-pdf-lock-command "qpdf --encrypt %p %p 256 -- %i %o"
  "Command pattern to lock PDF files.
%p = password, %i = input (PDF file), %o = output (locked PDF file)."
  :type 'string
  :group 'dired-lock)

(defcustom dired-lock-pdf-unlock-command "qpdf --decrypt --password=%p %i %o"
  "Command pattern to unlock PDF files.
%p = password, %i = input (locked PDF file), %o = output (unlocked PDF file)."
  :type 'string
  :group 'dired-lock)

(defcustom dired-lock-replace-original t
  "Whether to replace the original file/directory with the locked/unlocked version.
If t, the original file/directory is replaced.
If nil, creates a new file/directory with -locked/-unlocked suffix."
  :type 'boolean
  :group 'dired-lock)

(defcustom dired-lock-revert-buffer t
  "Whether to revert the dired buffer after locking/unlocking operations.
If t, the buffer is reverted to show changes.
If nil, the buffer is not automatically reverted."
  :type 'boolean
  :group 'dired-lock)

(defcustom dired-lock-focus-output-file t
  "Whether to move cursor to the output file after locking/unlocking operations.
If t, the cursor is moved to the resulting file.
If nil, the cursor position is not changed."
  :type 'boolean
  :group 'dired-lock)

(defun dired-lock--read-password (prompt)
  "Read a password with PROMPT, hiding input."
  (read-passwd prompt))

(defun dired-lock--get-locked-filename (filename)
  "Get the locked filename for FILENAME based on customization settings."
  (if dired-lock-replace-original
      filename
    (let ((base (file-name-sans-extension filename))
          (ext (file-name-extension filename)))
      (if ext
          (concat base "-locked." ext)
        (concat filename "-locked")))))

(defun dired-lock--get-unlocked-filename (filename)
  "Get the unlocked filename for FILENAME based on customization settings."
  (if dired-lock-replace-original
      filename
    (let ((base (file-name-sans-extension filename))
          (ext (file-name-extension filename)))
      ;; If the base ends with "-locked", replace it with "-unlocked"
      (when (string-suffix-p "-locked" base)
        (setq base (concat (string-remove-suffix "-locked" base) "-unlocked")))
      (if ext
          (concat base (if (string-suffix-p "-unlocked" base) "" "-unlocked") "." ext)
        (concat filename (if (string-suffix-p "-unlocked" filename) "" "-unlocked"))))))

(defun dired-lock--get-locked-zip-name (directory)
  "Get the zip filename for locking DIRECTORY based on customization settings."
  (if dired-lock-replace-original
      (concat directory ".zip")
    (concat directory "-locked.zip")))

(defun dired-lock--get-unlocked-directory-name (zip-file)
  "Get the directory name for unlocking ZIP-FILE based on customization settings."
  (let ((base (file-name-sans-extension zip-file)))
    (if dired-lock-replace-original
        (if (string-suffix-p "-locked" base)
            (string-remove-suffix "-locked" base)
          base)
      (if (string-suffix-p "-locked" base)
          (concat (string-remove-suffix "-locked" base) "-unlocked")
        (concat base "-unlocked")))))

(defun dired-lock--substitute-command (command-pattern &rest substitutions)
  "Substitute placeholders in COMMAND-PATTERN with SUBSTITUTIONS.
SUBSTITUTIONS should be a plist with keys like :password, :input, :output."
  (let ((result command-pattern))
    (when-let ((password (plist-get substitutions :password)))
      (setq result (replace-regexp-in-string "%p" password result)))
    (when-let ((input (plist-get substitutions :input)))
      (setq result (replace-regexp-in-string "%i" input result)))
    (when-let ((output (plist-get substitutions :output)))
      (setq result (replace-regexp-in-string "%o" output result)))
    result))

(defun dired-lock--execute-command (command)
  "Execute COMMAND and return exit code."
  (call-process-shell-command command))

(defun dired-lock--lock-directory (directory password)
  "Lock DIRECTORY with PASSWORD by creating a password-protected zip.
Returns the path to the created zip file."
  (let* ((dir-name (file-name-nondirectory (directory-file-name directory)))
         (zip-file (dired-lock--get-locked-zip-name directory))
         (default-directory (or (file-name-directory directory) ".")))
    (when (file-exists-p zip-file)
      (error "Zip file %s already exists" zip-file))
    (let* ((command (dired-lock--substitute-command 
                     dired-lock-zip-lock-command
                     :password (shell-quote-argument password)
                     :input (shell-quote-argument dir-name)
                     :output (shell-quote-argument (file-name-nondirectory zip-file))))
           (exit-code (dired-lock--execute-command command)))
      (if (zerop exit-code)
          (progn
            (when dired-lock-replace-original
              (delete-directory directory t))
            zip-file)
        (error "Failed to create locked zip file")))))

(defun dired-lock--unlock-zip (zip-file password)
  "Unlock ZIP-FILE with PASSWORD by extracting it.
Returns the path to the extracted directory."
  (let* ((target-dir (dired-lock--get-unlocked-directory-name zip-file))
         (extract-dir (file-name-directory zip-file))
         (default-directory extract-dir)
         (zip-basename (file-name-nondirectory zip-file)))
    (when (file-exists-p target-dir)
      (error "Directory %s already exists" target-dir))
    (let* ((command (dired-lock--substitute-command 
                     dired-lock-zip-unlock-command
                     :password (shell-quote-argument password)
                     :input (shell-quote-argument zip-basename)))
           (exit-code (dired-lock--execute-command command)))
      (if (zerop exit-code)
          (let* ((extracted-name (file-name-sans-extension zip-basename))
                 (extracted-path (concat extract-dir extracted-name)))
            ;; Rename extracted directory to target name if different
            (when (and (not (string= extracted-name (file-name-nondirectory target-dir)))
                       (file-exists-p extracted-path))
              (rename-file extracted-path target-dir))
            (when dired-lock-replace-original
              (delete-file zip-file))
            target-dir)
        (error "Failed to unlock zip file (wrong password?)")))))

(defun dired-lock--lock-pdf (pdf-file password)
  "Lock PDF-FILE with PASSWORD.
Returns the path to the locked PDF file."
  (let* ((base (file-name-sans-extension pdf-file))
         (ext (file-name-extension pdf-file))
         (locked-pdf (concat base "-locked." ext))
         (temp-pdf (concat base "-temp.pdf")))
    
    (when (file-exists-p locked-pdf)
      (error "Locked PDF %s already exists" locked-pdf))
    (when (file-exists-p temp-pdf)
      (error "Temporary PDF %s already exists" temp-pdf))
      
    (let* ((command (dired-lock--substitute-command 
                     dired-lock-pdf-lock-command
                     :password (shell-quote-argument password)
                     :input (shell-quote-argument pdf-file)
                     :output (shell-quote-argument temp-pdf)))
           (exit-code (dired-lock--execute-command command)))
      (if (zerop exit-code)
          (progn
            (rename-file temp-pdf locked-pdf)
            
            ;; Handle replacement logic
            (if dired-lock-replace-original
                (progn
                  (delete-file pdf-file)
                  (rename-file locked-pdf pdf-file)
                  pdf-file)
              locked-pdf))
        (when (file-exists-p temp-pdf)
          (delete-file temp-pdf))
        (error "Failed to lock PDF file")))))

(defun dired-lock--unlock-pdf (pdf-file password)
  "Unlock PDF-FILE with PASSWORD.
Returns the path to the unlocked PDF file."
  (let* ((base (file-name-sans-extension pdf-file))
         (ext (file-name-extension pdf-file))
         (unlocked-pdf (if (string-suffix-p "-locked" base)
                           (concat (string-remove-suffix "-locked" base) "-unlocked." ext)
                         (concat base "-unlocked." ext)))
         (temp-pdf (concat base "-temp.pdf")))
    
    (when (file-exists-p unlocked-pdf)
      (error "Unlocked PDF %s already exists" unlocked-pdf))
    (when (file-exists-p temp-pdf)
      (error "Temporary PDF %s already exists" temp-pdf))
      
    (let* ((command (dired-lock--substitute-command 
                     dired-lock-pdf-unlock-command
                     :password (shell-quote-argument password)
                     :input (shell-quote-argument pdf-file)
                     :output (shell-quote-argument temp-pdf)))
           (exit-code (dired-lock--execute-command command)))
      (if (zerop exit-code)
          (progn
            (rename-file temp-pdf unlocked-pdf)
            
            ;; Handle replacement logic
            (if dired-lock-replace-original
                (progn
                  (delete-file pdf-file)
                  (rename-file unlocked-pdf pdf-file)
                  pdf-file)
              unlocked-pdf))
        (when (file-exists-p temp-pdf)
          (delete-file temp-pdf))
        (error "Failed to unlock PDF file (wrong password?)")))))

(defun dired-lock--is-locked-zip-p (file)
  "Check if FILE is a password-protected zip file."
  (and (string-match-p "\\.zip\\'" file)
       (file-exists-p file)
       (not (file-directory-p file))))

(defun dired-lock--is-pdf-p (file)
  "Check if FILE is a PDF file."
  (and (string-match-p "\\.pdf\\'" file)
       (file-exists-p file)
       (not (file-directory-p file))))

(defun dired-lock--post-operation-actions (output-file)
  "Perform post-operation actions: revert buffer and focus output file.
OUTPUT-FILE is the resulting file after lock/unlock operation."
  (when dired-lock-revert-buffer
    (revert-buffer))
  (when (and dired-lock-focus-output-file output-file (file-exists-p output-file))
    (dired-goto-file output-file)))

;;;###autoload
(defun dired-lock-lock ()
  "Lock the selected files in dired mode."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (when (null files)
      (error "No files selected"))
    (let ((password (dired-lock--read-password "Enter password to lock files: "))
          (output-files nil))
      (dolist (file files)
        (let ((output-file
               (cond
                ((file-directory-p file)
                 (dired-lock--lock-directory file password))
                ((dired-lock--is-pdf-p file)
                 (dired-lock--lock-pdf file password))
                (t
                 nil))))
          (when output-file
            (push output-file output-files))))
      (dired-lock--post-operation-actions (car output-files)))))

;;;###autoload
(defun dired-lock-unlock ()
  "Unlock the selected files in dired mode."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (when (null files)
      (error "No files selected"))
    (let ((password (dired-lock--read-password "Enter password to unlock files: "))
          (output-files nil))
      (dolist (file files)
        (let ((output-file
               (cond
                ((dired-lock--is-locked-zip-p file)
                 (dired-lock--unlock-zip file password))
                ((dired-lock--is-pdf-p file)
                 (dired-lock--unlock-pdf file password))
                (t
                 nil))))
          (when output-file
            (push output-file output-files))))
      (dired-lock--post-operation-actions (car output-files)))))

(provide 'dired-lock)
;;; dired-lock.el ends here
