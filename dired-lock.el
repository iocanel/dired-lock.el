;;; dired-lock.el --- Lock/unlock archives or pdf files in dired mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ioannis Canellos

;; Author: Ioannis Canellos <iocanel@gmail.com>
;; Maintainer: Ioannis Canellos <iocanel@gmail.com>
;; URL: https://github.com/iocanel/dired-lock
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
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

(defgroup dired-lock nil
  "Lock/unlock files and directories in dired mode."
  :group 'dired
  :prefix "dired-lock-")

(defcustom dired-lock-zip-lock-command "zip -r -P %p %o %i"
  "Command pattern to create password-protected zip archives.
%p = password, %i = input (directory name), %o = output (zip file)."
  :type 'string
  :group 'dired-lock)

(defcustom dired-lock-zip-unlock-command "unzip -P %p %i"
  "Command pattern to extract password-protected zip archives.
%p = password, %i = input (zip file)."
  :type 'string
  :group 'dired-lock)

(defcustom dired-lock-pdf-lock-command "qpdf --encrypt %p %p 40 -- %i %o"
  "Command pattern to lock PDF files.
%p = password, %i = input (PDF file), %o = output (locked PDF file)."
  :type 'string
  :group 'dired-lock)

(defcustom dired-lock-pdf-unlock-command "qpdf --decrypt --password=%p %i %o"
  "Command pattern to unlock PDF files.
%p = password, %i = input (locked PDF file), %o = output (unlocked PDF file)."
  :type 'string
  :group 'dired-lock)

(defun dired-lock--read-password (prompt)
  "Read a password with PROMPT, hiding input."
  (read-passwd prompt))

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
  "Lock DIRECTORY with PASSWORD by creating a password-protected zip."
  (let* ((dir-name (file-name-nondirectory (directory-file-name directory)))
         (zip-file (concat directory ".zip"))
         (default-directory (file-name-directory directory)))
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
            (delete-directory directory t)
            (message "Directory %s locked as %s" directory zip-file))
        (error "Failed to create locked zip file")))))

(defun dired-lock--unlock-zip (zip-file password)
  "Unlock ZIP-FILE with PASSWORD by extracting it."
  (let* ((dir-name (file-name-sans-extension (file-name-nondirectory zip-file)))
         (extract-dir (file-name-directory zip-file))
         (default-directory extract-dir))
    (when (file-exists-p (concat extract-dir dir-name))
      (error "Directory %s already exists" (concat extract-dir dir-name)))
    (let* ((command (dired-lock--substitute-command 
                     dired-lock-zip-unlock-command
                     :password (shell-quote-argument password)
                     :input (shell-quote-argument (file-name-nondirectory zip-file))))
           (exit-code (dired-lock--execute-command command)))
      (if (zerop exit-code)
          (progn
            (delete-file zip-file)
            (message "Zip file %s unlocked to %s" zip-file (concat extract-dir dir-name)))
        (error "Failed to unlock zip file (wrong password?)")))))

(defun dired-lock--lock-pdf (pdf-file password)
  "Lock PDF-FILE with PASSWORD."
  (let ((temp-pdf (concat (file-name-sans-extension pdf-file) "-temp.pdf")))
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
            (delete-file pdf-file)
            (rename-file temp-pdf pdf-file)
            (message "PDF %s locked" pdf-file))
        (when (file-exists-p temp-pdf)
          (delete-file temp-pdf))
        (error "Failed to lock PDF file")))))

(defun dired-lock--unlock-pdf (pdf-file password)
  "Unlock PDF-FILE with PASSWORD."
  (let ((temp-pdf (concat (file-name-sans-extension pdf-file) "-temp.pdf")))
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
            (delete-file pdf-file)
            (rename-file temp-pdf pdf-file)
            (message "PDF %s unlocked" pdf-file))
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

;;;###autoload
(defun dired-lock-lock ()
  "Lock the selected files in dired mode."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (when (null files)
      (error "No files selected"))
    (let ((password (dired-lock--read-password "Enter password to lock files: ")))
      (dolist (file files)
        (cond
         ((file-directory-p file)
          (dired-lock--lock-directory file password))
         ((dired-lock--is-pdf-p file)
          (dired-lock--lock-pdf file password))
         (t
          (message "Skipping %s (not a directory or PDF)" file)))))
    (revert-buffer)))

;;;###autoload
(defun dired-lock-unlock ()
  "Unlock the selected files in dired mode."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (when (null files)
      (error "No files selected"))
    (let ((password (dired-lock--read-password "Enter password to unlock files: ")))
      (dolist (file files)
        (cond
         ((dired-lock--is-locked-zip-p file)
          (dired-lock--unlock-zip file password))
         ((dired-lock--is-pdf-p file)
          (dired-lock--unlock-pdf file password))
         (t
          (message "Skipping %s (not a locked zip or PDF)" file)))))
    (revert-buffer)))

(provide 'dired-lock)
;;; dired-lock.el ends here
