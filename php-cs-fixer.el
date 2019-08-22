;;; php-cs-fixer.el --- emacs wrapper for php-cs-fixer            -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: Mikael Kermorgant <mikael@kgtech.fi>
;; Created: 7 October 2018
;; Version: 0.0.1
;; Keywords: tools, php
;; Package-Requires: ((emacs "24.3") (f "0.17"))
;; URL: https://github.com/kermorgant/php-cs-fixer.el
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; PHP-CS-Fixer: A tool to automatically fix PHP coding standards issues https://cs.sensiolabs.org
;; https://github.com/FriendsOfPHP/PHP-CS-Fixer
;;
;; The following definitions from go-mode.el have been adapted :
;; (Author: Dominik Honnef, url: https://github.com/dominikh/go-mode.el)
;;
;; go--apply-rcs-patch go--goto-line go--delete-whole-line

(require 'php-project)

;;; Code:
(defvar php-cs-fixer--executable nil
  "The path to php-cs-fixer.")

(defvar php-cs-fixer--config-file nil
  "The path to php-cs-fixer config file. When nil, will be guessed.")

(defvar php-cs-fixer--args '()
  "List of args for php-cs-fixer command.")

(defcustom php-cs-fixer--enable t
  "Control whether code style fixing happens or not.")

(defun php-cs-fixer--get-project-dir ()
  "Return project directory."
  (directory-file-name
   (expand-file-name (php-project-get-root-dir))))

(defun php-cs-fixer--find-executable ()
  "Return path of php-cs-fixer executable."
  (let ((executable php-cs-fixer--executable))
    (unless executable
      (setq executable (executable-find "php-cs-fixer"))
      (unless executable (error "Could not find php-cs-fixer in path")))
    (unless (file-exists-p executable)
      (error (format "Could not find php-cs-fixer at path %s" executable)))
    executable))

;;;###autoload
(defun  php-cs-fixer--fix ()
  "Replace the source code in the current file."
  (interactive)
  (when php-cs-fixer--enable
    (let ((tmpfile (make-temp-file "php-cs-fixer" nil ".php"))
          (patchbuf (get-buffer-create "*PhpCsFixer patch*"))
          (msgbuf (get-buffer-create "*PhpCsFixer messages*"))
          (sourcebuffer (current-buffer))
          ;; (coding-system-for-read 'utf-8)
          ;; (coding-system-for-write 'utf-8)
          )

      (unwind-protect
          (save-restriction
            (widen)
            (with-current-buffer patchbuf
              (erase-buffer))

            (with-temp-file tmpfile
              (insert-buffer-substring-no-properties sourcebuffer))

            (if (zerop (apply 'call-process (php-cs-fixer--find-executable) nil msgbuf t
                              (append (list "fix")
                                      php-cs-fixer--args
                                      (php-cs-fixer--get-config-arg)
                                      (php-cs-fixer--get-cache-arg tmpfile)
                                      (list tmpfile))))
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                    (message "Buffer was unchanged by php-cs-fixer")
                  (php-cs-fixer--apply-rcs-patch patchbuf)
                  (message "Buffer modified by php-cs-fixer"))
              (message "php-cs-fixer exited in error")))

        (kill-buffer patchbuf)
        (delete-file tmpfile)))))

(defun php-cs-fixer--get-config-arg ()
  "Look for a config file and return relevant cli argument if found."
  (if php-cs-fixer--config-file
      (if (file-exists-p php-cs-fixer--config-file)
          (list "--config" php-cs-fixer--config-file)
        (error (format "php-cs-fixer config file %s not found" php-cs-fixer--config-file)))
    (cond ((file-exists-p (f-join (php-cs-fixer--get-project-dir) ".php_cs"))
           (list "--config" (f-join (php-cs-fixer--get-project-dir) ".php_cs")))
          ((file-exists-p (f-join (php-cs-fixer--get-project-dir) ".php_cs.dist"))
           (list "--config"(f-join (php-cs-fixer--get-project-dir) ".php_cs.dist")))
          (t ()))))

(defun php-cs-fixer--get-cache-arg (target)
  "Return --using-cache argument, based on TARGET."
  (cond ((file-exists-p target)
         (list "--using-cache" "no"))
        (t ())))

;; this function was adapted from go-mode
(defun php-cs-fixer--goto-line (line)
  "Goto line LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

;; this function was adapted from go-mode
(defun php-cs-fixer--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

;; this function was adapted from go-mode
(defun php-cs-fixer--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0)
        (column (current-column)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in go--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (php-cs-fixer--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (php-cs-fixer--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in phpactor--apply-rcs-patch")))))))
    (move-to-column column)))

(provide 'php-cs-fixer)
;;; php-cs-fixer.el ends here
