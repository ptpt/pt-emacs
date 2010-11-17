;;; pt-compile-run.el

;; Copyright (C) 2010 Tao Peng <ptpttt@gmail.com>

;; Author:   Tao Peng <ptpttt@gmail.com>
;; Keywords: c, tools

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Code:
(require 'pt-substitution)

(defvar pt-compile-run-command-start-sign "$")
(defvar pt-compile-run-command-end-sign "$")
(defvar pt-compile-run-compilation-finished-time nil)
(make-local-variable 'pt-compile-run-compilation-finished-time)
(defvar pt-compile-run-finish-handle #'ignore)

(defvar pt-compile-run-alist nil
  "(MODE . (COMPILE-COMMAND . RUN-COMMAND))")

(defun pt-compile-run-get-command (type)
  "Return the command string according to TYPE from
`pt-compile-run-alist'."
  (let ((mode (assoc major-mode pt-compile-run-alist)))
    (when mode
      (let ((cmd (cond ((eq 'compile type) (cadr mode))
                       ((eq 'run type) (cddr mode)))))
        (if (stringp cmd)
            (pt-substitution-string
             cmd
             pt-compile-run-command-start-sign
             pt-compile-run-command-end-sign)
          cmd)))))

(defun pt-compile-run-read-command (type &optional arg)
  "Read command from `pt-compile-run-alist' or the minibuffer."
  (let ((cmd (pt-compile-run-get-command type)))
    (if (or arg (null cmd))
        (cond ((or (stringp cmd) (null cmd))
               (condition-case nil
                   (read-shell-command
                    (format "%s command: "
                            (capitalize (symbol-name type))) cmd)
                 (quit nil)))
              ((consp cmd)
               (read-from-minibuffer
                (format "%s eval: "
                        (capitalize (symbol-name type)))
                (prin1-to-string cmd)
                read-expression-map t
                'read-expression-history)))
      cmd)))

(defun pt-compile-run-2 (&optional command)
  "Like `pt-compile-run', except it doesn't save buffer.
It's only for internal use."
  (interactive
   (list (pt-compile-run-read-command 'compile current-prefix-arg)))
  (unless (called-interactively-p 'any)
    (setq command (or command (pt-compile-run-get-command 'compile))))
  (if (or (null command)
          (string-equal command ""))
      (if (called-interactively-p 'any)
          (call-interactively 'pt-compile-run-only-run)
        (pt-compile-run-only-run))
    (progn
      (fset #'pt-compile-run-finish-handle
            `(lambda (buf description)
               (fset #'pt-compile-run-finish-handle #'ignore)
               (when (string= "finished\n" description)
                 (set-window-configuration ,(current-window-configuration))
                 (setq pt-compile-run-compilation-finished-time (current-time))
                 (with-current-buffer ,(current-buffer)
                   ,(if (called-interactively-p 'any)
                        '(call-interactively 'pt-compile-run-only-run)
                      '(pt-compile-run-only-run))))))
      (cond ((stringp command)
             (let (newframe comint)
               (string-match  "\\(?1:\\(\\\\[nc]\\)*\\)\\(?2:.*\\)" command)
               (if newframe (new-frame))
               (compilation-start (match-string 2 command) comint)))
            ((consp command)
             (eval-expression command))
            (t (fset #'pt-compile-run-finish-handle #'ignore))))))

(defun pt-compile-run-only-run (&optional command)
  "Run the compilation program as a command."
  (interactive
   (list (pt-compile-run-read-command 'run current-prefix-arg)))
  (unless (called-interactively-p 'any)
    (setq command (or command (pt-compile-run-get-command 'run))))
  (cond ((and (stringp command)
              (not (string-equal command "")))
         (let (newframe comint)
           (string-match "\\(?1:\\(\\\\[nc]\\)*\\)\\(?2:.*\\)" command)
           (setq newframe (memq ?n (string-to-list (match-string 1 command))))
           (setq comint (memq ?c (string-to-list (match-string 1 command))))
           (let ((pop-up-frames newframe))
             (if comint
                 (progn (if newframe (new-frame))
                        (comint-run (match-string 2 command)))
               (shell-command (match-string 2 command))))))
        ((consp command)
         (eval-expression command))))

(add-to-list 'compilation-finish-functions
             #'pt-compile-run-finish-handle)

(defun pt-compile-run (&optional arg)
  "Compile and run or only run commands from `pt-compile-run-alist' or
minibuffer. Before compilation, current buffer will be saved."
  (interactive "P")
  (if (or arg
          (null (buffer-file-name))
          (buffer-modified-p)
          (null pt-compile-run-compilation-finished-time)
          (time-less-p pt-compile-run-compilation-finished-time
                       (nth 5 (file-attributes (buffer-file-name)))))
      (progn
        (if (null (buffer-file-name))
            (set-buffer-modified-p t))
        (save-buffer)
        (if (called-interactively-p 'any)
            (call-interactively 'pt-compile-run-2)
          (pt-compile-run-2)))
    (if (called-interactively-p 'any)
        (call-interactively 'pt-compile-run-only-run)
      (pt-compile-run-only-run))))

(define-key ctl-x-map "c" 'pt-compile-run)
(define-key ctl-x-map "x" 'pt-compile-run-only-run)

(provide 'pt-compile-run)