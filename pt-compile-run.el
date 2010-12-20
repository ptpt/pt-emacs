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
(make-variable-buffer-local 'pt-compile-run-compilation-finished-time)
(defvar pt-compile-run-finish-handle #'ignore)
(defvar pt-compile-run-alist nil
  "(MODE . (COMPILE-COMMAND . RUN-COMMAND))")
(defvar pt-compile-run-compile-history nil)
;; (make-variable-buffer-local 'pt-compile-run-compile-history)
(defvar pt-compile-run-run-history nil)
;; (make-variable-buffer-local 'pt-compile-run-run-history)
(defvar pt-compile-run-last-commands nil
  "((TYPE . COMMAND)...)")
(make-variable-buffer-local 'pt-compile-run-last-commands)

(add-to-list 'compilation-finish-functions
             #'pt-compile-run-finish-handle)

(defun pt-compile-run-default-command (type)
  "Return the command string corresponding to TYPE from
`pt-compile-run-alist'."
  (let ((mode (assoc major-mode pt-compile-run-alist)))
    (if mode
        (let ((cmd (cond ((eq 'compile type) (cadr mode))
                         ((eq 'run type) (cddr mode)))))
          (cond ((stringp cmd)
                 (pt-substitution-string
                  cmd
                  pt-compile-run-command-start-sign
                  pt-compile-run-command-end-sign))
                ((eq 'prompt cmd)
                 nil)))
      'prompt)))

(defun pt-compile-run-remember-commands (type cmd)
  "Remember CMD in `pt-compile-run-last-commands'."
  (let ((c (assoc type pt-compile-run-last-commands)))
    (if c
        (setcdr c cmd)
      (add-to-list 'pt-compile-run-last-commands (cons type cmd)))))

(defun pt-compile-run-read-command (type)
  "Read and return command from `pt-compile-run-alist' or the
minibuffer."
  ;; (print pt-compile-run-compile-history)
  (let ((cmd (pt-compile-run-default-command type))
        (history (intern
                  (concat "pt-compile-run-"
                          (symbol-name type)
                          "-history"))))
    (read-shell-command
     (format "%s command: "
             (capitalize (symbol-name type)))
     (if (and cmd (not (eq 'prompt cmd)))
         cmd)
     history)))

(defun pt-compile-run-only-run (&optional run)
  "Run the command RUN. The following Values can be set:"
  (if (eq 'default run)
      (setq run (pt-compile-run-default-command 'run)))
  (if (eq 'prompt run)
      (setq run (pt-compile-run-read-command 'run)))
  (pt-compile-run-remember-commands
   'run (if (string-equal "" run) nil run))
  (if (stringp run)
      (add-to-history 'pt-compile-run-run-history run))
  (cond ((stringp run)
         (let (newframe comint)
           (string-match "\\(?1:\\(\\\\[nc]\\)*\\)\\(?2:.*\\)" run)
           (setq newframe (memq ?n (string-to-list (match-string 1 run))))
           (setq comint (memq ?c (string-to-list (match-string 1 run))))
           (let ((pop-up-frames newframe))
             (if comint
                 (progn (if newframe (new-frame))
                        (comint-run (match-string 2 run)))
               (shell-command (match-string 2 run))))))
        ((consp run)
         (eval-expression run))))

(defun pt-compile-run (&optional compile run)
  "Run command COMPILE if COMPILE is non-nil. After the compilation succeeds,
run the command RUN if RUN is non-nil. The commands can be one of the
following values:

- `default'
    Use the default command from `pt-compile-run-alist'.
- `prompt'
    Prompt in minibuffer which command to run.
- anything else
    Actual command to run."
  (when compile
    (when (eq 'default compile)
      (setq compile (pt-compile-run-default-command 'compile)))
    (when (eq 'prompt compile)
      (setq compile (pt-compile-run-read-command 'compile)))
    (pt-compile-run-remember-commands
     'compile (if (string-equal "" compile) nil compile))
    (when (stringp compile)
      (add-to-history 'pt-compile-run-compile-history compile)))
  (if compile
      (progn (if run
                 (fset #'pt-compile-run-finish-handle
                       `(lambda (buf description)
                          (fset #'pt-compile-run-finish-handle #'ignore)
                          (when (string= "finished\n" description)
                            (set-window-configuration ,(current-window-configuration))
                            (setq pt-compile-run-compilation-finished-time (current-time))
                            (with-current-buffer ,(current-buffer)
                              (pt-compile-run-only-run (quote ,run))))))
               (fset #'pt-compile-run-finish-handle #'ignore))
             (cond ((stringp compile)
                    (let (newframe comint)
                      (string-match  "\\(?1:\\(\\\\[nc]\\)*\\)\\(?2:.*\\)" compile)
                      (if newframe (new-frame))
                      (compilation-start (match-string 2 compile) comint)))
                   ((consp compile) (eval-expression compile))
                   (t (fset #'pt-compile-run-finish-handle #'ignore))))
    (pt-compile-run-only-run run)))

(defmacro pt-compile-run-compile-p ()
  "Return whether it should be compiled or not."
  '(or (and (null (buffer-file-name))
            (set-buffer-modified-p t))
       (buffer-modified-p)
       (null pt-compile-run-compilation-finished-time)
       (time-less-p pt-compile-run-compilation-finished-time
                    (nth 5 (file-attributes (buffer-file-name))))))

(defun pt-compile-run-command (&optional prompt)
  "Repeat last compile and run commands. If PROMPT is non-nil, the last
commands will be prompted."
  (interactive "P")
  (let ((compile (cdr (assoc 'compile pt-compile-run-last-commands)))
        (run (cdr (assoc 'run pt-compile-run-last-commands))))
    (setq run
          (cond (prompt 'prompt)
                ((null run)
                 (if pt-compile-run-last-commands
                     nil
                   'default))
                (t run)))
    (setq compile
          (cond (prompt 'prompt)
                ((null compile)
                 (if pt-compile-run-last-commands
                     nil
                   'default))
                (t compile)))
    (if (or prompt (pt-compile-run-compile-p))
        (progn
          (setq pt-compile-run-last-commands nil)
          (save-buffer)
          (pt-compile-run compile run))
      (pt-compile-run nil run))))

(define-key ctl-x-map "c" 'pt-compile-run-command)
(provide 'pt-compile-run)
