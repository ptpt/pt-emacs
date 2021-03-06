;;; -*- lexical-binding: t -*-

;;; pt-simple.el --- Tao Peng's basic configuration for Emacs

;; Copyright (C) 2010-2016 Tao Peng <ptpttt@gmail.com>

;; Author:   Tao Peng <ptpttt@gmail.com>
;; Keywords: local

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

(eval-when-compile
  (require 'cl))


;; variables


;;; functions

(defadvice mouse-drag-region
    (around pt-mouse-drag-region-no-cursor activate)
  (let ((old-cursor-type cursor-type))
    (setq cursor-type nil)
    ad-do-it
    (setq cursor-type old-cursor-type)))

(defmacro pt-get-directory-create (dir)
  "Create directory DIR if not found. Return DIR."
  `(progn (unless (file-exists-p ,dir)
            (make-directory ,dir t))
          (file-name-as-directory ,dir)))

(defun pt-add-subdirectories-to-list (dir)
  "Add all subdirectories not starting with \".\" of DIR to `load-path'."
  (mapc (lambda (file)
          (if (file-directory-p file)
              (add-to-list 'load-path file)))
        (directory-files dir t "\\`[^.]")))

(defun pt-set-font (&optional font)
  "Like `set-frame-font', in addition to set font of the current frame, it
also set font for new frames."
  (interactive
   (list (completing-read (format "Font: ")
                          (font-family-list))))
  (when font
    (add-to-list 'default-frame-alist (cons 'font font))
    (set-frame-font font)))

(defun pt-tab-command (&optional arg)
  "If mark is active, indent the region, otherwise run
`indent-for-tab-command' in the beginning of line, elsewhere run
`dabbrev-expand'."
  (interactive "P")
  (cond (mark-active
         (indent-region (point) (mark) arg))
        ((or buffer-read-only
             (minibufferp)
             (looking-back "^[ \t]*"))
         (indent-for-tab-command arg))
        (t (dabbrev-expand arg))))

(global-set-key [remap indent-for-tab-command] 'pt-tab-command)

(defun pt-beginning-of-line-or-text ()
  "Switch position between beginning of line and text."
  (interactive "^")
  (let ((pt (point)))
    (back-to-indentation)
    (when (= pt (point))
      (move-beginning-of-line nil))))

(global-set-key [remap move-beginning-of-line] 'pt-beginning-of-line-or-text)

(defun pt-delete-lines (&optional arg)
  "Delete lines like `delete-blank-lines'.
But if current line is not blank, it will `kill-whole-line'."
  (interactive "*p")
  (let (thisblank)
    (save-excursion
      (beginning-of-line)
      (setq thisblank (looking-at "[ \t]*$")))
    (if thisblank
        (delete-blank-lines)
      (let ((col (current-column)))
        (kill-whole-line arg)
        (let ((kill (current-kill 0 t)))
          (kill-new (replace-regexp-in-string "^[\\t ]*\\|[\\t ]*$" "" kill)))
        (move-to-column col)))))

(defun pt-kill-region-or-line (&optional arg)
  "If mark is active, act `kill-region' as normal, otherwise delete
current lines using `pt-lines'."
  (interactive "*p")
  (if mark-active
      (kill-region (mark) (point))
    (pt-delete-lines arg)))

(global-set-key [remap kill-region] 'pt-kill-region-or-line)

(mapc (lambda (command)
        (global-set-key (vector 'remap command)
                        (lambda (&optional arg)
                          (interactive "p")
                          (save-excursion
                            (when (not mark-active)
                              (beginning-of-line)
                              (push-mark-command t t)
                              (end-of-line arg))
                            (call-interactively command)))))
      '(kill-ring-save
        kill-region
        comment-region
        comment-or-uncomment-region
        clipboard-kill-region
        clipboard-kill-ring-save))

(defun pt-set-mark-or-copy-region (&optional arg)
  (interactive "P")
  (if (use-region-p)
      (kill-ring-save (mark) (point)))
  (funcall (or (command-remapping 'set-mark-command) 'set-mark-command) arg))

(global-set-key [remap kill-ring-save] 'pt-set-mark-or-copy-region)

(defun pt-hungry-delete-backwards ()
  "Delete backwards whitespaces."
  (interactive)
  (let ((here (point)))
    ;; copy from `c-skip-ws-backward'
    (skip-chars-backward " \t\n\r\f\v")
    (when (/= here (point))
      (delete-region (point) here))))

(defun pt-hungry-delete-forwards ()
  "Delete forwards whitespaces."
  (interactive)
  (let ((here (point)))
    ;; copy from `c-skip-ws-forward'
    (skip-chars-forward " \t\n\r\f\v")
    (when (/= here (point))
      (delete-region (point) here))))

(defun pt-hungry-delete ()
  "Delete whitespaces"
  (interactive)
  (if (or (bobp)
          (looking-back "[^ \t\n\r\f\v]"))
      (pt-hungry-delete-forwards)
    (pt-hungry-delete-backwards)))

(defun pt-backward-kill-word-or-whitespace ()
  "hungrily kill word backward."
  (interactive)
  (if (looking-back "[ \t\n\r\f\v]")
      (pt-hungry-delete-backwards)
    (backward-kill-word 1)))

(defun pt-forward-kill-word-or-whitespace ()
  "hungrily kill word backward."
  (interactive)
  (if (looking-at "[ \t\n\r\f\v]")
      (pt-hungry-delete-forwards)
    (kill-word 1)))

(global-set-key [remap backward-kill-word] 'pt-backward-kill-word-or-whitespace)
(global-set-key [remap kill-word] 'pt-forward-kill-word-or-whitespace)

(defun pt-buffer-file-basename (&optional buffer)
  (or (and (buffer-file-name buffer)
           (file-name-nondirectory (buffer-file-name buffer)))
      (buffer-name buffer)))

(defmacro pt-xor (a b)
  "XOR logic operation."
  `(and (or ,a ,b) (not (and ,a ,b))))

(defun pt-forward-whitespace (&optional arg)
  "Move point forward whitespace."
  (interactive "^p")
  (re-search-forward
   "[^ \t\n\r\f$][ \t\n\r\f$]\\|\\'"
   nil nil arg)
  (unless (eobp)
    (backward-char)))

(defun pt-backward-whitespace (&optional arg)
  "Move point backward whitespace."
  (interactive "^p")
  (re-search-backward
   "[ \t\n\r\f^][^ \t\n\r\f^]\\|\\`"
   nil nil arg)
  (unless (bobp)
    (forward-char)))

(global-set-key (kbd "M-e") 'pt-forward-whitespace)
(global-set-key (kbd "M-a") 'pt-backward-whitespace)

(when (eq 'darwin system-type)
  (defun pt-pbpaste ()
    "Paste data from pasteboard."
    (interactive)
    (when (executable-find "pbpaste")
      (shell-command-on-region
       (point)
       (if mark-active (mark) (point))
       "pbpaste" nil t)))

  (defun pt-pbcopy ()
    "Copy region to pasteboard."
    (interactive)
    (when (and mark-active
               (executable-find "pbcopy"))
      (shell-command-on-region
       (point) (mark) "pbcopy")
      (kill-buffer "*Shell Command Output*")))

  (global-set-key [?\C-x ?\C-y] 'pt-pbpaste)
  (global-set-key [?\C-x ?\M-w] 'pt-pbcopy))

(defun pt-delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' has been removed successfully." filename)))))

(defun pt-change-cursor-type ()
  (cond ((or buffer-read-only
             (get-text-property (point) 'read-only)
             (eq major-mode 'term-mode))
         (setq cursor-type 'box)
         (blink-cursor-mode -1))
        (t
         (setq cursor-type (default-value 'cursor-type))
         (blink-cursor-mode 1))))

(defun pt-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

(define-key ctl-x-map "7" 'pt-swap-windows)

(defun pt-delete-directory (dir)
  "Delete empty directories recursively."
  (let ((dirs (directory-files dir t "\\`[^.]")))
    (cond ((and (= 1(length dirs))
                (file-directory-p (car dirs)))
           (and (pt-delete-directory (car dirs))
                (or (delete-directory dir) t)))
          ((= 0 (length dirs))
           (delete-directory dir) t)
          (t nil))))

(defun pt-make-directory-and-return-parents (dir)
  "Create directory DIR and its parent directories.
Return a list of the new created directories."
  (unless (file-exists-p dir)
    (append (pt-make-directory-and-return-parents (expand-file-name ".." dir))
            (cons (progn (make-directory dir) (file-name-as-directory dir)) nil))))

(defadvice save-buffer (around pave-path activate)
  "Pave the path of the visited file before save buffer"
  (if (and (not (buffer-file-name))
           (called-interactively-p 'any))
      (condition-case err
          ad-do-it
        (error
         (let ((filename (and (listp file-name-history)
                              (car file-name-history))))
           (if (and (stringp filename)
                    (string-match "\\(.*\\): no such directory\\'" (cadr err))
                    (string-equal (match-string 1 (cadr err))
                                  (file-name-directory (expand-file-name filename)))
                    (y-or-n-p (format "%s; save create? " (cadr err))))
               ;; now, the parent directory has been created
               (let* ((dirs (pt-make-directory-and-return-parents
                             (file-name-directory filename)))
                      (buffer-name (buffer-name)))
                 (condition-case err2
                     (progn (set-visited-file-name filename)
                            ad-do-it)
                   (error
                    ;; recover buffer
                    (set-visited-file-name nil)
                    (rename-buffer buffer-name)
                    (mapc 'delete-directory (reverse dirs))
                    (signal (car err2) (cdr err2)))))
             (signal (car err) (cdr err))))))
    ad-do-it))

(add-hook 'write-file-functions
          (lambda ()
            (let ((dir (file-name-directory (buffer-file-name))))
              (and (not (file-directory-p dir))
                   (y-or-n-p (format "%s: no such directory; create? " dir))
                   (make-directory dir t)))))

(defvar pt-binary-span 0)
(make-variable-buffer-local 'pt-binary-span)

(defun pt-count-window-lines (&optional pos counter)
  "Return the number of lines between window top line and POS.
If pos is nil, it defaults to current point.
COUNTER, if non-nil, means count lines between bottom line and POS."
  (let ((pos (or pos (point)))
        window-start-pos
        window-end-pos)
    (save-excursion
      ;; get window start and end points
      (move-to-window-line 0)
      (setq window-start-pos (point))
      ;; you can also use (window-start) and (window-end) to get the points,
      ;; but (window-end) will get the start point of the line that totally
      ;; doesn't show, while (move-to-window-line -1) just goto the start point
      ;; of the last line that is totally shown in the window.
      (move-to-window-line -1)
      (setq window-end-pos (point))
      (goto-char pos)
      (if line-move-visual
          (progn (vertical-motion 0)
                 (count-screen-lines (if counter (point) window-start-pos)
                                     (if counter window-end-pos (point))))
        (progn (count-lines (if counter (line-beginning-position) window-start-pos)
                            (if counter window-end-pos (line-beginning-position))))))))

(defun pt-binary-previous-line (&optional arg)
  (interactive "^p")
  (unless (and (numberp pt-binary-span)
               (memq last-command
                     '(pt-binary-next-line
                       pt-binary-previous-line)))
    (setq pt-binary-span (1+ (pt-count-window-lines))))
  (dotimes (_ arg)
    (previous-line (max 1 (/ pt-binary-span 2)))
    (setq pt-binary-span (- pt-binary-span (/ pt-binary-span 2)))))

(defun pt-binary-next-line (&optional arg)
  (interactive "^p")
  (unless (and (numberp pt-binary-span)
               (memq last-command
                     '(pt-binary-next-line
                       pt-binary-previous-line)))
    (setq pt-binary-span (1+ (pt-count-window-lines nil t))))
  (dotimes (_ arg)
    (next-line (max 1 (/ pt-binary-span 2)))
    (setq pt-binary-span (- pt-binary-span (/ pt-binary-span 2)))))

(global-set-key [?\M-p] 'pt-binary-previous-line)
(global-set-key [?\M-n] 'pt-binary-next-line)

(defun pt-capitalize-word-or-region (&optional arg)
  (interactive "p")
  (if mark-active
      (capitalize-region (mark) (point))
    (capitalize-word arg)))

(defun pt-upcase-word-or-region (&optional arg)
  (interactive "p")
  (if mark-active
      (upcase-region (mark) (point))
    (upcase-word arg)))

(defun pt-downcase-word-or-region (&optional arg)
  (interactive "p")
  (if mark-active
      (downcase-region (mark) (point))
    (downcase-word arg)))

(global-set-key [remap capitalize-word] 'pt-capitalize-word-or-region)
(global-set-key [remap upcase-word] 'pt-upcase-word-or-region)
(global-set-key [remap downcase-word] 'pt-downcase-word-or-region)

(defun pt-switch-buffer (&optional arg)
  (interactive "p")
  (if (not (one-window-p))
      (other-window (or arg 1))
    (progn
      (if (consp arg)
          (split-window))
      (switch-to-buffer (other-buffer (current-buffer) t)))))

(global-set-key (kbd "M-`") 'pt-switch-buffer)

(defun pt-shrink-frame-to-fit-buffer (&optional frame)
  (interactive)
  (when (null frame)
    (setq frame (selected-frame)))
  (let* ((window (frame-root-window frame))
         (old-height (window-height window))
         height)
    (when (window-live-p window)
      (split-window window)
      (fit-window-to-buffer window)
      (setq height (window-height window))
      (unless (equal old-height height)
        (set-frame-height frame height))
      (delete-other-windows window))))

;; get rid of the annoying error message "text is read-only"
(defun pt-minibuffer-delete-backward-char (&optional arg)
  (interactive "p")
  (let ((pt (max (- (point) 1) (point-min))))
    (unless (get-text-property pt 'read-only)
      (delete-backward-char arg))))

(define-key minibuffer-local-map [remap delete-backward-char]
  'pt-minibuffer-delete-backward-char)

(defun pt-minibuffer-backward-kill-word (&optional arg)
  (interactive "p")
  (let ((pt (max (- (point) 1) (point-min))))
    (unless (get-text-property pt 'read-only)
      (backward-kill-word arg))))

(define-key minibuffer-local-map [remap backward-kill-word]
  'pt-minibuffer-backward-kill-word)

(defmacro pt-same-file-p (f1 f2)
  "Return t if two directory pathes are same."
  `(string-equal (file-name-as-directory (file-truename (expand-file-name ,f1)))
                 (file-name-as-directory (file-truename (expand-file-name ,f2)))))

(defun pt-rename-this-buffer-and-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive (list (if (buffer-file-name)
                         (read-file-name (format "Rename file '%s' to: " (buffer-name)))
                       (read-buffer (format "Rename buffer '%s' to: " (buffer-name))))))
  (let ((new-buffer-name (if (buffer-file-name)
                             (file-name-nondirectory new-name)
                           new-name))
        (this-file-name (buffer-file-name)))
    (if (and this-file-name
             (not (pt-same-file-p this-file-name new-name)))
        (when (or (and (file-exists-p new-name)
                       (y-or-n-p "Replace?"))
                  (not (file-exists-p new-name)))
          (rename-file this-file-name new-name t)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))
    (if (and (not (string-equal new-buffer-name (buffer-name)))
             (get-buffer new-buffer-name)
             (y-or-n-p (format "The buffer '%s' already exists.  Rename to '%s'?"
                               new-buffer-name
                               (generate-new-buffer-name new-buffer-name))))
        (rename-buffer (generate-new-buffer-name new-buffer-name)))))

(defmacro pt-add-hook (hook &rest form)
  "Add FORM to HOOK."
  `(add-hook ,hook (lambda () ,@form)))

(defun pt-kill-line-or-region (&optional arg)
  (interactive "P")
  (if (use-region-p)
      (kill-region (mark) (point))
    (kill-line arg)))

(global-set-key [remap kill-line] 'pt-kill-line-or-region)

(defun pt-kill-visual-line-or-region (&optional arg)
  (interactive "P")
  (if (use-region-p)
      (kill-region (mark) (point))
    (kill-visual-line arg)))

(add-hook 'visual-line-mode-hook
          (lambda ()
            (define-key visual-line-mode-map [remap kill-line] 'pt-kill-visual-line-or-region)))

(defvar pt-package-content-refreshed nil
  "An internal variable used to detect if packages information is
  refreshed")

(defun pt-install-package (package &optional min-version)
  (unless (package-installed-p package min-version)
    (unless pt-package-content-refreshed
      (setq pt-package-content-refreshed t)
      (package-refresh-contents))
    (package-install package)))

(defun pt-immediately-quit-term ()
  (set-process-sentinel
   (get-buffer-process (current-buffer))
   (lambda (proc event)
     (when (string-equal event "finished\n")
       (kill-this-buffer)))))

(add-hook 'term-exec-hook #'pt-immediately-quit-term)

(defun pt--advice-add-login-switch (args)
  "open term as login shell"
  (cond ((= (length args) 2)
         (append args '(nil "--login")))
        ((= (length args) 3)
         (append args '("--login")))
        (t args)))

(defun pt-open-ansi-term (&optional new)
  (interactive "P")
  (let ((shell-buffer "*ansi-term*")
        (shell-program
         (or (and (boundp 'explicit-shell-file-name)
                  explicit-shell-file-name)
             (getenv "ESHELL")
             (getenv "SHELL")
             "/bin/sh")))

    (if (fboundp 'advice-add)
        (advice-add 'term-ansi-make-term
                    :filter-args
                    #'pt--advice-add-login-switch))
    (if new
        (ansi-term shell-program)
      (if (get-buffer shell-buffer)
          (pop-to-buffer (get-buffer shell-buffer))
        (ansi-term shell-program)))

    (if (fboundp 'advice-remove)
        (advice-remove 'term-ansi-make-term
                       #'pt--advice-add-login-switch))))

(define-key ctl-x-map [?\M-t] 'pt-open-ansi-term)

(defun pt-whitespace-cleanup ()
  (interactive)
  (if (use-region-p)
      (whitespace-cleanup-region (point) (mark))
    (whitespace-cleanup))
  (message "Whitespace cleanup done"))

(define-key ctl-x-map (kbd "SPC") #'pt-whitespace-cleanup)

(provide 'pt-simple)

;; pt-simple ends here
