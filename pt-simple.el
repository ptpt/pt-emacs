;;; pt-simple.el --- Tao Peng's basic configuration for Emacs

;; Copyright (C) 2010, 2011 Tao Peng <pt@taopeng.me>

;; Author:   Tao Peng <pt@taopeng.me>
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

(require 'cl)


;; variables

(defvar pt-ignored-buffers
  '("*Help*" "*Completions*" "*Diff*" "*Messages*"
    "*Buffer List*" "*Apropos*")
  "Skip the buffers when `pt-next-buffer' and `pt-previous-buffer'.")


;;; functions
(defadvice message (around pt-inhibit-message compile)
  (ignore))

(defmacro pt-inhibit-message (&rest forms)
  `(progn
     (ad-activate-regexp "\\`pt-inhibit-message\\'")
     ,@forms
     (ad-deactivate-regexp "\\`pt-inhibit-message\\'")))

(defadvice mouse-drag-region
  (around pt-mouse-drag-region-no-cursor activate)
  (let ((old-cursor-type cursor-type))
    (setq cursor-type nil)
    ad-do-it
    (setq cursor-type old-cursor-type)))

(defmacro pt-make-directory-and-return (dir)
  "Create directory DIR if not found. Return DIR."
  `(progn (unless (file-directory-p ,dir)
            (make-directory ,dir t))
          ,dir))

(defun pt-add-subdirectories-to-list (&optional dir)
  "Add all subdirectories not starting with \".\" of DIR to `load-path'.
If DIR is nil, add `user-emacs-directory' instead."
  (mapc #'(lambda (file)
            (if (file-directory-p file)
                (add-to-list 'load-path file)))
        (directory-files (or dir user-emacs-directory) t "\\`[^.]")))

(defmacro pt-same-file-p (f1 f2)
  "Return t if two directory pathes are same."
  `(string-equal (downcase (file-name-as-directory (file-truename (expand-file-name ,f1))))
                 (downcase (file-name-as-directory (file-truename (expand-file-name ,f2))))))

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

(global-set-key (kbd "TAB") 'pt-tab-command)

(defun pt-beginning-of-line-or-text ()
  "Switch position between beginning of line and text."
  (interactive)
  (let ((pt (point)))
    (back-to-indentation)
    (when (= pt (point))
      (move-beginning-of-line nil))))

(global-set-key [?\C-a] 'pt-beginning-of-line-or-text)

(defun pt-delete-lines (&optional arg)
  "Delete lines like `delete-blank-lines'.
But if current line is not blank, it will `kill-whole-line'."
  (interactive "p")
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
current lines using `pt-delete-lines'."
  (interactive "p")
  (if mark-active
      (kill-region (mark) (point))
    (pt-delete-lines arg)))

(global-set-key [?\C-w] 'pt-kill-region-or-line)

(defun pt-keyboard-quit ()
  "Delete all windows that match `pt-ignored-buffers' and then call `keyboard-quit'"
  (interactive)
  (mapc #'(lambda (name)
            (dolist (window (cdr (window-list nil nil (selected-window))))
              (if (string-equal name (buffer-name (window-buffer window)))
                  (delete-window window))))
        pt-ignored-buffers)
  (keyboard-quit))

(global-set-key [escape] 'pt-keyboard-quit)

(defmacro pt-defun-treat-current-line-as-region (orig-function)
  `(defun ,(intern (concat "pt-" (symbol-name (eval orig-function)))) (&optional arg)
     ,(format
       "Like `%s', except acts on the current line if mark is not active."
       (eval orig-function))
     (interactive "p")
     (save-excursion
       (when (not mark-active)
         (beginning-of-line)
         (push-mark-command t t)
         (end-of-line arg))
       (call-interactively (quote ,(eval orig-function))))))

(mapc #'(lambda (command)
          (global-set-key (vector 'remap command)
                          (intern (concat "pt-" (symbol-name command))))
          (pt-defun-treat-current-line-as-region command))
      '(kill-ring-save
        kill-region
        comment-region
        comment-or-uncomment-region
        clipboard-kill-region
        clipboard-kill-ring-save))

(defun pt-hungry-delete-backwards ()
  "Delete backwards whitespaces."
  (interactive)
  (let ((here (point)))
    (skip-chars-backward " \t\n\r\f\v")
    (when (/= here (point))
      (delete-region (point) here))))

(when window-system
  (global-set-key [C-backspace] 'pt-hungry-delete-backwards))

(defun pt-hungry-delete-forwards ()
  "Delete forwards whitespaces."
  (interactive)
  (let ((here (point)))
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

(global-set-key [?\M-\\] 'pt-hungry-delete)

(defvar pt-new-buffer-hook nil
  "List of functions to be called after a new buffer is created by
  `pt-new-buffer'.")

(defvar pt-new-buffer-mode-alist nil
  "When create new buffer with prefix arg, the new buffer will use the
  mode corresponding to the arg in this list as major mode.")

(defvar pt-new-buffer-is-me nil
  "Non-nil means the buffer was created by `pt-new-buffer'. This kind of
  buffer will serve as a special buffer, e.g. Killing this buffer will
  be asked. It's for internal use and will be set automatically")

(put 'pt-new-buffer-is-me 'permanent-local t)
(make-variable-buffer-local 'pt-new-buffer-is-me)

(defun pt-new-buffer (&optional arg)
  "Create a new buffer."
  (interactive "P")
  (let ((mode major-mode))
    (switch-to-buffer (generate-new-buffer "untitled"))
    (funcall
     (cond ((numberp arg)
            (or
             (cdr (assoc (abs arg) pt-new-buffer-mode-alist))
             default-major-mode))
           ((consp arg)
            mode)
           (t default-major-mode)))
    (setq pt-new-buffer-is-me t)
    (if (or (and (numberp arg)
                 (>= arg 0))
            (and (consp arg)
                 (= 4 (car arg))))
        (run-hooks 'pt-new-buffer-hook))
    (set-buffer-modified-p nil)))

(define-key ctl-x-map [?\C-n] 'pt-new-buffer)
(global-set-key [(alt n)] 'pt-new-buffer)

(defun pt-new-buffer-query-funtion ()
  (if (and pt-new-buffer-is-me
           (not (buffer-file-name))
           (buffer-modified-p)
           (> (buffer-size) 0))
      (y-or-n-p (format "Buffer %s modified; kill anyway? "
                        (buffer-name)))
    t))

(add-hook 'kill-buffer-query-functions
          'pt-new-buffer-query-funtion)

(defun pt-buffer-file-name (&optional buffer)
  (or (and (buffer-file-name buffer)
           (file-name-nondirectory (buffer-file-name buffer)))
      (buffer-name buffer)))

(defmacro pt-xor (a b)
  "XOR logic operation."
  `(and (or ,a ,b) (not (and ,a ,b))))

(defun pt-next-buffer (&optional arg)
  "Switch to next buffer which is not in `pt-ignored-buffers'.
If ARG is non-nil, then switch between file-visted-buffer and
non-file-visted-buffer."
  (interactive "P")
  (let ((buffer (current-buffer))
        (new-buffer pt-new-buffer-is-me))
    (next-buffer)
    (setq arg (pt-xor (or new-buffer
                          (buffer-file-name buffer)) arg))
    (while (and (not (eq buffer (current-buffer)))
                (or (member (buffer-name)
                            pt-ignored-buffers)
                    (if arg (and (not pt-new-buffer-is-me)
                                 (not (buffer-file-name)))
                      (or pt-new-buffer-is-me (buffer-file-name)))))
      (next-buffer))))

(defun pt-previous-buffer (&optional arg)
  "Switch to previous buffer which is not in `pt-ignored-buffers'.
If ARG is non-nil, then switch between file-visted-buffer and
non-file-visted-buffer."
  (interactive "P")
  (let ((buffer (current-buffer))
        (new-buffer pt-new-buffer-is-me))
    (previous-buffer)
    (setq arg (pt-xor (or new-buffer
                          (buffer-file-name buffer)) arg))
    (while (and (not (eq buffer (current-buffer)))
                (or (member (buffer-name)
                            pt-ignored-buffers)
                    (if arg (and (not pt-new-buffer-is-me)
                                 (not (buffer-file-name)))
                      (or pt-new-buffer-is-me (buffer-file-name)))))
      (previous-buffer))))

(defun pt-beginning-or-end-of-buffer (&optional arg)
  (interactive "^P")
  (if (bobp)
      (end-of-buffer arg)
    (beginning-of-buffer arg)))

(defun pt-forward-whitespace (&optional arg)
  "Move point forward whitespace."
  (interactive "p")
  (re-search-forward
   "[^ \t\n\r\f$][ \t\n\r\f$]\\|\\'"
   nil nil arg)
  (unless (eobp)
    (backward-char)))

(defun pt-backward-whitespace (&optional arg)
  "Move point backward whitespace."
  (interactive "p")
  (re-search-backward
   "[ \t\n\r\f^][^ \t\n\r\f^]\\|\\`"
   nil nil arg)
  (unless (bobp)
    (forward-char)))

(global-set-key (kbd "M-e") 'pt-forward-whitespace)
(global-set-key (kbd "M-a") 'pt-backward-whitespace)

(defadvice kill-this-buffer
  (after pt-kill-this-buffer-and-switch-to-next-buffer
         activate)
  (when (and (called-interactively-p 'any)
             (and (null pt-new-buffer-is-me)
                  (null (buffer-file-name))))
    (pt-next-buffer 1)))

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

;; from internet
(defun pt-rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun pt-delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun pt-change-cursor-type ()
  (cond ((or buffer-read-only
             (get-text-property (point) 'read-only)
             (eq major-mode 'term-mode))
         (setq cursor-type 'box)
         (blink-cursor-mode -1))
        (t
         (setq cursor-type (default-value 'cursor-type))
         (blink-cursor-mode 1))))

(defun pt-split-window ()
  "Split window into 2 windows with different buffers."
  (interactive)
  (save-selected-window
    (select-window (split-window) t)
    (let ((pt-ignored-buffers pt-ignored-buffers))
      (mapc #'(lambda (w)
                (add-to-list 'pt-ignored-buffers
                             (buffer-name (window-buffer w))))
            (window-list))
      (pt-next-buffer))))

(defun pt-split-window-horizontally ()
  "Split window horizontally into 2 windows with different buffers."
  (interactive)
  (save-selected-window
    (select-window (split-window-horizontally) t)
    (let ((pt-ignored-buffers pt-ignored-buffers))
      (mapc #'(lambda (w)
                (add-to-list 'pt-ignored-buffers
                             (buffer-name (window-buffer w))))
            (window-list))
      (pt-next-buffer))))

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

(define-key ctl-x-map "2" 'pt-split-window)
(define-key ctl-x-map "3" 'pt-split-window-horizontally)
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
          #'(lambda ()
              (let ((dir (file-name-directory (buffer-file-name))))
                (and (not (file-directory-p dir))
                     (y-or-n-p (format "%s: no such directory; create? " dir))
                     (make-directory dir t)))))

(defvar pt-binary-range '(0 . 0))

(defun pt-binary-previous-line (&optional arg)
  (interactive "P")
  (when (or arg
            (not (memq last-command
                       '(pt-binary-next-line
                         pt-binary-previous-line))))
    (save-excursion
      (let ((end (point)))
        (setcdr pt-binary-range (line-number-at-pos))
        ;; (move-to-window-line 0)
        (goto-char (window-start))
        (setcar pt-binary-range (- (cdr pt-binary-range)
                                   (count-screen-lines (point) end))))))
  (let ((lines (max 1 (ceiling (/ (- (cdr pt-binary-range)
                                     (car pt-binary-range)) 2.0)))))
    (setcdr pt-binary-range (- (cdr pt-binary-range) lines))
    (previous-line lines)))

(defun pt-binary-next-line (&optional arg)
  (interactive "P")
  (when (or arg
            (not (memq last-command
                       '(pt-binary-next-line
                         pt-binary-previous-line))))
    (save-excursion
      (let ((start (point)))
        (setcar pt-binary-range (line-number-at-pos))
        ;; (move-to-window-line -1)
        (goto-char (window-end))
        (setcdr pt-binary-range (+ (count-screen-lines start (point))
                                   (car pt-binary-range))))))
  (let ((lines  (max 1 (ceiling (/ (- (cdr pt-binary-range)
                                      (car pt-binary-range)) 2.0)))))
    (setcar pt-binary-range (+ (car pt-binary-range) lines))
    (next-line lines)))

(global-set-key [?\M-p] 'pt-binary-previous-line)
(global-set-key [?\M-n] 'pt-binary-next-line)

(defvar pt-ido-recentf-list nil)

(defun pt-ido-find-recentf ()
  "Find recently opened files using ido-mode."
  (interactive)
  (if (fboundp 'pt-inhibit-message)
      (pt-inhibit-message
       (recentf-cleanup)))
  (setq pt-ido-recentf-list nil)
  (let (records suffix)
    (dolist (file recentf-list)
      (let* ((f (file-name-nondirectory file))
             (match (assoc f records)))
        (if match
            (setcdr match (1+ (cdr match)))
          (add-to-list 'records (cons f 1)))
        (setq suffix
              (if (cdr match)
                  (format "<%d>" (cdr match))
                ""))
        (add-to-list 'pt-ido-recentf-list
                     (cons (concat f suffix) file) t))))
  (let ((filename
         (ido-completing-read
          "Open recent: "
          (mapcar 'car pt-ido-recentf-list)
          nil t)))
    (let ((filename (assoc filename pt-ido-recentf-list)))
      (when filename
        (find-file (cdr filename))))))

(defun pt-ido-kill-recentf ()
  "Kill the recent file at the head of `ido-matches'
and remove it from `recentf-list'. If cursor is not at
the end of the user input, delete to end of input."
  (interactive)
  (if (not (eobp))
      (delete-region (point) (line-end-position))
    (let ((enable-recursive-minibuffers t)
          (file (ido-name (car ido-matches))))
      (when file
        (setq recentf-list
              (delq (cdr (assoc file pt-ido-recentf-list)) recentf-list))
        (setq pt-ido-recentf-list
              (delq (assoc file pt-ido-recentf-list) pt-ido-recentf-list))
        (setq ido-cur-list
              (delq file ido-cur-list))))))

(add-hook 'ido-setup-hook
          #'(lambda ()
              (define-key ido-common-completion-map [?\C-k] 'pt-ido-kill-recentf)))
(define-key ctl-x-map "\C-r" 'pt-ido-find-recentf)

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

(global-set-key [?\M-c] 'pt-capitalize-word-or-region)
(global-set-key [?\M-u] 'pt-upcase-word-or-region)
(global-set-key [?\M-l] 'pt-downcase-word-or-region)

(defun pt-switch-buffer (&optional arg)
  (interactive "P")
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
  (unless (get-text-property (- (point) 1) 'read-only)
    (delete-backward-char arg)))

(define-key minibuffer-local-map [backspace]
  'pt-minibuffer-delete-backward-char)

(provide 'pt-simple)
;; pt-simple ends here
