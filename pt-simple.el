;;; pt-simple.el --- Tao Peng's basic configuration for Emacs

;; Copyright (C) 2010 Tao Peng <ptpttt@gmail.com>

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

(require 'cl)


;; variables
(defvar ido-recentf-list nil
  "internal use")
(defvar pt-emacs-tmp-directory "~/.emacs.tmp/"
  "Directory path that stores temporary files like auto-save files, backups
  etc.")

(defvar pt-fonts nil
  "Fonts list, in decreasing order of preference.")

(defvar pt-new-buffer-hook nil
  "List of functions to be called after a new buffer is created by
  `pt-new-buffer'.")

(defvar pt-custom-global-map (make-sparse-keymap)
  "Your own global-map.")
(global-set-key [?\C-x ?g] pt-custom-global-map)

(defvar pt-ignore-buffer-list
  '("*Help*" "*Completions*" "*Diff*" "*Messages*"
    "*Buffer List*" "*Apropos*")
  "Skip buffers in the list when `pt-next-buffer' and `pt-previous-buffer'.")

(defconst pt-emacs-name-and-version
  (let ((version-int (number-to-string emacs-major-version)))
    (concat "emacs-" version-int))
  "The name-version of this Emacs.")

(defvar pt-new-buffer-mode-alist nil
  "When create new buffer with prefix arg, the new buffer will use the
  mode corresponding to the arg in this list as major mode.")

(defvar pt-color-theme-list nil
  "Color theme list.")

(defvar pt-find-font-function #'pt-find-font
  "Function for finding font.")

(defvar pt-new-buffer-is-me nil
  "Non-nil means the buffer was created by `pt-new-buffer'. This kind of
  buffer will serve as a special buffer, e.g. Killing this buffer will
  be asked. It's for internal use and will be set automatically")

(put 'pt-new-buffer-is-me 'permanent-local t)
(make-variable-buffer-local 'pt-new-buffer-is-me)

(defvar pt-choose-color-theme-function
  'pt-choose-color-theme-by-second
  "Function that returns a color theme.")

(defvar pt-binary-range '(0 . 0))

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

(defmacro pt-get-directory-create (dir)
  "Create directory DIR if not found.
Return DIR."
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

(defmacro pt-directory-equal (dir1 dir2)
  "Return t if two directory pathes are same."
  `(string-equal (file-name-as-directory
                  (downcase (expand-file-name
                             ,dir1)))
                 (file-name-as-directory
                  (downcase (expand-file-name
                             ,dir2)))))

(defun pt-find-font ()
  "Return the first available font listed in `pt-fonts'."
  (let ((family-list (font-family-list)))
    (find-if (lambda (font)
               (member (symbol-name
                        (font-get (font-spec :name font) :family))
                       (font-family-list)))
             pt-fonts)))



(defun pt-set-font (&optional font)
  "Figure out and install which font and size I use on this system.
If called interactively, prompts the user for the font and size to use."
  (interactive
   (list (completing-read (format "Font: ")
                          (font-family-list))))
  (let* ((font (or font (funcall pt-find-font-function))))
    (when font
      (add-to-list 'default-frame-alist (cons 'font font))
      (set-frame-font font))))

(defun pt-choose-color-theme-by-second ()
  "Choose a valid color theme in `pt-color-theme-list' according to
current second."
  (if pt-color-theme-list
   (nth
    (mod (car (decode-time (current-time)))
         (length pt-color-theme-list))
    pt-color-theme-list)
   nil))

(defun pt-set-color-theme ()
  "Set color theme by `pt-choose-color-theme-function'."
  (interactive)
  (let ((pt-color-theme-list pt-color-theme-list) color-theme)
    (while pt-color-theme-list
      (setq color-theme
            (funcall (or pt-choose-color-theme-function 'ignore)))
      (if (condition-case nil
              (progn (cond
                      ((listp color-theme)
                       (eval color-theme))
                      ((symbolp color-theme)
                       (require color-theme)
                       (funcall color-theme))
                      (t (error "Color theme not found")))
                     t)
            (error nil))
          (setq pt-color-theme-list nil)
        (remove color-theme pt-color-theme-list))
      (setq pt-color-theme-list nil))))

;; (add-hook 'emacs-startup-hook #'pt-set-color-theme)
(add-hook 'window-setup-hook
          #'(lambda ()
              (if window-system
                  (pt-set-color-theme))))

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

(defun pt-beginning-of-line-or-text (&optional arg)
  "Switch position between beginning of line and text."
  (interactive "p")
  (let ((pt (point)))
    (beginning-of-line-text arg)
    (when (= pt (point))
      (beginning-of-line arg))))

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
        (save-excursion
          (move-beginning-of-line 1)
          (kill-whole-line arg))
        (move-to-column col)))))

(defun pt-kill-region (&optional arg)
  "If mark is active, act `kill-region' as normal, otherwise delete
current lines using `pt-delete-lines'."
  (interactive "p")
  (if mark-active
      (kill-region (point) (mark))
    (pt-delete-lines arg)))

(defadvice keyboard-quit (before delete-windows activate)
  "Delete all matched window before really `keyboard-quit'"
  (mapc #'(lambda (name)
            (dolist (window (cdr (window-list nil nil (selected-window))))
              (if (string-equal name (buffer-name (window-buffer window)))
                  (delete-window window))))
        pt-ignore-buffer-list))

;; act on lines if mark is not active
(defmacro pt-defadvice-current-line-as-region (orig-function)
  `(defadvice ,(eval orig-function) (before current-line-as-region activate compile)
     ,(format
       "Like `%s', except acts on the current line if mark is not active."
       (eval orig-function))
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (line-beginning-position)
              (line-beginning-position
               (+ 1 (prefix-numeric-value current-prefix-arg))))))))

(mapc #'(lambda (command)
          (pt-defadvice-current-line-as-region command))
      '(kill-ring-save
        kill-region
        comment-region
        comment-or-uncomment-region
        upcase-region
        downcase-region
        clipboard-kill-region
        clipboard-kill-ring-save))

(defun pt-hungry-delete-backwards ()
  "Delete backwards whitespaces."
  (interactive)
  (let ((here (point)))
    (skip-chars-backward " \t\n\r\f\v")
    (when (/= here (point))
      (delete-region (point) here))))

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
  "xor logic operation."
  `(and (or ,a ,b) (not (and ,a ,b))))

(defun pt-next-buffer (&optional arg)
  "Switch to next buffer which is not in `pt-ignore-buffer-list'.
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
                            pt-ignore-buffer-list)
                    (if arg (and (not pt-new-buffer-is-me)
                                 (not (buffer-file-name)))
                      (or pt-new-buffer-is-me (buffer-file-name)))))
      (next-buffer))))

(defun pt-previous-buffer (&optional arg)
  "Switch to previous buffer which is not in `pt-ignore-buffer-list'.
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
                            pt-ignore-buffer-list)
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

(defadvice kill-this-buffer
  (after pt-kill-this-buffer-and-switch-to-next-buffer
          activate)
  (when (and (called-interactively-p 'any)
             (and (null pt-new-buffer-is-me)
                  (null (buffer-file-name))))
    (pt-next-buffer 1)))

;; (defun pt-switch-buffer (&optional arg)
;;   "C-u M-x `pt-switch-buffer' switchs between file-visited-buffer and
;; non-file-visited-buffer;
;; C-u C-u M-x `pt-switch-buffer' switchs previous
;; buffer."
;;   (interactive "P")
;;   (if (or (and (numberp arg) (= arg 2))
;;           (and (consp arg) (= (car arg) 16)))
;;       (pt-previous-buffer)
;;     (pt-next-buffer arg)))

(when (eq 'darwin system-type)
  (defun pt-pbpaste ()
    "Paste data from pasteboard."
    (interactive)
    (shell-command-on-region
     (point)
     (if mark-active (mark) (point))
     "pbpaste" nil t))

  (defun pt-pbcopy ()
    "Copy region to pasteboard."
    (interactive)
    (when mark-active
      (shell-command-on-region
       (point) (mark) "pbcopy")
      (kill-buffer "*Shell Command Output*")))

  (global-set-key [?\C-x ?\C-y] 'pt-pbpaste)
  (global-set-key [?\C-x ?\M-w] 'pt-pbcopy))

(defun pt-read-env-from-profile (profile)
  (let (count env)
    (setq profile (expand-file-name profile))
    (with-temp-buffer
      (when (and (file-exists-p profile)
                 (= 0 (call-process-shell-command
                       (format "source \"%s\" && env && env | wc -l" profile)
                       nil (buffer-name))))
        (goto-char (point-max))
        (when (re-search-backward "^\\s-*[0-9]+\\s-*$" nil t)
          (setq count (match-string-no-properties 0)))
        (when count
          (setq count (string-to-number count))
          (beginning-of-line (- count))
          (while (re-search-forward
                  "\\([a-zA-Z_]+\\)=\\([^\n]*\\)\n" nil t)
            (add-to-list 'env
                         (cons (match-string-no-properties 1)
                               (match-string-no-properties 2))))
          env)))))

(defun pt-read-env-from-paths.d ()
  (let (env (paths ""))
    (with-temp-buffer
      (when (and (file-exists-p "/etc/paths.d/")
                 (= 0 (call-process-shell-command
                       "cat /etc/paths.d/*" nil (buffer-name))))
        (goto-char (point-min))
        (while (re-search-forward "\\(.*\\)\n" nil t)
          (add-to-list 'env
                       (cons "PATH"
                             (match-string-no-properties 1))))))
    env))

(defun pt-setenv-path-from-system ()
  "Set the value of PATH variable from system."
  (let ((env (getenv "PATH")) result)
    (setq env (nconc (pt-read-env-from-profile "~/.bash_profile")
                     (pt-read-env-from-paths.d)))
    (dolist (e env result)
      (if (string= "PATH" (car e))
          (setq result (concat result (if result ":") (cdr e)))))
    (setenv "PATH" result)))

;; from internet
(defun pt-rename-file-and-buffer (new-name)
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

;; from internet
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

;; window settings
(defun pt-split-window ()
  "Split window into 2 windows with different buffers."
  (interactive)
  (save-selected-window
    (select-window (split-window) t)
    (let ((pt-ignore-buffer-list pt-ignore-buffer-list))
      (mapc #'(lambda (w)
                (add-to-list 'pt-ignore-buffer-list
                             (buffer-name (window-buffer w))))
            (window-list))
      (pt-next-buffer))))

(defun pt-split-window-horizontally ()
  "Split window horizontally into 2 windows with different buffers."
  (interactive)
  (save-selected-window
    (select-window (split-window-horizontally) t)
    (let ((pt-ignore-buffer-list pt-ignore-buffer-list))
      (mapc #'(lambda (w)
                (add-to-list 'pt-ignore-buffer-list
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

(defun pt-delete-directory-recursion (dir)
  "Delete empty directories recursively."
  (let ((dirs (directory-files dir t "\\`[^.]")))
    (cond ((and (= 1(length dirs))
                (file-directory-p (car dirs)))
           (and (pt-delete-directory-recursion (car dirs))
                (or (delete-directory dir) t)))
          ((= 0 (length dirs))
           (delete-directory dir) t)
          (t nil))))

(defun pt-get-directory-create-recursively (dir)
  "Create directory DIR and return the a list of all parent directories
that are needed to create."
  (unless (file-exists-p dir)
    (append (pt-get-directory-create-recursively (expand-file-name ".." dir))
            (cons (progn (make-directory dir) (file-name-as-directory dir)) nil))))

(defadvice save-buffer (around pave-path activate)
  (if (and (not (buffer-file-name))
           (called-interactively-p 'any))
      (condition-case err
          ad-do-it
        (error
         (let ((filename (and (listp file-name-history)
                              (car file-name-history))))
           (if (and (string-match "\\(.*\\): no such directory\\'" (cadr err))
                    (stringp filename)
                    (string-equal (match-string 1 (cadr err))
                                  (file-name-directory (expand-file-name filename)))
                    (y-or-n-p (format "%s; create? " (cadr err))))
               (let* ((dirs (pt-get-directory-create-recursively
                             (file-name-directory filename)))
                      (buffer-name (buffer-name)))
                 (condition-case err2
                     (progn (set-visited-file-name filename)
                            ad-do-it)
                   (error
                    (set-visited-file-name nil)
                    (rename-buffer buffer-name)
                    (mapc 'delete-directory (reverse dirs))
                    (signal (car err2) (cdr err2)))))
             (signal (car err) (cdr err))))))
    ad-do-it))

(add-hook 'write-file-functions ;; create file when the file doesn't exist
          #'(lambda ()
              (let ((dir (file-name-directory (buffer-file-name))))
                (and (not (file-directory-p dir))
                     (y-or-n-p (format "%s: no such directory; create? " dir))
                     (make-directory dir t)))))

(defun pt-binary-previous-line (&optional arg)
  (interactive "P")
  (when (or arg
            (not (memq last-command
                       '(pt-binary-next-line
                         pt-binary-previous-line))))
    (save-excursion
      (let ((end (point)))
        (setcdr pt-binary-range (line-number-at-pos))
        (move-to-window-line 0)
        (setcar pt-binary-range (- (cdr pt-binary-range)
                                   (count-lines (point) end))))))
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
        (move-to-window-line -1)
        (setcdr pt-binary-range (+ (count-lines start (point))
                                   (car pt-binary-range))))))
  (let ((lines  (max 1 (ceiling (/ (- (cdr pt-binary-range)
                                      (car pt-binary-range)) 2.0)))))
    (setcar pt-binary-range (+ (car pt-binary-range) lines))
    (next-line lines)))

(defun pt-ido-kill-recentf-at-head ()
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
              (delq (cdr (assoc file ido-recentf-list)) recentf-list))
        (setq ido-recentf-list
              (delq (assoc file ido-recentf-list) ido-recentf-list))
        (setq ido-cur-list
              (delq file ido-cur-list))))))

(defun pt-recentf-ido-find-file ()
  "Find recently opened files using ido-mode."
  (interactive)
  (if (fboundp 'pt-inhibit-message)
      (pt-inhibit-message
       (recentf-cleanup)))
  (setq ido-recentf-list nil)
  (let (records suffix)
    (dolist (file recentf-list)
      (let* ((f (file-name-nondirectory file))
             (match (assoc f records)))
        (if match
            (setcdr match (1+ (cdr match)))
          (add-to-list 'records
                       (cons f 1)))
        (setq suffix
              (if (cdr match)
                  (format "<%d>" (cdr match))
                ""))
        (add-to-list 'ido-recentf-list
                     (cons (concat f suffix) file)
                     t))))              ;append
  (let ((filename
         (ido-completing-read
          "Open recent: "
          (mapcar 'car ido-recentf-list)
          nil t)))
    (when filename
      (find-file
       (cdr (assoc filename
                   ido-recentf-list))))))

(defun capitalize-word-or-region (&optional arg)
  (interactive "p")
  (if mark-active
      (capitalize-region (mark) (point))
    (capitalize-word arg)))

(defun upcase-word-or-region (&optional arg)
  (interactive "p")
  (if mark-active
      (upcase-region (mark) (point))
    (upcase-word arg)))

(defun downcase-word-or-region (&optional arg)
  (interactive "p")
  (if mark-active
      (downcase-region (mark) (point))
    (downcase-word arg)))

(defun pt-switch-buffer (&optional arg)
  (interactive "P")
  (if (not (one-window-p))
      (other-window (or arg 1))
    (progn
      (if (consp arg)
          (split-window))
      (switch-to-buffer (other-buffer (current-buffer) t)))))

(defun shrink-frame-if-larger-than-buffer (&optional frame)
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


;; basic settings

;; (require 'ido nil t)
(ido-mode 1)
(if (boundp 'pt-ignore-buffer-list)
    (mapc #'(lambda (buffer)
              (add-to-list 'ido-ignore-buffers buffer))
          pt-ignore-buffer-list))

(setq ido-enable-dot-prefix t)
(setq ido-enable-flex-matching t)
(setq ido-execute-command-cache nil)
;; do not confirm a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)
;; (ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
;; tramp problem solved here
;; ido-enable-tramp-completion should be t (default)
;; (setq ido-enable-tramp-completion t)

;; but set it to nil to avoid initial message when ido-find-file
;; (setq ido-enable-tramp-completion nil)

(setq ido-use-filename-at-point 'guess)
(setq ido-enable-last-directory-history nil)
(setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
(setq ido-show-dot-for-dired t) ;; put . as the first item
;; (setq ido-use-filename-at-point t) ;; prefer file names near point)
(setq ido-save-directory-list-file
      (expand-file-name "ido.last" pt-emacs-tmp-directory))

(add-hook 'post-command-hook 'pt-change-cursor-type)

(show-paren-mode 1)

(setq-default save-place t)
(require 'saveplace)

(when pt-emacs-tmp-directory
  (pt-get-directory-create pt-emacs-tmp-directory)
  
  (setq recentf-save-file
        (expand-file-name "recenf" pt-emacs-tmp-directory))
  
  (setq bookmark-default-file
        (expand-file-name
         "bookmarks"
         pt-emacs-tmp-directory))

  (setq auto-save-list-file-prefix
        (concat (pt-get-directory-create
                 (expand-file-name
                  (file-name-as-directory "auto-save-list")
                  pt-emacs-tmp-directory))
                "save-list-"))

  ;; save auto-save files under `pt-emacs-tmp-directory'/auto-saves/
  (add-to-list 'auto-save-file-name-transforms
               (list "\\`\\(/[^/]+\\)*/\\([^/]+\\)\\'"
                     (concat (pt-get-directory-create
                              (expand-file-name
                               (file-name-as-directory "auto-saves")
                               pt-emacs-tmp-directory))
                             "\\2")
                     t))

  ;; save backups under `pt-emacs-tmp-directory'/backups/
  (setq backup-directory-alist
        `((".*" . ,(pt-get-directory-create
                    (expand-file-name
                     (file-name-as-directory "backups")
                     pt-emacs-tmp-directory)))))

  ;; remember cursor position
  (setq save-place-file
        (expand-file-name
         "emacs-places" pt-emacs-tmp-directory)))

(setq recentf-auto-cleanup 'never)
(setq recentf-max-menu-items 25)
(recentf-mode 1)
;; set distributed elisp files read-only
(when (eq window-system 'ns)
  (add-to-list 'recentf-exclude
               "/.+\\.app/Contents/Resources/.*\\.el"))

;; if it's not tty
(unless (tty-type)
  (add-hook 'emacs-startup-hook 'pt-set-font)
  (when (fboundp 'text-scale-increase)
    (global-set-key (kbd "M-=") 'text-scale-increase)
    (global-set-key (kbd "M--") 'text-scale-decrease)))

;; (add-to-list 'default-frame-alist
;;              '(wait-for-wm . nil))

(setq initial-frame-alist default-frame-alist)

(when (display-graphic-p)
  (setq frame-title-format
        '((buffer-file-name
           "%f [%I]"
           "%b [%I]")))
  (scroll-bar-mode -1)
  ;; (when (fboundp 'tool-bar-mode)
  ;;   (tool-bar-mode -1)
  ;;   (add-to-list 'default-frame-alist '(tool-bar-lines . 0)))

  (when (featurep 'tooltip)
    (setq tooltip-gud-tips-p t)))

(when (display-graphic-p)
  (setq focus-follows-mouse (eq window-system 'x)
        mouse-autoselect-window t)

  (setq-default mouse-yank-at-point t)

  (cond ((fboundp 'mouse-wheel-mode)
         (mouse-wheel-mode 1))
        ((locate-library "mwheel")
         (unless (fboundp 'mwheel-install)
           (autoload 'mwheel-install "mwheel" nil nil))
         (setq mwheel-follow-mouse t)
         (setq mwheel-scroll-amount '(4 . 1))
         (mwheel-install))))

(when (eq 'ns (window-system))
  (setq ns-pop-up-frames nil)) ; NSEmacs (Emacs.app, aqua, from NeXT Emacs)

(setq inhibit-startup-message t)

(setq ring-bell-function #'ignore
      visible-bell nil)

;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (setq custom-unlispify-tag-names       nil
;;       custom-unlispify-menu-entries    nil
;;       custom-unlispify-remove-prefixes nil)

(setq custom-file
      (expand-file-name
       (concat pt-emacs-name-and-version "-custom.el")
       user-emacs-directory))

;; (load custom-file)

(when (eq window-system 'ns)
  ;; been set at emacs-startup-hook
  (setq ns-alternate-modifier 'meta)
  (setq ns-command-modifier 'alt))

;; set distributed elisp files read-only
(when (eq window-system 'ns)
  (add-hook 'find-file-hook
            '(lambda ()
               (let ((case-fold-search t))
                 (if (string-match
                      "/.+\\.app/Contents/Resources/.*\\.el"
                      (buffer-file-name))
                     (toggle-read-only t))))))

;;automatically revert buffer while file on disk changes
(global-auto-revert-mode 1)

;; this can fix next-line vertical move problem
(setq line-move-visual t)

(setq scroll-preserve-screen-position t)

(setq-default tab-width 4)

(setq cua-enable-cua-keys nil)
(cua-mode 1)
(define-key cua-global-keymap [(control return)] nil)

;; don't add new line at the end of buffer.
(setq require-final-newline nil)
(setq inhibit-default-init  t)

(fset #'yes-or-no-p #'y-or-n-p)

;;don't insert tabs while indent.
(setq-default indent-tabs-mode nil)

;;don't truncate lines in windows which are smaller than frame.
(setq truncate-partial-width-windows nil)

(setq-default truncate-lines t)
(let ((foo #'(lambda () (setq truncate-lines nil))))
  (mapc #'(lambda (hook)
            (add-hook hook foo))
        '(term-mode-hook eshell-mode-hook html-mode-hook)))

(setq-default scroll-step              1
              scroll-conservatively    most-positive-fixnum
              scroll-up-aggressively   0.0
              scroll-down-aggressively 0.0)

(when (fboundp 'temp-buffer-resize-mode)
  (temp-buffer-resize-mode 1))

(when (require 'uniquify nil t)
  (setq-default uniquify-buffer-name-style 'forward))

(column-number-mode 1)

(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; try second case-insensitive pass through `auto-mode-alist'
(setq auto-mode-case-fold t)

;; show more apropos
(setq apropos-do-all t)

(setq completion-ignore-case t)

(setq-default case-fold-search t)

(setq-default abbrev-mode t)
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

(setq-default cursor-type '(bar . 1))

;; turn off auto fill
(mapc #'(lambda (mode-hook)
          (add-hook mode-hook 'turn-off-auto-fill))
      '(emacs-lisp-mode-hook
        sh-mode-hook comint-mode-hook
        shell-mode-hook lisp-mode-hook erc-mode-hook ruby-mode-hook))

(setq-default fill-column 72)
(setq emacs-lisp-docstring-fill-column 72)

(setq message-log-max most-positive-fixnum)

(add-hook 'write-file-functions 'time-stamp)

;; url and mail address recognition
(when (fboundp 'goto-address)
  (setq goto-address-fontify-maximum-size most-positive-fixnum)
  (add-hook 'find-file-hook 'goto-address))

(setq tramp-default-method "ssh")

(when (tty-type)
  (defun ted-delete-frame-or-kill-emacs ()
    (interactive)
    (if (cdr (frame-list)) ; (> (length (frame-list)) 1)
        (delete-frame)
      (save-buffers-kill-emacs)))
  (global-set-key (kbd "C-x C-c") 'ted-delete-frame-or-kill-emacs))

(find-function-setup-keys)

;; forward-sexp, etc., should treat comments as whitespace
(setq parse-sexp-ignore-comments t)

;; follow symbolic link.
(setq vc-follow-symlinks t)

(when (require 'generic-x nil t)
  (setq default-major-mode 'default-generic-mode))

;; completion in M-:
(when (keymapp read-expression-map)
  (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol))

;; `eldoc'. echo documents about function or variable at the point
(mapc (lambda (mode-hook)
          (add-hook mode-hook 'turn-on-eldoc-mode))
        '(emacs-lisp-mode-hook lisp-interaction-mode-hook
                               ielm-mode-hook))

(defun help-default-arg-highlight (arg)
    "Upcase and fontify ARG for use with `eldoc-mode' and help."
    (propertize (upcase arg)
                'face 'font-lock-variable-name-face))
(setq eldoc-argument-case 'help-default-arg-highlight)

(define-key pt-custom-global-map "d" 'diff-buffer-with-file)
(setq diff-switches "-u")

(when (eq window-system 'ns)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser))

;; minibuffer settings
(setq enable-recursive-minibuffers t)

;; default value at point
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-map [?\C-x escape] 'abort-recursive-edit)

;; icomplete-mode
(autoload 'icomplete-mode "icomplete"
  "Toggle incremental minibuffer completion" t)
(icomplete-mode t)

;; kill completion buffer when exit minibuffer
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (ignore-errors
               (kill-buffer "*Completions*"))))

;; get rid of the annoying error message "text is read-only"
(defun pt-minibuffer-delete-backward-char (&optional arg)
  (interactive "p")
  (unless (get-text-property (- (point) 1) 'read-only)
    (delete-backward-char arg)))

(define-key minibuffer-local-map [backspace]
  'pt-minibuffer-delete-backward-char)


;;; global key bindings
(global-set-key [f2] ctl-x-map)

;; window bindings
(define-key ctl-x-map "2" 'pt-split-window)
(define-key ctl-x-map "3" 'pt-split-window-horizontally)
(winner-mode 1)
(define-key ctl-x-map "7" 'pt-swap-windows)
(define-key ctl-x-map "9" 'winner-undo)

(when (eq 'ns window-system)
  (global-set-key [?\A-n] 'pt-new-buffer)
  (global-set-key [?\A-\M-,]
                  #'(lambda ()
                      (interactive)
                      (find-file user-init-file))))

(global-set-key [f1 ?j] 'elisp-index-search)
(global-set-key (kbd "TAB") 'pt-tab-command)
(global-set-key [?\M-m] 'set-mark-command)
(when (fboundp 'cua-set-rectangle-mark)
  (global-set-key (kbd "M-M") 'cua-set-rectangle-mark))
(global-set-key [?\C-a] 'pt-beginning-of-line-or-text)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [?\M-p] 'pt-binary-previous-line)
(global-set-key [?\M-n] 'pt-binary-next-line)
(global-set-key [escape] 'keyboard-quit)
(define-key ctl-x-map [escape] 'keyboard-quit)

(autoload 'dired-jump "dired-x" "Jump to dired corresponding current buffer.")
(autoload 'dired-jump-other-window "dired-x"
  "jump to dired corresponding current buffer in other window.")
(global-set-key [?\C-x ?\C-d] 'dired-jump-other-window)

(global-set-key (kbd "C-j")
                #'(lambda ()
                    (interactive)
                    (end-of-line)
                    ;; (comment-indent-new-line)
                    (newline-and-indent)))

(global-set-key (kbd "M-j")
                #'(lambda ()
                    (interactive)
                    (beginning-of-line)
                    (split-line)))

(define-key ctl-x-map [?\C-g] 'keyboard-quit)
(define-key ctl-x-map "k" 'kill-this-buffer)
(define-key ctl-x-map "f" 'find-file)

;; smartly comment or uncomment line/region
(define-key ctl-x-map (kbd "/") 'comment-or-uncomment-region)
(define-key ctl-x-map "l" 'find-library)
(define-key ctl-x-map (kbd "M-m") 'pop-global-mark)
(define-key ctl-x-map [f2] 'other-window)
(define-key ctl-x-map [?p] 'pop-to-mark-command)
(define-key ctl-x-map [?\C-n] 'pt-new-buffer)

(when (eq 'ns window-system)
  (global-set-key [ns-drag-file] 'ns-find-file))
(global-set-key [?\C-w] 'pt-kill-region)
(global-set-key (kbd "M-`") 'pt-switch-buffer)
(global-set-key [?\M-\\] 'pt-hungry-delete)
(global-set-key (kbd "M-]") 'end-of-defun)
(global-set-key (kbd "M-[") 'beginning-of-defun)
(add-hook 'Info-mode-hook
          #'(lambda ()(define-key Info-mode-map (kbd "M-n") nil)))

(when window-system
  (global-set-key [C-backspace] 'pt-hungry-delete-backwards))

(global-set-key (kbd "M-a") 'pt-backward-whitespace)
(global-set-key (kbd "M-e") 'pt-forward-whitespace)

(define-key pt-custom-global-map (kbd "SPC")
  #'(lambda ()
      (interactive)
      (if mark-active
          (whitespace-cleanup-region (point) (mark))
        (whitespace-cleanup))))

(global-set-key [M-right] 'windmove-right)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

(define-key ctl-x-map "t" 'ansi-term)

(provide 'pt-simple)
;; pt-simple ends here