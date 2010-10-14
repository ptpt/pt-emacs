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


;; intitize etc.
(defvar pt-emacs-tmp-directory "~/.emacs.tmp/"
  "Directory path that stores temporary files like auto-save files, backups
  etc.")

(defvar pt-emacs-data-directory "~/.emacs.data/"
  "Directory path that stores data file such as snippets, templates
  etc.")

(defvar pt-fonts nil
  "Fonts list, in decreasing order of preference.")

(defvar pt-font-size 12
  "My preferred font size.
Can be overridden in `local-elisp-config'.")

(defvar pt-emacs-server t
  "If non-null, this emacs should run an edit server.
By edit server, I mean the bit that emacsclient or gnuclient talk to.")

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

(defvar pt-after-init-functions nil)
(add-to-list 'load-path
             (expand-file-name user-emacs-directory))

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

;; Add all sub-directories under `user-emacs-directory' to `load-path'.
(pt-add-subdirectories-to-list)

(setq inhibit-startup-message t
      initial-major-mode 'lisp-interaction-mode)

(setq ring-bell-function 'ignore
      visible-bell nil)

;; don't add new line at the end of buffer.
(setq require-final-newline nil)
(setq inhibit-default-init  t)

(defalias 'yes-or-no-p 'y-or-n-p)

;;don't insert tabs while indent.
(setq-default indent-tabs-mode nil)

;;don't truncate lines in windows which are smaller than frame.
(setq truncate-partial-width-windows nil)

(setq-default truncate-lines t)
(let ((foo #'(lambda () (setq truncate-lines nil))))
  (mapc #'(lambda (hook)
            (add-hook hook foo))
        '(term-mode-hook eshell-mode-hook html-mode-hook)))

(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

(setq-default scroll-step              1
              scroll-conservatively    most-positive-fixnum
              scroll-up-aggressively   0.0
              scroll-down-aggressively 0.0)

(when (fboundp 'temp-buffer-resize-mode)
  (temp-buffer-resize-mode 1))

(when (require 'uniquify nil t)
  (setq-default uniquify-buffer-name-style 'forward))

(when (fboundp 'column-number-mode)
  (column-number-mode 1))

(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; try second case-insensitive pass through `auto-mode-alist'
(setq auto-mode-case-fold t)

;; show more apropos
(setq apropos-do-all t)

(setq completion-ignore-case t)

;; (when (locate-library "pcomplete")
;;   (setq pcomplete-cycle-completions nil))

;; (when (locate-library "longlines")
;;   (setq longlines-wrap-follows-window-size t))

(setq-default case-fold-search t)

(setq-default abbrev-mode t)
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

(when (fboundp 'show-paren-mode)
  (show-paren-mode 1)
  (make-variable-buffer-local 'show-paren-mode))

(setq blink-matching-paren-on-screen nil)
(setq blink-matching-delay 0.125)

;; turn off auto fill
(mapc #'(lambda (mode-hook)
          (add-hook mode-hook 'turn-off-auto-fill))
      '(emacs-lisp-mode-hook
        sh-mode-hook comint-mode-hook
        shell-mode-hook lisp-mode-hook erc-mode-hook ruby-mode-hook))

(setq-default fill-column 72)
(setq emacs-lisp-docstring-fill-column 72)

;; trailing whitespace
(setq-default show-trailing-whitespace t)
(mapc #'(lambda (mode-hook)
          (add-hook mode-hook
                    #'(lambda ()
                        (setq show-trailing-whitespace nil))))
      '(Buffer-menu-mode-hook custom-mode-hook text-mode-hook
                              help-mode-hook
                              term-mode-hook Info-mode-hook comint-mode-hook
                              buffer-menu-mode-hook apropos-mode-hook
                              tooltip-show-hook gnus-article-mode-hook mail-mode-hook
                              gnus-summary-mode-hook message-mode-hook
                              gnus-group-mode-hook eshell-mode-hook w3-mode-hook
                              initial-calendar-window-hook))

(setq message-log-max most-positive-fixnum)

(add-hook 'write-file-functions 'time-stamp)

;; url and mail address recognition
(when (fboundp 'goto-address)
  (setq goto-address-fontify-maximum-size most-positive-fixnum)
  (add-hook 'find-file-hook 'goto-address))

(setq-default indicate-empty-lines       t
              indicate-buffer-boundaries t)

(let ((hook #'(lambda ()
                (setq indicate-empty-lines       nil
                      indicate-buffer-boundaries nil))))
  (mapc #'(lambda (mode-hook)
            (add-hook mode-hook hook))
        '(shell-mode-hook term-mode-hook gnus-article-mode-hook
                          gnus-summary-mode-hook gnus-group-mode-hook
                          eshell-mode-hook)))

;; linum from g.e.s -- much better than setnu
;; (when (and (locate-library "linum")
;;            (facep 'fringe))
;;   (setq linum-format (propertize "%5d " 'face 'fringe)))

;; (setq woman-use-own-frame nil)
;; (setq mouse-buffer-menu-mode-mult 1)

(setq tramp-default-method "ssh")

(when (tty-type)
  (defun ted-delete-frame-or-kill-emacs ()
    (interactive)
    (if (cdr (frame-list)) ; (> (length (frame-list)) 1)
        (delete-frame)
      (save-buffers-kill-emacs)))
  (global-set-key (kbd "C-x C-c") 'ted-delete-frame-or-kill-emacs))

(when (fboundp 'find-function-setup-keys)
  (find-function-setup-keys))

;; forward-sexp, etc., should treat comments as whitespace
(setq parse-sexp-ignore-comments t)

;; follow symbolic link.
(setq vc-follow-symlinks t)

(setq skeleton-pair t)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

(when (require 'generic-x nil t)
  (setq default-major-mode 'default-generic-mode))

;; completion in M-:
(when (keymapp read-expression-map)
  (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol))

;;Echo documents about function or variable at the point
(when (locate-library "eldoc")
  (mapc (lambda (mode-hook)
          (add-hook mode-hook 'turn-on-eldoc-mode))
        '(emacs-lisp-mode-hook lisp-interaction-mode-hook
                               ielm-mode-hook))

  (defun help-default-arg-highlight (arg)
    "Upcase and fontify ARG for use with `eldoc-mode' and help."
    (propertize (upcase arg)
                'face 'font-lock-variable-name-face))

  (setq eldoc-argument-case 'help-default-arg-highlight))

(when (locate-library "table")
  (autoload 'table-insert "table" nil t)
  (define-key pt-custom-global-map "t" 'table-insert))

(define-key pt-custom-global-map "d" 'diff-buffer-with-file)
(setq diff-switches "-u")

(when (eq window-system 'ns)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser))

(defun pt-find-font ()
  "Return the first available font listed in `pt-fonts'."
  (let ((family-list (font-family-list)))
    (find-if (lambda (font) (member font family-list))
             pt-fonts)))

;; (defun pt-set-font (&optional font size)
;;   "Figure out and install which font and size I use on this system.
;; If called interactively, prompts the user for the font and size to use."
;;   (interactive
;;    (list (progn (setq font (or font (pt-find-font)))
;;                 (setq size2 (or size pt-font-size))
;;                 (completing-read (format "Font (default %s): " font)
;;                                  (font-family-list) nil t nil nil font))
;;          (read-number "Size: " pt-font-size)))
;;   (let* ((font (or font pt-font))
;;          (size2 (or size pt-font-size))
;;          (font-spec (format "%s-%d" font size2)))
;;     (when (or font size)
;;       (add-to-list 'default-frame-alist (cons 'font font-spec))
;;       (set-frame-font font-spec))))

;; if it's not tty
(unless (tty-type)
  ;; (add-hook 'emacs-startup-hook 'pt-set-font)
  (when (fboundp 'text-scale-increase)
    (global-set-key (kbd "M-=") 'text-scale-increase)
    (global-set-key (kbd "M--") 'text-scale-decrease)))

(add-to-list 'default-frame-alist
             '(wait-for-wm . nil))

(setq initial-frame-alist default-frame-alist)

(when (display-graphic-p)
  (setq frame-title-format
        ;; "file/buffer name" ["size"]
        '((buffer-file-name
           "%f [%I]"
           "%b [%I]")))

  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1)
    (add-to-list 'default-frame-alist '(tool-bar-lines . 0)))

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

(when (display-color-p)
  (when (locate-library "htmlize")
    (autoload 'htmlize-buffer "htmlize" nil t)))

(defvar pt-color-theme-list nil
  "Color theme list.")

(defvar pt-choose-color-theme-function
  'pt-choose-color-theme-by-second
  "Function that returns a color theme.")

(defun pt-choose-color-theme-by-second ()
  "Choose a valid color theme in `pt-color-theme-list' according to
current second."
  (let ((theme-list pt-color-theme-list)
        theme)
    (while theme-list
      (setq theme (nth
                   (mod (car (decode-time (current-time)))
                        (length theme-list))
                   theme-list))
      (if (and (require theme nil t)
               (fboundp theme))
          (setq theme-list nil)
        (remove theme theme-list)))
    theme))

(when (require 'color-theme nil t)
  (add-hook 'emacs-startup-hook
            #'(lambda ()
                (funcall
                 (or (funcall (or pt-choose-color-theme-function 'ignore))
                     'ignore)))))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq custom-unlispify-tag-names       nil
      custom-unlispify-menu-entries    nil
      custom-unlispify-remove-prefixes nil)

(setq custom-file
      (expand-file-name (concat pt-emacs-name-and-version "-custom.el")
                        user-emacs-directory))

(when (eq window-system 'ns)
  (progn
    (if (require 'mac-key-mode nil t)
        (progn
          (mac-key-mode 1)
          (define-key mac-key-mode-map [?\A-m] 'toggle-maxframe)
          (define-key mac-key-mode-map [?\A-w] 'delete-frame)
          (define-key mac-key-mode-map [?\A-o] 'find-file)
          (define-key mac-key-mode-map [?\A-/] 'comment-or-uncomment-region)))
    ;; been set at emacs-startup-hook
    (setq ns-alternate-modifier 'meta)
    (setq ns-command-modifier 'alt)))

;; maximize frame
(when (and window-system
           (require 'maxframe nil t))
  (defun toggle-maxframe ()
    "Toggle maxframe."
    (interactive)
    (if (or mf-restore-width mf-restore-height mf-restore-top mf-restore-left)
        (restore-frame)
      (maximize-frame)))
  ;; (add-hook 'window-setup-hook 'maximize-frame t)
  (global-set-key [?\A-m] 'toggle-maxframe))

;; set distributed elisp files read-only
(when (eq window-system 'ns)
  (add-hook 'find-file-hook
            '(lambda ()
               (let ((case-fold-search t))
                 (if (string-match
                      "/.+\\.app/.*\\.el"
                      (buffer-file-name))
                     (toggle-read-only t))))))

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

;;automatically revert buffer while file on disk changes
(global-auto-revert-mode 1)

;; this can fix next-line vertical move problem
(setq line-move-visual nil)

(defadvice keyboard-quit (before delete-windows activate)
  "Delete all matched window before really `keyboard-quit'"
  (mapc #'(lambda (name)
            (dolist (window (cdr (window-list nil nil (selected-window))))
              (if (string-equal name (buffer-name (window-buffer window)))
                  (delete-window window))))
        pt-ignore-buffer-list))

(setq scroll-preserve-screen-position t)

;; act on lines if mark is not active
(defmacro defadvice-current-line-as-region (orig-function)
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
          (defadvice-current-line-as-region command))
      '(kill-ring-save
        kill-region
        comment-region
        comment-or-uncomment-region
        upcase-region
        downcase-region
        clipboard-kill-region
        clipboard-kill-ring-save))

(when (require 'ido nil t)
  (ido-mode 1)
  (mapc #'(lambda (buffer)
            (add-to-list 'ido-ignore-buffers buffer))
        pt-ignore-buffer-list)
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
  (setq ido-use-filename-at-point 'guess)
  (setq ido-enable-last-directory-history nil)
  (setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
  (setq ido-show-dot-for-dired t) ;; put . as the first item
  ;; (setq ido-use-filename-at-point t) ;; prefer file names near point)
  (add-hook 'ido-setup-hook
            #'(lambda ()
                (define-key ido-common-completion-map (kbd "TAB") 'ido-next-match)
                (define-key ido-common-completion-map (kbd "M-TAB") 'ido-prev-match))))

(setq-default tab-width 4)

(setq cua-enable-cua-keys nil)
(cua-mode 1)
;; (define-key cua--region-keymap [?\C-w] 'cua-cut-region)
(define-key cua-global-keymap [(control return)] nil)

(add-hook 'pt-after-init-functions
          #'(lambda ()
              (when pt-emacs-tmp-directory
                (setq bookmark-default-file
                      (pt-get-directory-create
                       (expand-file-name
                        "bookmarks"
                        pt-emacs-tmp-directory))))))

(setq default-input-method nil)

(when (require 'recentf nil t)
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)

  (when (require 'ido nil t)
    (defvar ido-recentf-list nil)
    (defun recentf-ido-find-file ()
      "Find recently open file using ido-mode."
      (interactive)
      ;; remove symlinks
      (setq recentf-list
            (remove-duplicates
             recentf-list
             :test
             '(lambda (x y)
                (if (or (file-remote-p y) (file-remote-p x)) ;;fix tramp problem
                    (string= x y)
                  (string= (file-truename x)
                           (file-truename y))))))
      (setq ido-recentf-list
            (let (records result)
              (dolist (file recentf-list result)
                (let* ((f (file-name-nondirectory file))
                       (match (assoc f records)))
                  (progn
                    (if match
                        (setcdr match (1+ (cdr match)))
                      (setq records
                            (append records
                                    (list (cons f 1)))))
                    (setq result
                      (append result
                         (list (cons
                                 (concat f
                                    (if (cdr match)
                                      (concat "<"
                                        (int-to-string
                                         (cdr match)) ">")))
                                     file)))))
                  result))))
      (let ((filename
             (ido-completing-read "Choose recent file: "
                (mapcar 'car ido-recentf-list)
                nil
                t)))
        (when filename
          (find-file
           (cdr (assoc filename
                       ido-recentf-list))))))

    (defun ido-kill-recentf-at-head ()
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

    (add-hook 'ido-setup-hook
              #'(lambda ()
                  (define-key
                    ido-common-completion-map [?\C-k]
                    'ido-kill-recentf-at-head)))

    (global-set-key "\C-x\ \C-r" 'recentf-ido-find-file)))

;; highlight the current line
;; (when (locate-library "hl-line")
;;   (autoload 'global-hl-line-mode "hl-line"
;;     "Highlight current line." t)
;;   (autoload 'hl-line-mode "hl-line"
;;     "Global highlight current line." t)
;;   (when window-system
;;     (global-hl-line-mode 1)))

(add-hook 'pt-after-init-functions
          #'(lambda ()
              (when pt-emacs-tmp-directory
                (setq auto-save-list-file-prefix
                      (concat (pt-get-directory-create
                               (expand-file-name
                                (file-name-as-directory "auto-save-list")
                                pt-emacs-tmp-directory))
                              "save-list-")))))

;; save auto-save files under `pt-emacs-tmp-directory'/auto-saves/
(add-hook 'pt-after-init-functions
          #'(lambda ()
              (when pt-emacs-tmp-directory
                (add-to-list 'auto-save-file-name-transforms
                             (list "\\`\\(/[^/]+\\)*/\\([^/]+\\)\\'"
                                   (concat (pt-get-directory-create
                                            (expand-file-name
                                             (file-name-as-directory "auto-saves")
                                             pt-emacs-tmp-directory))
                                           "\\2")
                                   t)))))

;; save backups under `pt-emacs-tmp-directory'/backups/
(add-hook 'pt-after-init-functions
          #'(lambda ()
              (when pt-emacs-tmp-directory
                (setq backup-directory-alist
                      `((".*" . ,(pt-get-directory-create
                                  (expand-file-name
                                   (file-name-as-directory "backups")
                                   pt-emacs-tmp-directory))))))))

;; run hooks after init
(add-hook 'after-init-hook
          #'(lambda ()
              (run-hooks 'pt-after-init-functions)))

;; remember cursor position
(when (require 'saveplace nil t)
  (setq-default save-place t)
  (add-hook 'after-init-hook
            #'(lambda ()
                (setq save-place-loaded nil)
                (setq save-place-alist nil)
                (setq save-place-file
                      (pt-get-directory-create
                       (expand-file-name
                        "emacs-places"
                        pt-emacs-tmp-directory))))))

;; create file when the file doesn't exist
(add-hook 'write-file-functions
          '(lambda ()
             (let ((directory (file-name-directory (buffer-file-name))))
               (unless (file-directory-p directory)
                 (make-directory directory t)))))

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

(defvar pt-new-buffer-query nil)
(make-variable-buffer-local 'pt-new-buffer-query)

(defun pt-new-buffer (&optional arg)
  "Create a new buffer."
  (interactive "P")
  (let ((mode (if arg major-mode default-major-mode)))
    (switch-to-buffer (generate-new-buffer "untitled"))
    (funcall mode)
    ;; (pt-yas-template)
    (set-buffer-modified-p nil)
    (setq pt-new-buffer-query t)))

(defun pt-new-buffer-query-funtion ()
  (if (and pt-new-buffer-query
           (not (buffer-file-name))
           (buffer-modified-p)
           (> (buffer-size) 0))
      (y-or-n-p (format "Buffer %s modified; kill anyway? " (buffer-name)))
    t))

(add-hook 'kill-buffer-query-functions
          'pt-new-buffer-query-funtion)

(defun pt-yas-template (&optional mode-symbol)
  "Insert template by expanding a yas key named #template-yas# for MODE-SYMBOL mode at
beginning of current buffer."
  (interactive)
  (unless (and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (let* ((tables (yas/get-snippet-tables mode-symbol)))
      (dolist (table tables)
        (let ((template (yas/fetch table "yas-template$")))
          (when template
            (goto-char (point-min))
            (yas/expand-snippet (yas/template-content (cdar template)))
            (setq tables nil)))))))

(add-hook 'find-file-hook 'pt-yas-template)

(defmacro pt-xor (a b)
  `(and (or ,a ,b) (not (and ,a ,b))))

(defun pt-next-buffer (&optional arg)
  "Switch to next buffer which is not in `pt-ignore-buffer-list'.
If ARG is non-nil, then switch between file-visted-buffer and
non-file-visted-buffer."
  (interactive "P")
  (let ((buffer (current-buffer)))
    (next-buffer)
    (setq arg (pt-xor (buffer-file-name buffer) arg))
    (while (and (not (eq buffer (current-buffer)))
                (or (member (buffer-name (current-buffer))
                            pt-ignore-buffer-list)
                    (if arg (not (buffer-file-name))
                      (buffer-file-name))))
      (next-buffer))))

(defun pt-previous-buffer (&optional arg)
  "Switch to previous buffer which is not in `pt-ignore-buffer-list'.
If ARG is non-nil, then switch between file-visted-buffer and
non-file-visted-buffer."
  (interactive "P")
  (let ((buffer (current-buffer)))
    (previous-buffer)
    (setq arg (pt-xor (buffer-file-name buffer) arg))
    (while (and (not (eq buffer (current-buffer)))
                (or (member (buffer-name (current-buffer))
                            pt-ignore-buffer-list)
                    (if arg (not (buffer-file-name))
                      (buffer-file-name))))
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

(add-hook 'kill-buffer-hook
          #'(lambda ()
              (when (interactive-p)
                (pt-next-buffer))))


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

(define-key ctl-x-map "2" 'pt-split-window)
(define-key ctl-x-map "3" 'pt-split-window-horizontally)
(winner-mode 1)
(define-key ctl-x-map "7" 'pt-swap-windows)
(define-key ctl-x-map "9" 'winner-undo)


;; minibuffer settings
(setq enable-recursive-minibuffers t)

;; (require 'mb-depth nil t)

;; default value at point
(when (fboundp 'minibuffer-depth-indicate-mode)
  (minibuffer-depth-indicate-mode 1))

(when (fboundp 'minibuffer-electric-default-mode)
  (minibuffer-electric-default-mode 1))

(define-key minibuffer-local-map [escape] 'abort-recursive-edit)

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
(define-key minibuffer-local-map [backspace]
  '(lambda (n)
     (interactive "P")
     (unless n (setq n 1))
     (unless (get-text-property (- (point) 1) 'read-only)
       (delete-backward-char n))))


;;; global key bindings
(global-set-key [f2] ctl-x-map)
(when (eq 'ns window-system)
  (global-set-key [?\A-n] 'pt-new-buffer)
  (global-set-key [?\A-\M-,]
                  #'(lambda ()
                      (interactive)
                      (find-file user-init-file))))

(global-set-key [f1 ?j] 'elisp-index-search)
(global-set-key (kbd "TAB") 'pt-tab-command)
(global-set-key [?\M-m] 'set-mark-command)
;; (when (fboundp 'cua-set-rectangle-mark)
;;   (global-set-key (kbd "M-S-SPC") 'cua-set-rectangle-mark))
(when (fboundp 'ido-switch-buffer-other-window)
  (global-set-key [?\C-x ?\C-b] 'ido-switch-buffer-other-window))
;; (global-set-key [f1 ?a] 'apropos)
(global-set-key [?\C-a] 'pt-beginning-of-line-or-text)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [?\M-n] 'forward-paragraph)
(global-set-key [?\M-p] 'backward-paragraph)
(global-set-key [escape] 'keyboard-quit)
(global-set-key [?\C-x ?p] 'pop-to-mark-command)

(global-set-key [?\C-x ?\C-n] 'pt-new-buffer)

(autoload 'dired-jump "dired-x" "Jump to dired buffer")
(global-set-key [?\C-x ?\C-d] 'dired-jump)


(global-set-key (kbd "C-j")
                #'(lambda ()
                    (interactive)
                    (end-of-line)
                    (comment-indent-new-line)))

(global-set-key (kbd "M-j")
                #'(lambda ()
                    (interactive)
                    (beginning-of-line)
                    (split-line)))

(mapc #'(lambda (key-command)
          (define-key ctl-x-map
            (car key-command)
            (cdr key-command)))
      '(([?\C-g] . keyboard-quit)
        ("k" . kill-this-buffer)
        ("f" . find-file)))

(when (eq 'ns window-system)
  (global-set-key [ns-drag-file] 'ns-find-file))

(global-set-key [?\C-w] 'pt-kill-region)

;; smart comment or uncomment line/region
(define-key ctl-x-map (kbd "/") 'comment-or-uncomment-region)

(define-key ctl-x-map "l" 'find-library)

(defun pt-switch-buffer (&optional arg)
  "C-u M-x `pt-switch-buffer' switchs between file-visited-buffer and
non-file-visited-buffer;
C-u C-u M-x `pt-switch-buffer' switchs previous
buffer."
  (interactive "P")
  (if (and (consp arg)
           (= (car arg) 16))
      (pt-previous-buffer arg)
    (pt-next-buffer arg)))

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

(define-key ctl-x-map (kbd "M-m") 'pop-global-mark)
(define-key ctl-x-map [f2] 'other-window)

(provide 'pt-simple)
;; pt-simple ends here
