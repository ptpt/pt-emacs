;;; pt-template.el

;; Copyright (C) 2010 Tao Peng <ptpttt@gmail.com>

;; Author:   Tao Peng <ptpttt@gmail.com>
;; Keywords: convenience, extensions

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

(defvar pt-template-alist nil)

(defun pt-template-edit ()
  ""
  (interactive)
  (let ((tpl (pt-template-find)))
    (if tpl
        (progn
          (find-file (cadr tpl))
          (if (symbolp (car tpl))
              (funcall (car tpl)))))))

(defun pt-template-insert (&optional buffer)
  ""
  (interactive)
  (let ((template (pt-template-find major-mode
                                    (buffer-file-name)))
        count)
    (if template
        (progn
          (setq count (cadr (insert-file-contents (cadr template))))
          (cond ((consp (cddr template))
                 (pt-substitution (caddr template)
                                  (cdddr tempate)
                                  (point)
                                  (+ (point) count)))
                ((eq t (cddr template))
                 (pt-substitution nil nil
                                  (point) (+ (point) count)))))
      (if (called-interactively-p)
          (message "Template not found.")))))

(defun pt-template-find (&optional mode filename)
  ""
  (setq mode (or mode major-mode)
        filename (or (and filename (expand-file-name filename))
                     (buffer-file-name)))
  (assoc-if #'(lambda (condition)
                (or (and (stringp condition)
                         (stringp filename)
                         (string-match condition filename))
                    (and (symbolp condition)
                         (eq mode condition))
                    ))
            pt-template-alist))

(defun pt-template-add-file (filename)
  ""
  (let* ((filename (expand-file-name filename))
         (template (cond ((string-match "/\\([^/]+\\)\\.template\\'" filename)
                          (cons (intern (match-string 1 filename))
                                (cons filename t)))
                         ((string-match "/template\\(\\.[^/]+\\)\\'" filename)
                          (cons (concat (regexp-quote (match-string 1 filename)) "\\'")
                                (cons filename t)))
                         ((string-match "/non-template\\(\\.[^/]+\\)\\'" filename)
                          (cons (concat (regexp-quote (match-string 1 filename)) "\\'")
                                (cons filename nil)))
                         (t
                          (cons (intern (file-name-nondirectory filename))
                                (cons filename nil)))))
         (match (assoc (car template) pt-template-alist)))
    (if match
        (unless (equal match template)
          (setcdr match (cdr template)))
      (add-to-list 'pt-template-alist
                   template))))

(defun pt-template-add-directory (dir)
  ""
  (mapc #'(lambda (filename)
              (pt-template-add-file filename))
          (directory-files dir t "\\`[^.]")))

(provide 'pt-template)