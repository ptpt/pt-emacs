;;; pt-substitution.el

;; Copyright (C) 2010 Tao Peng <ptpttt@gmail.com>

;; Author:   Tao Peng <ptpttt@gmail.com>
;; Keywords: convenience, internal

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

(defun pt-substitution
  (&optional start-sign end-sign start end execute-buffer noerror)
  "Replace signed form in the current buffer with its corresponding
evaluated value. the signed forms must start with START-SIGN, and end
with END-SIGN if END-SIGN is non-nil."
  (let ((len-of-start-sign (length (or start-sign "(")))
        (old-buffer-string (buffer-string))
        (regexp (format "%s\\|%s"
                        (regexp-quote (or start-sign "("))
                        (regexp-quote "\\")))
        (end-marker (make-marker))
        exp-start exp val result)
    (set-marker end-marker (or end (point-max)))
    (set-marker-insertion-type end-marker t)
    (goto-char (or start (point-min)))
    (while (and (null result)
                (re-search-forward regexp (marker-position end-marker) t))
      (cond ((string= (match-string-no-properties 0) "\\")
             (cond ((looking-at (regexp-quote "\\"))
                    (delete-char -1)
                    (forward-char))
                   ((looking-at (regexp-quote (or start-sign "(")))
                    (delete-char -1)
                    (forward-char len-of-start-sign))
                   (t (forward-char))))
            ((string= (match-string-no-properties 0) (or start-sign "("))
             (unless start-sign
               (backward-char len-of-start-sign))
             (setq exp-start (point))
             (setq exp (read (current-buffer)))
             (when (or (null end-sign)
                       (search-forward end-sign nil t)
                       (and (goto-char (or end (point-max)))
                            (if (not noerror)
                                (error "End-sign `%s' not found" end-sign)
                              (setq result (cons exp-start (point))))
                            nil))
               (cond ((or (symbolp exp) (consp exp))
                      (delete-region
                       (if start-sign (- exp-start len-of-start-sign) exp-start)
                       (point))
                      (setq val (if execute-buffer
                                    (let ((read-buffer (current-buffer)))
                                      (with-current-buffer execute-buffer
                                        (ignore-errors (eval exp))))
                                  (ignore-errors (eval exp))))
                      (cond ((null val) t)
                            ((or (stringp val)
                                 (integerp val))
                             (insert val))
                            (t (if (not noerror)
                                   (error "%s must return string or integer or nil"
                                          (prin1-to-string exp))
                                 (setq result (cons exp-start (point)))))))
                     (t (if (not noerror)
                            (error "Reading expression error")
                          (setq result (cons exp-start (point))))))))))
    (goto-char (marker-position end-marker))
    result))

(defun pt-substitution-string (string &optional start-sign end-sign noerror)
  "Replace signed form in the string STRING with its corresponding
evaluated value. the signed forms must start with START-SIGN, and end
with END-SIGN if END-SIGN is non-nil."
  (let ((cbuf (current-buffer)))
    (with-temp-buffer
      (insert string)
      (if (null (pt-substitution start-sign end-sign nil nil cbuf noerror))
          (buffer-string)
        nil))))

(provide 'pt-substitution)