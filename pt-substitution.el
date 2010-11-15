(defun pt-substitution (start-sign &optional end-sign execute-buffer noerror)
  "Replace signed form in the current buffer with its corresponding
evaluated value. the signed forms must start with START-SIGN, and end
with END-SIGN if END-SIGN is non-nil."
  (let ((len-of-start-sign (length start-sign))
        (old-buffer-string (buffer-string))
        (sub t)
        (start nil)
        (regexp (format "%s\\|%s"
                        (regexp-quote start-sign)
                        (regexp-quote "\\"))))
    (goto-char (point-min))
    (while (and sub (re-search-forward regexp nil t))
      (cond ((string= (match-string-no-properties 0) "\\")
             (cond ((looking-at (regexp-quote "\\"))
                    (delete-char -1)
                    (forward-char))
                   ((looking-at (regexp-quote start-sign))
                    (delete-char -1)
                    (forward-char len-of-start-sign))
                   (t (forward-char))))
            ((string= (match-string-no-properties 0) start-sign)
             (setq start (point))
             (setq sub (if execute-buffer
                           (let ((read-buffer (current-buffer)))
                             (with-current-buffer execute-buffer
                               (ignore-errors (eval (read read-buffer)))))
                         (ignore-errors (eval (read (current-buffer))))))
             (when (or (null end-sign)
                       (search-forward end-sign nil t)
                       (and (goto-char (point-max))
                            (setq sub nil) nil))
               (cond ((or (stringp sub)
                          (integerp sub))
                      (delete-region (- start len-of-start-sign) (point))
                      (insert sub))
                     (t (setq sub nil)))))))
    (if (and (not noerror)
             (null sub))
        (error "%s must return a string or integer."
               (buffer-substring
                (if start (- start len-of-start-sign)
                  (point-min))
                (point))))
    (if (null sub)
        (cons (if start (- start len-of-start-sign) (point-min)) (point))
      nil)))

(defun pt-substitution-string (string start-sign &optional end-sign noerror)
  "Replace signed form in the string STRING with its corresponding
evaluated value. the signed forms must start with START-SIGN, and end
with END-SIGN if END-SIGN is non-nil."
  (let ((cbuf (current-buffer)))
    (with-temp-buffer
      (insert string)
      (if (null (pt-substitution start-sign end-sign cbuf noerror))
          (buffer-string)
        nil))))

(provide 'pt-substitution)