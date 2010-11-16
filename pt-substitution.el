(defun pt-substitution
  (start-sign &optional end-sign start end execute-buffer noerror)
  "Replace signed form in the current buffer with its corresponding
evaluated value. the signed forms must start with START-SIGN, and end
with END-SIGN if END-SIGN is non-nil."
  (let ((len-of-start-sign (length start-sign))
        (old-buffer-string (buffer-string))
        (regexp (format "%s\\|%s"
                        (regexp-quote start-sign)
                        (regexp-quote "\\")))
        exp-start exp val result)
    (goto-char (or start (point-min)))
    (while (and (null result)
                (re-search-forward regexp end t))
      (cond ((string= (match-string-no-properties 0) "\\")
             (cond ((looking-at (regexp-quote "\\"))
                    (delete-char -1)
                    (forward-char))
                   ((looking-at (regexp-quote start-sign))
                    (delete-char -1)
                    (forward-char len-of-start-sign))
                   (t (forward-char))))
            ((string= (match-string-no-properties 0) start-sign)
             (setq exp-start (point))
             (setq exp (read (current-buffer)))
             (when (or (null end-sign)
                       (search-forward end-sign nil t)
                       (and (goto-char (or end (point-max)))
                            (if (not noerror)
                                (error "End-sign `%s' not found." end-sign)
                              (setq result (cons exp-start (point))))
                            nil))
               (cond ((or (symbolp exp) (consp exp))
                      (delete-region (- exp-start len-of-start-sign) (point))
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
                                   (error "%s must return string or integer or nil."
                                          (prin1-to-string exp))
                                 (setq result (cons exp-start (point)))))))
                     (t (if (not noerror)
                            (error "Reading expression error.")
                          (setq result (cons exp-start (point))))))))))
    result))

(defun pt-substitution-string (string start-sign &optional end-sign noerror)
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