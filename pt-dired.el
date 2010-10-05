(provide 'pt-dired)

(defun dired-find-file-one-buffer ()
  "In Dired, visit the file or directory named on this line in one
buffer"
  (interactive)
  (let ((oldbuf (current-buffer)))
    (when (dired-find-file)
      (kill-buffer oldbuf))))

(setq dired-recursive-deletes 'top
      dired-recursive-copies  'top)

(when (locate-library "wdired")
  (autoload 'wdired-change-to-wdired-mode "wdired" nil t)
  (add-hook 'dired-load-hook
            (lambda ()
              (define-key dired-mode-map (kbd "r")
                'wdired-change-to-wdired-mode))))
;; Dired, the Emacs directory editor.
(setq dired-dwim-target t)
