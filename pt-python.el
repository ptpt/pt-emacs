(provide 'pt-python)

(setq gud-pdb-command-name "python -m pdb")
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map "'" 'skeleton-pair-insert-maybe)
             (define-key python-mode-map
               [?\M-\[] #'(lambda ()
                           (interactive)
                           (python-beginning-of-defun)))
             (when (eq 'ns window-system)
               (define-key python-mode-map [?\A-r] 'pt-run-python))

             (define-key python-mode-map
               [?\M-\]] #'(lambda ()
                          (interactive)
                          (python-end-of-defun)))))

(add-to-list 'pt-compile-run-alist
             '(python-mode nil . "$(progn python-command)$ $(buffer-file-name)$"))

(defun pt-run-python ()
  "Save and run the script in a new frame."
  (interactive)
  (let ((pop-up-frames t))
    (when (buffer-modified-p)
      (save-buffer))
    (shell-command
     (concat python-command
             " "
             (buffer-file-name))
     "*Python Output*")))

(when (boundp 'pt-ignore-buffer-list)
  (add-to-list 'pt-ignore-buffer-list "*Python Output*"))