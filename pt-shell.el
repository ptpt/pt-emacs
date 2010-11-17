;; shell settings
(add-hook 'term-mode-hook
          #'(lambda ()
              (define-key term-raw-map [?\C-w] nil)
              (define-key term-raw-map [?\C-d]
                #'(lambda ()
                    (interactive)
                    (term-send-raw)
                    (sleep-for 2)
                    (kill-this-buffer)))
              (define-key term-raw-map [?\M-x] nil)))

(define-key pt-custom-global-map "s"
  #'(lambda ()
      (interactive)
      (ansi-term
       (or explicit-shell-file-name
           (getenv "ESHELL")
           (getenv "SHELL")
           "/bin/sh"))))

(add-hook 'sh-mode-hook
          #'(lambda ()
              (defun pt-sh-newline ()
                (interactive)
                (when (looking-back "\\(done\\|fi\\|else\\)[ \t\n]*")
                  (indent-according-to-mode))
                (newline-and-indent))
              (define-key sh-mode-map (kbd "RET") 'pt-sh-newline)))

(provide 'pt-shell)