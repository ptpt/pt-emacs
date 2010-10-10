(provide 'pt-cc)

(mapc #'(lambda (hook)
          (add-hook hook
                    #'(lambda ()
                        (c-toggle-auto-newline t))))
      '(c-mode-hook c++-mode-hook))

;; autoloaded
(c-set-offset 'inline-open 0)
(c-set-offset 'substatement-open 0)
(c-set-offset 'block-open '+)
