(require 'pt-compile-run)

(mapc #'(lambda (hook)
          (add-hook hook
                    #'(lambda ()
                        (when window-system
                          (define-key c-mode-map
                            [?\A-r] 'pt-compile-run))
                        (c-toggle-auto-newline t))))
      '(c-mode-hook c++-mode-hook))

;; autoloaded
(c-set-offset 'inline-open 0)
(c-set-offset 'substatement-open 0)
(c-set-offset 'block-open '+)
