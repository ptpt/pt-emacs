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

(defun pt-cc-output-file-name (&optional file-name)
  "Generate output file-name."
  (setq file-name (or file-name (buffer-file-name)))
  (when file-name
    (let ((output (file-name-sans-extension file-name)))
      (if (string= output (buffer-file-name))
          (concat output ".out")
        (if (memq system-type '(windows-nt ms-dos))
            (concat output ".exe")
          output)))))

(add-to-list 'pt-compile-run-alist
             `(c-mode . ("\\cgcc -Wall \"$(buffer-file-name)$\" -o \"$(pt-cc-output-file-name)$\""
                         . ,(if (memq system-type '(ms-dos windows-nt))
                                "START \"$(pt-cc-output-file-name)$\" CMD /K \"\"$(pt-cc-output-file-name)$\" & PAUSE & EXIT\""
                              "\\n\\c$(pt-cc-output-file-name)$"))))
;; (setq pt-compile-run-alist nil)
(provide 'pt-cc)