
(eval-after-load "latex"
  '(progn (setq font-latex-script-display nil)
          (setq TeX-save-query nil)
          (setq LaTeX-command "pdflatex --shell-escape --synctex=1")
          (add-hook 'LaTeX-mode-hook
                    '(lambda ()
                       (defkey LaTeX-mode-map [?\C-c ?\C-c]
                         (TeX-save-document (TeX-master-file))
                         (TeX-command "LaTeX" 'TeX-master-file))

                       (add-to-list 'TeX-view-program-list '("Open" ("open %o")))
                       (add-to-list 'TeX-view-program-selection '(output-pdf "Open"))))))

(when (require 'tex-site nil t)
  (setq-default TeX-auto-untabify nil)
  (setq TeX-auto-untabify nil))
