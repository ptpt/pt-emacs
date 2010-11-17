(require 'pt-simple)

(add-hook 'org-mode-hook
          #'(lambda ()
              (define-key org-mode-map (kbd "RET") 'org-return-indent)
              (define-key org-mode-map (kbd "M-]") 'outline-next-visible-heading)
              (define-key org-mode-map (kbd "M-[") 'outline-previous-visible-heading)))

(when (fboundp 'org-mode)
  (define-key pt-custom-global-map (kbd "o l") 'org-store-link)
  (define-key pt-custom-global-map (kbd "o a") 'org-agenda))

(when (fboundp 'remember)
  (define-key pt-custom-global-map (kbd "r") 'remember))

(provide 'pt-org)