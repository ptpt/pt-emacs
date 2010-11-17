;; lisp settings
(when (eq 'ns window-system)
  (define-key lisp-mode-shared-map [?\A-r] 'eval-defun))

(define-key lisp-mode-shared-map [?\M-a] nil)
(define-key lisp-mode-shared-map [?\M-e] nil)
(setq ielm-prompt "* ")
(setq inferior-lisp-program
      (or (executable-find "sbcl")
          (executable-find "lisp")
          (executable-find "openmcl")
          (executable-find "clisp")))

(mapc (lambda (hook)
        (add-hook hook
                  (lambda ()
                    (set (make-local-variable 'lisp-indent-function)
                         'common-lisp-indent-function))))
      '(lisp-mode-hook inferior-lisp-mode-hook))

(defun ted-install-lispy-bindings (map bind-ret)
  "FIXME"
  ;; (define-key map (kbd "M-k") 'kill-sexp)
  (define-key map (kbd "\"")
    (find-if 'commandp '(paredit-doublequote skeleton-pair-insert-maybe)))
  (when bind-ret
    (define-key map (kbd "RET")
      (find-if 'commandp '(paredit-newline newline-and-indent))))
  (define-key map (kbd "(")
    (find-if 'commandp '(paredit-open-parenthesis
                         paredit-open-list
                         insert-parentheses)))
  (define-key map (kbd ")")
    (find-if 'commandp '(paredit-close-parenthesis-and-newline
                         ;; paredit-close-list-and-newline
                         move-past-close-and-reindent))))

(ted-install-lispy-bindings
 (cond ((boundp 'lisp-mode-shared-map) lisp-mode-shared-map)
       ((boundp 'shared-lisp-mode-map) shared-lisp-mode-map)
       (t emacs-lisp-mode-map))
 t)

(eval-after-load "ielm"
  '(ted-install-lispy-bindings ielm-map nil))

(add-to-list 'auto-mode-alist '("\\.elc\\'" . emacs-lisp-mode))
(provide 'pt-lisp)