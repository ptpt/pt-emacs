
(defun ted-linkify-region (start end)
  (interactive "r")
  (let ((str (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert "<a href=\"\">" str "</a>")))

(add-to-list 'auto-mode-alist
             (cons "\\.\\(rdf\\|rss\\|atom\\)\\'" 'nxml-mode))

(defun ted-insert-char-entity-maybe (char entity)
  (if (equal (preceding-char) char)
      (progn
        (backward-delete-char 1)
        (insert entity))
    (insert char)))

(eval-after-load "sgml-mode"
  '(mapc (lambda (char-entity-alist)
           (let ((cmd `(lambda ()
                         (interactive)
                         (ted-insert-char-entity-maybe
                          ,(car char-entity-alist)
                          ,(cdr char-entity-alist)))))
             (define-key html-mode-map (string (car char-entity-alist))
               cmd)))
         '((?< . "&lt;")
           (?> . "&gt;")
           (?& . "&amp;")
           (?\" . "&quot;")
           (?\' . "&apos;"))))


(setq html-tag-face-alist
      '(("b" . bold)
        ("big" . bold)
        ("blink" . highlight)
        ("h1" bold underline)
        ("h4" . underline)
        ("h5" . underline)
        ("h6" . underline)
        ("rev"  . modeline)
        ("s" . underline)
        ("small" . default)
        ("strong" . bold)
        ("title" bold underline)
        ("tt" . default)
        ("u" . underline)
        ;; Were italic
        ("cite" . default)
        ("em" . bold)
        ("h2" bold underline)
        ("h3" underline)
        ("i" . italic)
        ("var" . default)))

(provide 'pt-html-mode)(when (and (featurep 'emacs)
           (or (load "rng-auto" t)
               (locate-library "nxml-mode")))

  (unless (fboundp 'nxml-mode)
    (autoload 'nxml-mode "nxml-mode" nil t))

  (unless (fboundp 'rng-validate-mode)
    (autoload 'rng-validate-mode "rng-valid" nil t))

  (setq nxml-sexp-element-flag t
        nxml-slash-auto-complete-flag t)

  ;; Hack `;' in nxml mode to automatically fix named character entity
  ;; references.
  (when (boundp 'nxml-mode-abbrev-table)
    (add-hook 'nxml-mode-hook
              (lambda ()
                (setq local-abbrev-table nxml-mode-abbrev-table))))

  (eval-after-load "nxml-mode"
    '(progn
       (define-key nxml-mode-map (kbd "<f8>") 'ted-linkify-region)
       (define-key nxml-mode-map (kbd "RET") 'newline-and-indent)
       (define-key nxml-mode-map (kbd "C-c C-t") 'sgml-tag))))

(when (locate-library "css-mode")
  (autoload 'css-mode "css-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (eval-after-load "css-mode"
    '(cond ((boundp 'cssm-indent-function) ; larsga's css-mode.el
            (add-hook 'css-mode-hook
                      (lambda ()
                        (setq cssm-mirror-mode nil
                              cssm-newline-before-closing-bracket nil
                              cssm-indent-function 'cssm-c-style-indenter))))
           ((fboundp 'css-extract-keyword-list) ; monnier's css-mode.el
            (setq css-basic-offset 2
                  css-indent-offset 2))
           (t nil))))
