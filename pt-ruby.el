
(when (locate-library "ruby-mode")
  ;; Autoloads
  (autoload 'ruby-mode "ruby-mode" nil t)

  ;; File associations, etc.
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
  ;; fixme: use two-mode-mode when possible
  (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . html-mode))

  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

  ;; Key bindings
  (eval-after-load "ruby-mode"
    '(define-key ruby-mode-map (kbd "RET")
       'reindent-then-newline-and-indent))

  ;; Install key bindings for running an inferior Ruby in `ruby-mode'.
  (when (locate-library "inf-ruby")
    (autoload 'run-ruby "inf-ruby" nil t)
    (autoload 'inf-ruby-keys "inf-ruby" nil)
    (add-hook 'ruby-mode-hook 'inf-ruby-keys))

  ;; Skeletons

  (define-skeleton ted-rails-migrate-create-table
    "Skeleton for creating a table in a rails migration."
    "Table name: "
    > "create_table \"" str "\" do |t|" \n
    _ \n
    -2 "end" \n)

  (define-skeleton ted-rails-migrate-drop-table
    "Skeleton for dropping a table in a rails migration."
    "Table name: "
    > "drop_table \"" str "\"" \n)

  (define-skeleton ted-rails-migrate-table-column
    "Skeleton for adding a column in a rails migration."
    "Column name: "
    > "t.column \"" str "\", :" (skeleton-read "Column type: " "string"))

  (define-skeleton ted-rails-migrate-add-column
    "Skeleton for adding a column in a rails migration."
    "Table name: "
    > "add_column \"" str
    "\", \"" (skeleton-read "Column name: ")
    "\", :" (skeleton-read "Column type: " "string"))

  (define-skeleton ted-rails-migrate-remove-column
    "Skeleton for adding a column in a rails migration."
    "Table name: "
    > "remove_column \"" str
    "\", \"" (skeleton-read "Column name: ") "\""))
