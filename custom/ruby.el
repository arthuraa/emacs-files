(dolist (pattern (list "\\.rake$" "Gemfile$" "Gemfile\\.lock$"))
  (add-to-list 'auto-mode-alist (cons pattern 'ruby-mode)))
