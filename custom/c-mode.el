(defun c-mode-keybindings ()
  (local-set-key (kbd "C-c C-k") 'compile))

(add-hook 'c-mode-common-hook 'c-mode-keybindings)
