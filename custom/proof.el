(defun set-proof-general-keys ()
  (local-set-key (kbd "C-c C-l") 'proof-goto-point))

(add-hook 'coq-mode-hook 'set-proof-general-keys)