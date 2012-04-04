(defun set-proof-general-keys ()
  (local-set-key (kbd "C-c C-k") 'proof-goto-point))

(defun set-proof-general-vars ()
  (setq proof-three-window-enable t))

(add-hook 'coq-mode-hook 'set-proof-general-keys)
(add-hook 'coq-mode-hook 'set-proof-general-vars)

(setq proof-splash-enable nil)
