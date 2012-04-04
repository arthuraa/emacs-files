(defun set-proof-general-keys ()
  (local-set-key (kbd "C-c C-k") 'proof-goto-point))

(add-hook 'coq-mode-hook 'set-proof-general-keys)

(setq proof-three-window-enable t)
(setq proof-splash-enable nil)
