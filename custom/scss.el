(defun set-scss-variables ()
  (setq scss-compile-at-save nil))

(add-hook 'scss-mode-hook 'set-scss-variables)
