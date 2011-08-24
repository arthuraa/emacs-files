(autoload 'opa-mode "opa-mode.el" "OPA editing mode." t)
(add-to-list 'auto-mode-alist '("\\.opa$" . opa-mode))

(defun opa-mode-customization ()
  (setq indent-line-function 'opa-indent-line
        indent-region-function 'opa-indent-region))

(add-hook 'opa-mode-hook 'opa-mode-customization)


