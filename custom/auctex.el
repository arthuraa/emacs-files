;; Try to work around anoying bug that turns off PDF mode when finding an error
;; http://tex.stackexchange.com/questions/114316/auctex-global-pdf-mode-stop-working-after-typesetting-error-occurs

(defun my-tex-global-pdf-mode ()
  (setq TeX-PDF-mode t)
  (setq TeX-show-compilation t))

(add-hook 'TeX-mode-hook 'my-tex-global-pdf-mode)
(add-hook 'TeX-mode-hook 'turn-on-auto-fill)
