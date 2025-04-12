(require 'package)
(require 'use-package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-load-list '(all (proof-general nil) (company-coq nil)))

(cl-labels ((add-path (path) (add-to-list 'load-path path)))
  (add-path "/usr/share/emacs/site-lisp")
  (mapc #'add-path (file-expand-wildcards "~/.emacs.d/site-lisp/*")))
