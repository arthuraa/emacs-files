(require 'use-package)

(setq package-enable-at-startup nil)

(cl-labels ((add-path (path) (add-to-list 'load-path path)))
  (add-path "/usr/share/emacs/site-lisp")
  (mapc #'add-path (file-expand-wildcards "~/.emacs.d/site-lisp/*")))
