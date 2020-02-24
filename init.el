(package-initialize)

(require 'cl)

(require 'ws-butler)
(ws-butler-global-mode)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(cl-labels ((add-path (path) (add-to-list 'load-path path)))
  (add-path "/usr/share/emacs/site-lisp")
  (mapc #'add-path (file-expand-wildcards "~/.emacs.d/site-lisp/*")))

(mapc 'load (file-expand-wildcards "~/.emacs.d/custom/*.el"))

(condition-case ex
    (load "~/.emacs.d/package-autoloads.el")
  ('error
   (util-compile-packages)
   (util-generate-package-autoloads)
   (load "~/.emacs.d/package-autoloads.el")))

(load "~/.emacs.d/site-lisp/proofgeneral/generic/proof-site.el")
(load "~/.emacs.d/site-lisp/pg-ssr.el")

(condition-case ex
    (load "lilypond-init.el")
  ('error
   (message "lilypond is not available")))

(server-start)

(defun opam-env ()
  (interactive)
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))

