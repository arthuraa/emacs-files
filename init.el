(let ((start-time (float-time)))

  (require 'cl)

  (labels ((add-path (path) (add-to-list 'load-path path)))
    (add-path "/usr/share/emacs/site-lisp")
    (add-path (expand-file-name "~/.emacs.d"))
    (mapc #'add-path (file-expand-wildcards "~/.emacs.d/site-lisp/*")))

  (require 'util)

  (condition-case ex
      (require 'package-autoloads)
    ('error
     (util-compile-packages)
     (util-generate-package-autoloads)
     (require 'package-autoloads)))

  (mapc 'load (file-expand-wildcards "~/.emacs.d/custom/*.el"))

  (load "~/.emacs.d/site-lisp/proofgeneral/generic/proof-site.el")

  (condition-case ex
      (load "lilypond-init.el")
    ('error
     (message "lilypond is not available")))

  (server-start)

  (message (format "Startup time: %fs" (- (float-time) start-time))))
