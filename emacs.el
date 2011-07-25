(let ((start-time (float-time)))

  (require 'cl)

  (labels ((add-path (path) (add-to-list 'load-path path)))
    (add-path (expand-file-name "~/.emacs.d"))
    (mapc 'add-path (file-expand-wildcards "~/.emacs.d/site-lisp/*")))

  (require 'util)
  (require 'package)
  (package-initialize)
  (require 'package-autoloads)

  (mapc 'load (file-expand-wildcards "~/.emacs.d/custom/*"))

  (server-start)

  (message (format "Startup time: %fs" (- (float-time) start-time))))
