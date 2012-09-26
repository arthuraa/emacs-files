(require 'cl)
(require 'package)

(labels ((add-repo (name url)
                   (add-to-list 'package-archives
                                (cons name url) t)))
  (add-repo "marlalade" "http://marmalade-repo.org/packages/")
  (add-repo "elpa" "http://tromey.com/elpa/")
  (add-repo "melpa" "http://melpa.milkbox.net/packages/"))

;(setq url-http-attempt-keepalives nil)

(defvar core-packages-list
  '(helm magit melpa rainbow-mode zenburn-theme))

(defun core-install-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (package core-packages-list)
    (unless (package-installed-p package)
      (package-install package))))
