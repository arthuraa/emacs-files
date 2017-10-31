(require 'cl)

(defun core-setup-package ()

  (labels ((add-repo (name url)
                     (add-to-list 'package-archives
                                  (cons name url) t)))
    (add-repo "marmalade" "https://marmalade-repo.org/packages/")
    (add-repo "elpa" "http://tromey.com/elpa/")
    (add-repo "melpa" "http://melpa.milkbox.net/packages/"))

  (package-initialize))

(if (require 'package nil t)
    (core-setup-package)
  (message "package.el is not available"))

(defvar core-packages-list
  '(helm magit rainbow-mode zenburn-theme js2-mode
         markdown-mode auctex))

(defun core-install-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (package core-packages-list)
    (unless (package-installed-p package)
      (package-install package))))
