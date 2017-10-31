(require 'cl)

(defvar util-packages-home
  (expand-file-name "~/.emacs.d/site-lisp"))

(defvar generated-autoload-file
  (expand-file-name "~/.emacs.d/package-autoloads.el"))

(defvar util-package-source-dirs
  (remove-if-not #'file-accessible-directory-p
                 (file-expand-wildcards (concat util-packages-home "/*"))))

(defun util-compile-packages ()
  (interactive)
  (byte-recompile-directory util-packages-home 0))

(defun util-generate-package-autoloads ()
  (interactive)
  (mapc 'update-directory-autoloads util-package-source-dirs))

(provide 'util)
