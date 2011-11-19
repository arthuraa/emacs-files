(load-library "matlab-load")

(defun matlab-mode-add-keys ()
  (local-set-key (kbd "C-c C-c <C-return>") 'matlab-shell-run-region-or-line))

(add-hook 'matlab-mode-hook 'matlab-mode-add-keys)

(setf matlab-shell-command "~/.emacs.d/custom/runmatlab.sh")
