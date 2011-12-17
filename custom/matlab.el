(load-library "matlab-load")

(defun matlab-mode-add-keys ()
  (local-set-key (kbd "C-c C-r") 'matlab-shell-run-region-or-line)
  (local-set-key (kbd "C-c C-l") 'matlab-shell-run-cell))

(add-hook 'matlab-mode-hook 'matlab-mode-add-keys)

(setf matlab-shell-command "~/.emacs.d/custom/runmatlab.sh")
