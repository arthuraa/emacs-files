(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(load-file
 (let ((coding-system-for-read 'utf-8))
   (expand-file-name "agda-input.el"
                     (file-name-directory
                      (shell-command-to-string "agda-mode locate")))))

(agda-input-setup)
