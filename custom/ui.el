(defun toggle-fullscreen ()
  "If the current frame is not in fullscreen mode, go to that
  state. Otherwise, restore the frame to the previous state."
  (interactive)
  (let ((current-frame-size (frame-parameter nil 'fullscreen)))
    (if (eql current-frame-size 'fullboth)
        (let ((previous (frame-parameter nil 'previous-frame-size)))
          (modify-frame-parameters nil
                                   `((previous-frame-size . nil)
                                     (fullscreen . ,previous))))
      (modify-frame-parameters nil
                               `((previous-frame-size . ,current-frame-size)
                                 (fullscreen . fullboth))))))

(global-set-key [f11] 'toggle-fullscreen)

(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")

(setq ring-bell-function #'ignore)
(mouse-wheel-mode t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(fringe-mode 4)
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(condition-case ex
    (load-theme 'zenburn t)
  ('error
   (message "Couldn't load theme.")))
