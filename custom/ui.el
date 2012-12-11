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

(setq deja-vu-mono-sans
      "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso8859-1")

(setq deja-vu-mono-sans-bold
      "-unknown-DejaVu Sans Mono-bold-normal-normal-*-13-*-*-*-m-0-iso8859-1")
(setq inconsolata
      "-unknown-Inconsolata-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

(global-set-key [f11] 'toggle-fullscreen)

(mouse-wheel-mode t)
(set-default-font deja-vu-mono-sans)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(fringe-mode 4)

;(condition-case ex
;    (load-theme 'zenburn t)
;  ('error
;   (message "Couldn't load theme.")))
;
