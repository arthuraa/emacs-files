(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(setq deja-vu-mono-sans
      "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso8859-1")

(setq deja-vu-mono-sans-bold
      "-unknown-DejaVu Sans Mono-bold-normal-normal-*-13-*-*-*-m-0-iso8859-1")
(setq inconsolata
      "-unknown-Inconsolata-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

(global-set-key [f11] 'fullscreen)

(mouse-wheel-mode t)
(set-default-font deja-vu-mono-sans)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(fringe-mode 4)

(require 'color-theme-zenburn)
(color-theme-zenburn)
