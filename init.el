;;;; For global package setup, see early-init.el

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;;; Keys, global configurations, etc.

(use-package ws-butler
  :config
  (ws-butler-global-mode))

(show-paren-mode t)
(setq standard-indent 2)
(setq transient-mark-mode t)
(setq scroll-step 1)
(setq-default indent-tabs-mode nil)

(line-number-mode 1)
(column-number-mode 1)

(setq search-highlight t)
(global-font-lock-mode t)

(setq case-fold-search t)

(setq make-backup-files t)

;;;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)

;;;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backup/"))))

;;;; set icon "tool tip" to show full path of current file
(setq-default icon-title-format frame-title-format)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun word-count ()
  "Count words in buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)

(whitespace-mode)
(setq whitespace-style (cl-set-difference whitespace-style
                                          '(tabs newline spaces)))

;;;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;;;; meaningful names for buffers with the same name

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;;; by default re-builder doesn't read RE as strings, which means that you
;;;; can't copy and paste them in lisp code.
(require 're-builder)
(setq reb-re-syntax 'string)

;;;; replace default Emacs yes-or-no prompt
(fset 'yes-or-no-p 'y-or-n-p)

;;;; I keep quitting by accident all the time
(defun core-query-before-quit ()
  (y-or-n-p "Really exit Emacs?"))
(add-hook 'kill-emacs-query-functions
          'core-query-before-quit
          'append)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;; Very useful function stolen from emacs wiki
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

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

(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 140)

(setq ring-bell-function #'ignore)
(mouse-wheel-mode t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(fringe-mode 4)
(electric-indent-mode -1)

;;;; * Compilation settings
;;;;
;;;; By default, Emacs does a poor job of figuring out which command to use when
;;;; compiling a project.  The following code attempts to find a parent
;;;; directory that contains a Makefile to call make, while giving priority to
;;;; local definitions of compile-command when they are provided.

;;;; Use parallel make by default (-j)
(setq compile-command "make -k -j ")

(defun set-up-compile-command ()
  "Override the value of compile-command if there is a Makefile
in a parent directory, while making it buffer local. In this
case, the new compile-command will use the directory of the
Makefile to call make."
  (let ((makefile-dir (locate-dominating-file "." "Makefile")))
    (if makefile-dir
        (set (make-local-variable 'compile-command)
             (format "make -C %S -k -j " makefile-dir)))))

(defun compile-maybe-with-makefile ()
  "Call the compile function with the following compilation command:

1. The local value of compile-command, if one exists.

2. Change the directory to the nearest parent that contains a
Makefile, if one exists, then call make.  Use this as the local
value of compile-command henceforth.

3. The global value of compile-command, if no such parent exists."
  (interactive)
  (unless (assoc 'compile-command file-local-variables-alist)
    (set-up-compile-command))
  (call-interactively 'compile))

(global-set-key (kbd "C-c k") 'compile-maybe-with-makefile)

;;;; Enable direnv (manage environment variables on a per-directory basis)

(use-package direnv
  :config
  (direnv-mode))

;;;; * Theme

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; (use-package solarized-theme
;;   :config
;;   (load-theme 'solarized-light-high-contrast t))

;;;; * Multiple cursors

(use-package multiple-cursors
  :bind
  (("C-<" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-M->" . mc/skip-to-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package helm
  :config
  (helm-mode 1)
  :bind
  (("C-x C-b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x)))

(use-package tuareg)
(use-package utop
  :hook
  (tuareg-mode . utop-minor-mode))

(use-package magit
  :bind
  (("C-c g" . magit-status)))

;;;; Ensure that dead keys work
(require 'iso-transl)

;;;; Mode configurations

;;;; * Dired

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(use-package dired
  :ensure nil
  :config
  ; Use neighboring dired window as default file destination when copying file.
  (setq dired-dwim-target t)
  ; Use this binding to match what Helm uses in find-file
  :bind ("C-c C-x" . 'dired-open-file))

;;;; * Agda

(use-package agda2-mode)

;;;; * LilyPond

(use-package lilypond-mode
  :config
  (custom-set-variables
   '(LilyPond-pdf-command "evince")))

;;;; * Text

(defun setup-text-mode-isearch ()
  "Ensure that each space can match a newline in isearch"
  (make-local-variable 'search-whitespace-regexp)
  (setq search-whitespace-regexp "[ \t\n\r]+"))

(add-hook 'text-mode-hook 'setup-text-mode-isearch)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode)
(setq-default fill-column 80)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;;;; * TeX

; Allow moving to corresponding point in PDF with C-c C-v
(setq TeX-source-correlate-mode t)

;;;; * Haskell

(use-package haskell-mode
  :config
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-proceess-type 'cabal-repl))
  (eval-after-load 'haskell-mode '(progn
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
    (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
  (eval-after-load 'haskell-cabal '(progn
    (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-program-name "ghci"))

;;;; * Iris input mode

(require 'math-symbol-lists)

(add-hook 'coq-mode-hook (lambda () (set-input-method "iris")))

(defun iris-inherit-input-method ()
  "Inherit input method from `minibuffer-selected-window'."
  (let* ((win (minibuffer-selected-window))
         (buf (and win (window-buffer win))))
    (when buf
      (activate-input-method (buffer-local-value 'current-input-method buf)))))
(add-hook 'minibuffer-setup-hook #'iris-inherit-input-method)

(quail-define-package "iris" "UTF-8" "▷" t)
(quail-define-rules
 ("\\fun"    ?λ)
 ("\\mult"   ?⋅)
 ("\\ent"    ?⊢)
 ("\\valid"  ?✓)
 ("\\diamond" ?◇)
 ("\\box"    ?□)
 ("\\bbox"   ?■)
 ("\\later"  ?▷)
 ("\\pred"   ?φ)
 ("\\and"    ?∧)
 ("\\or"     ?∨)
 ("\\comp"   ?∘)
 ("\\ccomp"  ?◎)
 ("\\all"    ?∀)
 ("\\ex"     ?∃)
 ("\\to"     ?→)
 ("\\sep"    ?∗)
 ("\\lc"     ?⌜)
 ("\\rc"     ?⌝)
 ("\\Lc"     ?⎡)
 ("\\Rc"     ?⎤)
 ("\\lam"    ?λ)
 ("\\empty"  ?∅)
 ("\\Lam"    ?Λ)
 ("\\Sig"    ?Σ)
 ("\\-"      ?∖)
 ("\\aa"     ?●)
 ("\\af"     ?◯)
 ("\\auth"   ?●)
 ("\\frag"   ?◯)
 ("\\iff"    ?↔)
 ("\\gname"  ?γ)
 ("\\incl"   ?≼)
 ("\\latert" ?▶)
 ("\\update" ?⇝)

 ("\\\"o" ?ö)

 ("^^+" ?⁺) ("__+" ?₊) ("^^-" ?⁻)
 ("__0" ?₀) ("__1" ?₁) ("__2" ?₂) ("__3" ?₃) ("__4" ?₄)
 ("__5" ?₅) ("__6" ?₆) ("__7" ?₇) ("__8" ?₈) ("__9" ?₉)

 ("__a" ?ₐ) ("__e" ?ₑ) ("__h" ?ₕ) ("__i" ?ᵢ) ("__k" ?ₖ)
 ("__l" ?ₗ) ("__m" ?ₘ) ("__n" ?ₙ) ("__o" ?ₒ) ("__p" ?ₚ)
 ("__r" ?ᵣ) ("__s" ?ₛ) ("__t" ?ₜ) ("__u" ?ᵤ) ("__v" ?ᵥ) ("__x" ?ₓ)
)

(let ((symbols (append math-symbol-list-basic math-symbol-list-extended)))
  (dolist (symbol symbols)
    (cl-destructuring-bind (kind keys &optional char &optional _) symbol
      (when (and char (not (member kind '("Greek" "greek"))))
        (quail-defrule keys char "iris" t)))))

;;;; * Org

;;;; Make windmove work in org-mode:

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;;;; * Proof General / Coq

(defun set-proof-general-keys ()
  (local-set-key (kbd "C-c C-k") 'proof-goto-point))

(add-hook 'coq-mode-hook 'set-proof-general-keys)
(add-hook 'coq-mode-hook 'company-coq-mode)
(setq company-coq-disabled-features
      '(prettify-symbols
        title-comments
        coqdoc
        smart-subscripts
        code-folding
        company))

(setq proof-three-window-enable t)
(setq proof-splash-enable nil)

;;;; Customization of PG for ssreflect syntax
;;;; Assia Mahboubi 2007

(defcustom coq-user-tactics-db
   '(("nat_congr" "ncongr"  "nat_congr" t "nat_congr")
     ("nat_norm" "nnorm"  "nat_norm" t "nat_norm")
     ("bool_congr" "bcongr"  "bool_congr" t "bool_congr")
     ("prop_congr" "prcongr"  "prop_congr" t "prop_congr")
     ("move" "m"  "move" t "move")
     ("set" "set"  "set # := #" t "set")
     ("have" "hv" "have # : #" t "have")
     ("congr" "con" "congr #" t "congr")
     ("wlog" "wlog" "wlog : / #" t "wlog")
     ("without loss" "wilog" "without loss #" t "without loss")
     ("unlock" "unlock" "unlock #" t "unlock")
     ("suffices" "suffices" "suffices # : #" t "suffices")
     ("suff" "suff" "suff # : #" t "suff")
)
   "Extended list of tactics, includings ssr and user defined ones")


(defcustom coq-user-commands-db
  '(("Prenex Implicits" "pi" "Prenex Implicits #" t "Prenex\\s-+Implicits")
    ("Hint View for" "hv" "Hint View for #" t "Hint\\s-+View\\s-+for")
    ("inside" "ins" nil f "inside")
    ("outside" "outs" nil f "outside")
    ("Canonical " nil "Canonical  #." t "Canonical")
)
   "Extended list of commands, includings ssr and user defined ones")

(defcustom coq-user-tacticals-db
  '(("last" "lst" nil t "last"))
  "Extended list of tacticals, includings ssr and user defined ones")

(defcustom coq-user-reserved-db
  '("is" "nosimpl" "of")
  "Extended list of keywords, includings ssr and user defined ones")

(defcustom coq-user-solve-tactics-db
  '(("done" nil "done" nil "done")
    )
   "Extended list of closing tactic(al)s, includings ssr and user defined ones")

(defcustom coq-variable-highlight-enable nil
  "Activates partial bound variable highlighting"
)

;;;; * Proverif

(add-hook 'proverif-pv-mode-hook
  (lambda ()
    (unless (or (file-exists-p "makefile")
                (file-exists-p "Makefile"))
      (set (make-local-variable 'compile-command)
           (concat "proverif "
                   (if buffer-file-name
                       (shell-quote-argument buffer-file-name)))))))

;;;; * SCSS

(use-package scss-mode
  :config
  (defun set-scss-variables ()
    (setq scss-compile-at-save nil))
  (add-hook 'scss-mode-hook 'set-scss-variables))


;;;; * LSP

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :bind (("C-c p" . lsp-coq-proof/goals))
  :config (use-package lsp-coq)
  :commands lsp)

;;;; * Shell-here

(use-package shell-here)
