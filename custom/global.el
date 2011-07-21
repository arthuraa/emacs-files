(color-theme-solarized-light)

(dolist (pattern (list "\\.rake$" "Gemfile$" "Gemfile\\.lock$"))
  (add-to-list 'auto-mode-alist (cons pattern 'ruby-mode)))

(show-paren-mode t)
(ido-mode t)
(setq standard-indent 2)
(setq transient-mark-mode t)
(setq scroll-step 1)
(setq-default indent-tabs-mode nil) ; indentar com espaços
(setq-default fill-column 70)

(line-number-mode   1)
(column-number-mode 1)

(setq search-highlight t)
(global-font-lock-mode t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(coq-prog-name "/home/arthur/src/ssreflect-1.2/bin/ssrcoq")
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(js2-auto-indent-flag nil)
 '(js2-enter-indents-newline nil)
 '(js2-mirror-mode nil)
 '(js2-mode-escape-quotes nil))

;; ========== Place Backup Files in Specific Directory ==========

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; set icon "tool tip" to show full path of current file
(setq-default icon-title-format frame-title-format)

(put 'upcase-region 'disabled nil)

(add-hook 'c-mode-common-hook
          (lambda () (local-set-key "\C-c\C-k" 'compile)))

(defun word-count ()
  "Count words in buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(add-hook 'after-save-hook 'delete-trailing-whitespace)
