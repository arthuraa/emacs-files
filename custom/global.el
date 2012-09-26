(show-paren-mode t)
(ido-mode t)
(setq standard-indent 2)
(setq transient-mark-mode t)
(setq scroll-step 1)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 70)

(line-number-mode 1)
(column-number-mode 1)

(setq search-highlight t)
(global-font-lock-mode t)

(setq case-fold-search t)

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; set icon "tool tip" to show full path of current file
(setq-default icon-title-format frame-title-format)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun word-count ()
  "Count words in buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(add-hook 'after-save-hook 'whitespace-cleanup)
