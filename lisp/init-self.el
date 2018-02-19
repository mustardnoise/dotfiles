;;; init-self.el --- Self customed init file

;;; Commentary:

;; This file is used to manage self customed settings.

;;; Code:

(put 'upcase-region 'disabled nil)

(defvar backup-directory (concat user-emacs-directory "emacs-backups"))
(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))
(setq backup-directory-alist `(("." . ,backup-directory)))

(setq auto-save-default nil)
(setq auto-window-vscroll nil)
(setq inhibit-startup-screen t)
(setq package-enable-at-startup nil)
(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 80)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(prefer-coding-system 'utf-8)
(set-frame-font "Menlo 14")

(defvar display-time-24hr-format 1)
(defvar display-time-mail-string "")
(display-time-mode t)

(require 'linum)
(setq linum-format " %d ")
(global-linum-mode 1)
(global-hl-line-mode 1)

(require 'dired)
(setq dired-use-ls-dired nil)
(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

(provide 'init-self)

;;; init-self.el ends here
