;;; init-melpa.el --- Melpa init file.

;;; Commentary:

;; This file is used to manage melpa packages' settings.

;;; Code:

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'diminish)

(use-package diminish)

(use-package enh-ruby-mode
  :config
  (add-to-list
   'auto-mode-alist
   '("\\(?:\\.rb\\|ru\\|rake\\|gemspec\\|/\\(?:Gem\\|Rake\\)file\\)\\'" . enh-ruby-mode))
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'minitest-mode)
  (setq enh-ruby-deep-indent-paren nil)
  (setq enh-ruby-add-encoding-comment-on-save nil))

(use-package minitest
  :config
  (setq minitest-use-rails t)
  (setq minitest-use-docker t)
  (setq minitest-docker-container "dev"))

(use-package fill-column-indicator
  :config
  (add-hook 'prog-mode-hook 'turn-on-fci-mode)
  (add-hook 'text-mode-hook 'turn-on-fci-mode))

(use-package smartparens
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode))
(require 'smartparens-config)

(use-package helm)

(use-package railscasts-reloaded-theme
  :config
  (load-theme 'railscasts-reloaded t))

(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode)
  :config (setq-default flycheck-disabled-checkers '(ruby-reek)))

(use-package flycheck-credo
  :defer t
  :init (add-hook 'elixir-mode-hook 'flycheck-credo-setup)
  :config
  (setq flycheck-elixir-credo-strict t))

(use-package flycheck-rust)

(use-package company
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package forge
  :after magit)

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (defvar undo-directory (concat user-emacs-directory "undo"))
  (if (not (file-exists-p undo-directory))
      (make-directory undo-directory t))
  (setq undo-tree-history-directory-alist `(("." . ,undo-directory))))

(use-package disable-mouse
  :diminish (disable-mouse-mode global-disable-mouse-mode)
  :init (add-hook 'after-init-hook 'global-disable-mouse-mode))

(use-package highlight-indent-guides
  :init (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\⫶))

(use-package counsel)

(use-package counsel-tramp)

(use-package ivy
  :config
  (counsel-mode +1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package helm-projectile
  :config (helm-projectile-on))

(use-package robe
  :diminish (robe-mode)
  :after inf-ruby company
  :init (push 'company-robe company-backends))

(use-package alchemist)

(use-package rvm
  :init (rvm-use-default)
  :config
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))

(use-package rspec-mode
  :diminish rspec-mode
  :config
  (setq rspec-use-rvm t)
  (setq compilation-scroll-output t))

(use-package linum-relative
  :config (linum-relative-global-mode))

(use-package magit-gh-pulls
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package markdown-mode
  :diminish markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk))

(use-package telephone-line
  :init (telephone-line-mode))

(use-package embrace
  :init (global-set-key (kbd "C-,") #'embrace-commander))

(use-package transpose-frame)

(use-package restclient)

(use-package haskell-mode)

(use-package racer)

(use-package rust-mode
  :bind (:map rust-mode-map ("TAB" . company-indent-or-complete-common))
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (setq rust-format-on-save t))

(use-package gitlab-ci-mode)

(use-package gitlab-ci-mode-flycheck
  :after flycheck gitlab-ci-mode
  :init
  (gitlab-ci-mode-flycheck-enable))

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package bundler)

(use-package vue-mode
  :config
  (add-hook 'vue-mode-hook (lambda () (setq syntax-ppss-table nil))))

(setq flycheck-python-pycompile-executable "python3")

(provide 'init-melpa)

;;; init-melpa.el ends here
