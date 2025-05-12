;; init-melpa.el --- melpa init file.

;;; commentary:

;; this file is used to manage melpa packages' settings.

;;; code:

(require 'package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'diminish)

;; install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package diminish)

;; (use-package enh-ruby-mode
;;   :config
;;   (add-to-list
;;    'auto-mode-alist
;;    '("\\(?:\\.rb\\|ru\\|rake\\|gemspec\\|/\\(?:gem\\|rake\\)file\\)\\'" . enh-ruby-mode))
;;   (add-hook 'enh-ruby-mode-hook 'robe-mode)
;;   (setq enh-ruby-deep-indent-paren nil)
;;   (setq enh-ruby-add-encoding-comment-on-save nil))

;; (use-package ruby-end)

(defadvice minitest--run-command (around minitest--run-command-around)
  "Use bash shell for running the specs because of zsh issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'minitest--run-command)

(defadvice minitest-verify-all (around minitest-verify-all-around)
  "Use docker to run all test."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(use-package minitest
  :config
  (setq minitest-use-rails t)
  (setq minitest-use-docker t)
  (setq minitest-docker-container "dev")
  :bind-keymap
  ("C-c ," . minitest-mode-map))

(use-package smartparens
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode))
(require 'smartparens-config)

(use-package helm)

;; (use-package railscasts-reloaded-theme
;;   :config
;;   (load-theme 'railscasts-reloaded t))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t)
  (set-face-attribute 'line-number nil :background "#282828" :foreground "#4e4e4e"))

(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(ruby-reek))
  (setq-default flycheck-elixir-credo-strict t))

;; (use-package flycheck-rust)

(use-package company
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package forge
  :after magit
  :config)
  ;; (add-to-list 'ghub-insecure-hosts "gitlab.share-now.com/api/v4")
  ;; (add-to-list 'forge-alist '("gitlab.share-now.com" "gitlab.share-now.com/api/v4" "gitlab.share-now.com:10088" forge-gitlab-repository)))

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

(use-package counsel-tramp
  :config
  (global-set-key (kbd "C-c s") 'counsel-tramp)
  (setq make-backup-files nil)
  (setq create-lockfiles nil))

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
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  :config (helm-projectile-on))

(use-package robe
  :diminish (robe-mode)
  :after inf-ruby company
  :init (push 'company-robe company-backends))

(use-package elixir-mode
  :ensure t
  :diminish (elixir-mode)
  :commands projectile-project-p
  :init
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  (add-hook 'elixir-format-hook (lambda ()
                                  (if (projectile-project-p)
                                      (setq elixir-format-arguments
                                            (list "--dot-formatter"
                                                  (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                                    (setq elixir-format-arguments nil))))
  :config
  (defcustom exunit-docker-container "dev"
    "Name of the docker container to run tests."
    :type 'string
    :group 'elixir-mode
    :safe 'stringp)
  (setq exunit-mix-command
        (lambda (args)
          (let* ((args (s-join " " args))
                 (mix-command (concat "'MIX_ENV=test mix test " args "'")))
            ;; Adjust to your needs
            (list "docker compose" "exec" exunit-docker-container "sh" "-c" mix-command)))))

(use-package elixir-ts-mode
  :ensure t
  :commands projectile-project-p
  :init
  (add-hook 'elixir-ts-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  (add-hook 'elixir-format-hook (lambda ()
                                  (if (projectile-project-p)
                                      (setq elixir-format-arguments
                                            (list "--dot-formatter"
                                                  (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                                    (setq elixir-format-arguments nil))))
  :config
  (defcustom exunit-docker-container "dev"
    "Name of the docker container to run tests."
    :type 'string
    :group 'elixir-mode
    :safe 'stringp)
  (setq exunit-mix-command
        (lambda (args)
          (let* ((args (s-join " " args))
                 (mix-command (concat "'MIX_ENV=test mix test " args "'")))
            ;; Adjust to your needs
            (list "docker compose" "exec" exunit-docker-container "sh" "-c" mix-command)))))

(use-package mix
  :ensure t
  :diminish (mix)
  :init (add-hook 'elixir-mode-hook 'mix-minor-mode)
  :config
  (setq compilation-scroll-output t))

(use-package exunit
  :ensure t
  :diminish (exunit)
  :init (add-hook 'elixir-mode-hook 'exunit-mode))

(use-package go-ts-mode
  :ensure t
  :mode "\\.go\\'")

(use-package lsp-mode
  :commands lsp
  :ensure t
  :diminish (lsp-mode)
  :hook
  (elixir-mode . lsp)
  (ruby-mode . lsp)
  (ruby-ts-mode . lsp)
  (tex-mode . lsp)
  (latex-mode . lsp)
  (elixir-ts-mode . lsp)
  (go-ts-mode . lsp)
  (python-mode . lsp)
  (python-ts-mode . lsp)
  :init
  (add-to-list 'exec-path (concat (getenv "HOME") "/Projects/elixir-ls/releases"))
  :config
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-client-packages '(lsp-elixir lsp-ruby-lsp lsp-javascript lsp-tex lsp-go lsp-pylsp)) ;; lsp-ruby-syntax-tree
  ;; (setq lsp-diagnostics-provider :none)
  (setq lsp-enable-snippet nil)
  ;; (setq lsp-latex-texlab-executable (concat (getenv "HOME") "/.asdf/shims/texlab"))
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :custom
  (lsp-pylsp-plugins-black-enabled t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable nil))

(use-package lsp-treemacs
  :after lsp-mode
  :config
  (lsp-treemacs-sync-mode 1))

(use-package dap-mode
  :ensure t
  :diminish t
  :after lsp-mode
  :config
  (require 'dap-node)
  (require 'dap-elixir)
  ;; (dap-node-setup)
  (dap-ui-mode 1)
  (dap-mode 1)
  (dap-auto-configure-mode)
  )

;; (use-package dap-elixir)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

(use-package rspec-mode
  :diminish rspec-mode
  :init (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  :config
  ;; (setq rspec-use-rvm t)
  (setq compilation-scroll-output t))

(use-package markdown-mode
  :diminish markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk))

(use-package telephone-line
  :init (telephone-line-mode))

(use-package embrace
  :init (global-set-key (kbd "C-,") #'embrace-commander))

(use-package transpose-frame)

(use-package restclient)

;; (use-package racer)

;; (use-package rust-mode
;;   :bind (:map rust-mode-map ("TAB" . company-indent-or-complete-common))
;;   :config
;;   (add-hook 'rust-mode-hook 'cargo-minor-mode)
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;;   (add-hook 'racer-mode-hook #'company-mode)
;;   (setq rust-format-on-save t))

;; (use-package gitlab-ci-mode)

;; (use-package gitlab-ci-mode-flycheck
;;   :after flycheck gitlab-ci-mode
;;   :init
;;   (gitlab-ci-mode-flycheck-enable)
;;   (setq gitlab-ci-url "https://gitlab.share-now.com")
;;   (setq gitlab-ci-api-token (getenv "GITLAB_TOKEN")))

;; (use-package lab
;;   :init
;;   (setq lab-host "https://gitlab.share-now.com")
;;   (setq lab-token (getenv "GITLAB_TOKEN"))
;;   (setq lab-group "38"))

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package bundler)

(use-package web-mode
  :mode "\\.vue$"
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        ;; web-mode-sql-indent-offset 2
        web-mode-script-padding 0       ; start script in col 0
        ;; web-mode-enable-current-column-highlight t
        )
  ;; :custom-face
  ;; light color for highlighting the current HTML element's column
  ;; (web-mode-current-column-highlight-face
  ;;                     ((t (:background "#f0f0f0"))))
  )

(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package csv-mode)

(setq flycheck-python-pycompile-executable "python3")

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;; (use-package k8s-mode
;;   :ensure t
;;   :hook (k8s-mode . yas-minor-mode))

(use-package hcl-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package sqlite3
  :ensure t)

(use-package avy
  :ensure t)

(use-package crystal-mode
  :ensure t)

;; (use-package org-appear
;;   :ensure t
;;   :hook (org-mode . org-appear-mode)
;;   :custom
;;   (org-appear-autolinks t))

(use-package mise
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-mise-mode))

(provide 'init-melpa)

;;; init-melpa.el ends here
