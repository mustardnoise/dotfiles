;;; init-self.el --- Self customed init file

;;; Commentary:

;; This file is used to manage self customed settings.

;;; Code:

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

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
(setq-default display-fill-column-indicator-column 121)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(prefer-coding-system 'utf-8)

(set-frame-font "Berkeley Mono 14")

(defvar display-time-24hr-format 1)
(defvar display-time-mail-string "")
(display-time-mode t)

(display-battery-mode 1)

(global-display-line-numbers-mode t)
(column-number-mode t)
(global-hl-line-mode t)
(global-display-fill-column-indicator-mode t)


(set-face-attribute 'fill-column-indicator nil :background nil :foreground "#4e4e4e")
(setq-default line-prefix " ")


(require 'dired)
(setq dired-use-ls-dired nil)
(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

;; tree-sitter

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        ;; (css "https://github.com/tree-sitter/tree-sitter-css")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        ;; (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        ;; (toml "https://github.com/tree-sitter/tree-sitter-toml")
        ;; (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        ;; (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (gitignore "https://github.com/shunsambongi/tree-sitter-gitignore")
        (hcl "https://github.com/MichaHoffmann/tree-sitter-hcl")
        (vue "https://github.com/ikatyang/tree-sitter-vue")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (kotlin "https://github.com/fwcd/tree-sitter-kotlin")))

(setq major-mode-remap-alist
      '(
        ;; (yaml-mode . yaml-ts-mode)
        ;; (bash-mode . bash-ts-mode)
        ;; (js2-mode . js-ts-mode)
        ;; (typescript-mode . typescript-ts-mode)
      (json-mode . json-ts-mode)
      ;; (css-mode . css-ts-mode)
      ;; (python-mode . python-ts-mode)
      (go-mode . go-ts-mode)
      (ruby-mode . ruby-ts-mode)
      (python-mode . python-ts-mode)))

(defun add-ruby-magic-comment ()
  "Add ruby magic comment to beginning of the file."
  (interactive)
  (when (or (eq major-mode 'ruby-mode) (eq major-mode 'ruby-ts-mode))
    (push-mark)
    (goto-char (point-min))
    (let ((magic-comment "# frozen_string_literal: true"))
      (unless (search-forward magic-comment nil t)
        (insert (concat magic-comment "\n\n"))))
    (goto-char (mark-marker))
    (pop-mark)))

(add-hook 'ruby-ts-mode-hook
          (lambda ()
            (when (buffer-file-name)
              (add-ruby-magic-comment))))

;; (load "server")
(server-start)

(require 'epg)
(setq epg-gpg-program "gpg")
(setq epg-pinentry-mode 'loopback)

(setenv "GPG_AGENT_INFO" nil)

(setq auth-sources
      '((:source "~/.authinfo.gpg")))

;; (setq-default org-startup-indented t
;;               org-pretty-entities t
;;               org-use-sub-superscripts "{}"
;;               org-hide-emphasis-markers t
;;               org-startup-with-inline-images t
;;               org-image-actual-width '(300))

;; (add-hook 'org-mode-hook 'org-appear-mode)

;; (setq epg-pinentry-mode 'loopback)


;; (defun decode-jwt (start end &optional jwt)
;;   "Decode JWT in region and print to help buffer."
;;   (interactive "r")
;;   (let* ((tok (if jwt jwt
;;                 (buffer-substring start end)))
;;          (data (s-split "\\." tok))
;;          (header (car data))
;;          (claims (cadr data)))
;;     (with-temp-buffer
;;       (insert (format "%s\n\n%s"
;;                       (base64-decode-string header t)
;;                       (base64-decode-string claims t)))
;;       (json-pretty-print-buffer)
;;       (with-output-to-temp-buffer "*JWT*"
;;         (princ (buffer-string)))))
;;   t)

(defvar dotemacs-file-map
  '((:root . "~/.emacs.d/init.el")
    (:lisp . "~/.emacs.d/lisp/"))
  "Map of Emacs configuration file locations.")

(defun dotemacs-open (config-name)
    "Open Emacs CONFIG-NAME config file.  If CONFIG-NAME is root, opens init.el.
Otherwise opens the corresponding file in the /lisp directory."
  (interactive)
  (find-file
   (if (eq config-name 'root)
       (cdr (assoc :root dotemacs-file-map))
     (format "%sinit-%s.el"
             (cdr (assoc :lisp dotemacs-file-map))
             config-name))))

;; Define all config commands in one pass
(dolist (config '((root . root)
                 (elpa . "melpa")
                 (self . "self")))
  (defalias (intern (format "dotemacs-%s" (car config)))
    `(lambda () (interactive)
       (dotemacs-open ',(if (stringp (cdr config))
                           (intern (cdr config))
                         (cdr config))))
    (format "Open Emacs %s config." (car config))))

(defun dotzshrc ()
  "Open .zshrc config file."
  (interactive)
  (find-file "~/.zshrc"))

(provide 'init-self)

;;; init-self.el ends here
