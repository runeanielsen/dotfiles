;;; --- Garbage collection speedup ---
(setq gc-cons-threshold (* 50 1000 1000))

;;; --- Set up 'package' ---
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Font
(set-face-attribute 'default nil :font "Fira Code" :height 120)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; --- Use better defaults ---
(setq-default
 ;; Don't use the compiled code if its the older package.
 load-prefer-newer t

 ;; Do not show the startup message.
 inhibit-startup-message t

 ;; Do not put 'customize' config in init.el; give it another file.
 custom-file "~/.emacs.d/custom-file.el"

 ;; Use your name in the frame title. :)
 frame-title-format (format "%s's Emacs" (capitalize user-login-name))

 ;; Do not create lockfiles.
 create-lockfiles nil

 ;; Don't use hard tabs
 indent-tabs-mode nil

 ;; Emacs can automatically create backup files. This tells Emacs to put all backups in
 ;; ~/.emacs.d/backups. More info:
 ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

 ;; Do not autosave.
 auto-save-default nil

 ;; set default tab char's display width to 4 spaces
 tab-width 4

 ;; Allow commands to be run on minibuffers.
 enable-recursive-minibuffers t)

;; Electrical pair
(electric-pair-mode 1)

;; Change all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable narrowing commands.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Display column number in mode line.
(column-number-mode t)

;; Automatically update buffers if file content on the disk has changed.
(global-auto-revert-mode t)

;; No blinking cursor
(blink-cursor-mode 0)

;; make indent commands use space only (never tab character)
(progn
  (setq-default indent-tabs-mode nil))

;; display line numbers
(global-display-line-numbers-mode)

;; --- Disable unnecessary UI elements ---
(progn
  ;; Do not show menu bar.
  (menu-bar-mode -1)

  ;; Do not show tool bar.
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

  ;; Do not show scroll bar.
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; Highlight line on point.
  (global-hl-line-mode t))

;; --- Projectile ---
(use-package projectile
    :custom
    (projectile-completion-system 'ivy)
    (projectile-enable-caching nil)
    (projectile-indexing-method 'alien)
    (projectile-track-known-projects-automatically nil)
    :config
    (projectile-mode +1))

;; --- Persp-mode ---
(use-package persp-mode)

(with-eval-after-load "persp-mode-autoloads"
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-set-last-persp-for-new-frames nil
        persp-switch-to-added-buffer nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time -1 ; Don't auto-load on startup
        persp-auto-save-opt (if noninteractive 0 1)) ; auto-save on kill
  (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))

(use-package persp-mode-projectile-bridge)

(add-hook 'persp-mode-hook #'(lambda ()
                               (persp-mode-projectile-bridge-mode 1)))

(with-eval-after-load "persp-mode-projectile-bridge-autoloads"
  (add-hook 'persp-mode-projectile-bridge-mode-hook
            #'(lambda ()
                (if persp-mode-projectile-bridge-mode
                    (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                  (persp-mode-projectile-bridge-kill-perspectives))))
  (add-hook 'after-init-hook
            #'(lambda ()
                (persp-mode-projectile-bridge-mode 1))
            t))

;; --- dashboard ---
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((agenda . 5)))
  (setq dashboard-center-content t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner "~/.emacs.d/banner-text.txt")
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-footer nil))

;; --- Helpful ---
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; --- All-the-icons ---
(use-package all-the-icons)

;; --- vterm ---
(use-package vterm
  :config
  (setq vterm-shell "zsh"))

;; --- Modeline ---
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-height 34)
  (set-face-attribute 'mode-line nil :family "Monospace" :height 110)
  (set-face-attribute 'mode-line-inactive nil :family "Monospace" :height 110))

;; --- doom-themes ---
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-spacegrey t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; --- Theme Magic Pywal ---
(use-package theme-magic
  :config
  (theme-magic-export-theme-mode))

;; --- Linum relative ---
(use-package linum-relative
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (linum-relative-on))

(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                sly-mrepl-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; --- automatically clean whitespace ---
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; --- General ---
(use-package general
  :config
  (general-create-definer fp/leader-keys
    :keymaps '(normal visual)
    :prefix "SPC")

  (fp/leader-keys
   "TAB" '(:ignore t :which-key "persp")
   "TAB TAB" '(persp-switch :which-key "persp-switch")
   "TAB n" '(persp-add-new :which-key "persp-add-new")
   "TAB h" '(persp-prev :which-key "persp-prev")
   "TAB l" '(persp-next :which-key "persp-next"))

  (fp/leader-keys
   "SPC" '(counsel-projectile-find-file :which-key "find-file"))

  (fp/leader-keys
   "d" '(:ignore t :which-key "dired")
   "dd" '(dired-jump :which-key "dired-jump")
   "dp" '(projectile-dired :which-key "dired-projectile"))

  (fp/leader-keys
   "b" '(:ignore t :which-key "buffer")
   "bb" '(persp-switch-to-buffer :which-key "switch-to-buffer-persp")
   "bB" '(counsel-projectile-switch-to-buffer :which-key "switch-to-buffer-all")
   "ba" '(persp-add-buffer :which-key "persp-add-buffer")
   "bd" '(bury-buffer :which-key "bury-current-buffer")
   "bk" '(persp-kill-buffer :which-key "kill-current-buffer")
   "bs" '(evil-write :which-key "write-buffer")
   "bS" '(evil-write-all :which-key "write-buffer-all")
   "bl" '(evil-switch-to-windows-last-buffer :which-key "switch-last-buffer"))

  (fp/leader-keys
   "g" '(:ignore t :which-key "git")
   "gg" '(magit-status :which-key "magit-status"))

  (fp/leader-keys
   "s" '(:ignore t :which-key "search")
   "ss" '(swiper :which-key "swiper")
   "sg" '(counsel-projectile-rg :which-key "ripgrep-projectile"))

  (fp/leader-keys
   "c" '(:ignore t :which-key "code")
   "cr" '(lsp-rename :which-key "rename")
   "cR" '(lsp-workspace-restart :which-key "workspace-restart")
   "cc" '(comment-or-uncomment-region :which-key "comment-or-uncomment-region")
   "ca" '(lsp-execute-code-action :which-key "code-action")
   "ci" '(lsp-goto-implementation :which-key "goto-implementation")
   "ct" '(lsp-goto-type-definition :which-key "goto-type-definition")
   "co" '(lsp-organize-imports :which-key "organize-imports"))

  (fp/leader-keys
   "w" '(:ignore t :which-key "window")
   "ww" '(other-window :which-key "other-window")
   "wn" '(split-window-right :which-key "split-window-right")
   "wd" '(delete-window :which-key "delete-window"))

  (fp/leader-keys
   "o" '(:ignore t :which-key "open")
   "ot" '(vterm :which-key "vterm"))

  (fp/leader-keys
   "p" '(:ignore t :which-key "projectile")
   "pa" '(projectile-add-known-project :which-key "add-project")
   "pd" '(projectile-remove-known-project :which-key "remove-project")
   "pp" '(counsel-projectile-switch-project :which-key "switch-project")
   "pi" '(projectile-invalidate-cache :which-key "invalidate-cache")
   "pb" '(counsel-projectile-switch-to-buffer :which-key "switch-buffer")
   "pk" '(persp-kill :which-key "kill-project"))

  (fp/leader-keys
   "f"  '(:ignore t :which-key "files")
   "ff" '(counsel-find-file :which-key "find-file")))

;; --- Evil mode ---
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

;; --- Dired ---
(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "f" 'dired-create-empty-file
    "F" 'dired-create-directory))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; --- Counsel ---
(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

;; --- Ivy ---
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; --- Magit ---
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(ghub-request "GET" "/user" nil
              :forge 'github
              :host "api.github.com"
              :username "runeanielsen"
              :auth 'forge)

;; --- go mode ---
(use-package go-mode)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  "LSP Go install save hooks."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; --- json mode ---
(use-package json-mode)

;; --- csharp mode ---
(use-package csharp-mode)

(defun lsp-csharp-install-save-hooks ()
  "LSP CSharp install save hooks."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'csharp-mode-hook #'lsp-csharp-install-save-hooks)

;; --- python-ms ---
(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))

;; --- commmon lisp ---
(defvar inferior-lisp-program "sbcl")

(use-package sly)

;; --- lsp mode ---
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
    :hook (
            (csharp-mode . lsp)
            (go-mode . lsp)
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp
    :config
    (setq lsp-enable-links nil)
    (setq lsp-headerline-breadcrumb-enable nil))

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions nil))

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; Ivy
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; optional if you want which-key integration
(use-package which-key

    :config
    (which-key-mode))

;; --- Flycheck ---
(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

;; --- Tide ---
(defun setup-tide-mode ()
  "Setup Tide Mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(use-package tide
  :config
    (defvar company-tooltip-align-annotations)
    (setq company-tooltip-align-annotations t)
    (add-hook 'js-mode-hook #'setup-tide-mode))

(use-package js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)
(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

(use-package add-node-modules-path)
(use-package web-mode)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))

(eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'scss-mode-hook #'add-node-modules-path)
       (add-hook 'js2-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'prettier-js-mode)))

;; set default indent offset on modes
(setq-default
  js-indent-level 2
  css-indent-offset 2
  web-mode-code-indent-offset 2
  web-mode-css-indent-offset 2
  web-mode-markup-indent-offset 2)

(use-package prettier-js
    :init
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'web-mode-hook 'prettier-js-mode)
    (add-hook 'scss-mode-hook 'prettier-js-mode))

;; --- markdown ---
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun dw/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))
  (defun dw/markdown-mode-hook ()
    (dw/set-markdown-header-font-sizes))
  (add-hook 'markdown-mode-hook 'dw/markdown-mode-hook))

;; --- Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; --- Rainbow mode
(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         lisp-mode
         typescript-mode
         js2-mode))

;; --- Org mode
(defun fp/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1))

(defun fp/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org
  ;;:hook (org-mode . fp/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (fp/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun fp/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook
  (org-mode . fp/org-mode-visual-fill))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
