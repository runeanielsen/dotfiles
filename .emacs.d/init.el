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
 custom-file "~/.emacs.d/var/custom-file.el"

 ;; Use your name in the frame title. :)
 frame-title-format (format "%s's Emacs" (capitalize user-login-name))

 ;; Do not create lockfiles.
 create-lockfiles nil

 ;; Don't use hard tabs
 indent-tabs-mode nil

 ;; Do not autosave.
 auto-save-default nil

 ;; set default tab char's display width to 4 spaces
 tab-width 4

 ;; Allow commands to be run on minibuffers.
 enable-recursive-minibuffers t)

;; Electric pair
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

;; Show parens pairs
(show-paren-mode 1)

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

;; --- No littering ---
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; --- Disable mouse ---
(use-package disable-mouse
  :config
  (global-disable-mouse-mode))

;; --- shut up ---
(use-package shut-up
  :config
 (when noninteractive
  (shut-up-silence-emacs)))

;; --- yasnippet ---
(use-package yasnippet
  :config
  (global-set-key (kbd "<C-tab>") 'yas-expand)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

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
        persp-add-buffer-on-find-file nil
        persp-switch-to-added-buffer nil
        persp-set-last-persp-for-new-frames nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-nil-name "dashboard"
        persp-auto-resume-time -1) ; Don't auto-load on startup
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
  (setq doom-modeline-height 24)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-major-mode-icon nil)
  (set-face-attribute 'mode-line nil :family "Fira Code" :height 85)
  (set-face-attribute 'mode-line-inactive nil :family "Fira Code" :height 85))

;; --- doom-themes ---
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package hc-zenburn-theme
  :config
  (load-theme 'hc-zenburn t))

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

(defun fp/split-window-balanced ()
  (interactive)
  (split-window-right)
  (balance-windows))

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
   "wn" '(fp/split-window-balanced :which-key "split-window-balanced")
   "ws" '(window-swap-states :which-key "window-swap-states")
   "wd" '(delete-window :which-key "delete-window"))

  (fp/leader-keys
   "o" '(:ignore t :which-key "open")
   "ot" '(projectile-run-vterm :which-key "vterm")
   "oT" '(vterm :which-key "vterm"))

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
   "ff" '(counsel-find-file :which-key "find-file")
   "fc" '(dired-create-empty-file :which-key "create-file")
   "fd" '(dired-create-directory :which-key "create-directory")
   "ft" '(counsel-load-theme :which-key "load-theme")))

;; --- Evil mode ---
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (mapc #'disable-mouse-in-keymap
      (list evil-motion-state-map
            evil-normal-state-map
            evil-visual-state-map
            evil-insert-state-map)))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

;; --- Hydra ---
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-decrease "out")
  ("k" text-scale-increase "in")
  ("f" nil "finished" :exit t))

(defhydra hydra-window-resize (:timeout 4)
  "window-resize"
  ("h" evil-window-decrease-width "decrease-width")
  ("j" evil-window-decrease-height "decrease-height")
  ("k" evil-window-increase-height "increase-height")
  ("l" evil-window-increase-width "increase-width")
  ("f" nil "finished" :exit t))

(fp/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "wr" '(hydra-window-resize/body :which-key "resize window"))

;; --- Dired ---
(defun quiet-auto-revert ()
  "A hook to run for buffers you want to revert automatically and silently"
  (auto-revert-mode 1)
  (setq-local auto-revert-verbose nil))

(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (add-hook 'dired-mode-hook #'quiet-auto-revert t nil)
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
  (setq ivy-use-selectable-prompt t))

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

(use-package magit-todos)

(use-package forge
  :config
  (ghub-request "GET" "/user" nil
                :forge 'github
                :host "api.github.com"
                :username "runeanielsen"
                :auth 'forge))

;; --- commmon lisp ---
(defvar inferior-lisp-program "sbcl")

(use-package sly)

;; --- Rainbow delimiters ---
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit)

;; --- Rainbow mode ---
(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         lisp-mode
         typescript-mode
         js-mode))

;; --- tree-sitter ---
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook ((go-mode . tree-sitter-hl-mode)
         (csharp-mode . tree-sitter-hl-mode)
         (js-mode . tree-sitter-hl-mode)
         (python-mode . tree-sitter-hl-mode)
         (css-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs)

;; --- Company mode ---
(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;; --- Flycheck ---
(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode)
  :config
  ; Hack because csharp lsp mode often bugs out
  (setq flycheck-checker-error-threshold 10000))

;; --- lsp mode ---
(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-links nil)
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions nil))

;; lsp-ivy
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; --- go mode ---
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  "LSP Go install save hooks."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :hook (go-mode . lsp)
  :config
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

;; --- protobuf ---
(use-package protobuf-mode)

;; --- json mode ---
(use-package json-mode)

;; --- csharp mode ---
(defun lsp-csharp-install-save-hooks ()
  "LSP CSharp install save hooks."
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(use-package csharp-mode
  :hook (csharp-mode . lsp-deferred)
  :config
  (add-hook 'csharp-mode-hook #'lsp-csharp-install-save-hooks))

;; --- python-ms ---
(use-package python-mode
  :custom
  (python-shell-interpreter "python3"))

(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))

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

(use-package tide
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'js-mode-hook #'setup-tide-mode)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (setq flycheck-enabled-checkers (append flycheck-enabled-checkers '(javascript-eslint)))
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))

;; --- Typescript mode ---
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :config
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

(use-package css-mode
  :ensure nil
  :hook (css-mode . lsp-deferred)
  :config
  (setq css-indent-offset 2))

;; --- Node modules path ---
(use-package add-node-modules-path
  :hook ((js-mode . add-node-modules-path)
         (css-mode . add-node-modules-path)
         (typescript-mode . add-node-modules-path)))

;; --- Prettier ---
(use-package prettier-js
  :hook ((js-mode . prettier-js-mode)
         (css-mode . prettier-js-mode)
         (scss-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)))

(use-package emmet-mode
  :hook ((sgml-mode . emmet-mode)
         (js-mode . emmet-mode)
         (web-mode . emmet-mode)
         (css-mode . emmet-mode))
  :config
  (setq emmet-expand-jsx-className? t))

;; --- markdown ---
(defun fp/set-markdown-header-font-sizes ()
  (dolist (face '((markdown-header-face-1 . 1.1)
                  (markdown-header-face-2 . 1.05)
                  (markdown-header-face-3 . 1.0)
                  (markdown-header-face-4 . 1.0)
                  (markdown-header-face-5 . 1.0)))
    (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

(defun fp/markdown-mode-hook ()
  (fp/set-markdown-header-font-sizes))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (add-hook 'markdown-mode-hook 'fp/markdown-mode-hook))

;; --- Org mode ---
(defun fp/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun fp/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Code" :weight 'regular :height 180))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . fp/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done t)
  (setq org-log-into-drawer t)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("errand" . ?E)
       ("task" . ?T)
       ("work" . ?W)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard" ((agenda "" ((org-deadline-warning-days 7)))))
          ("W" "Work Tasks" tags-todo "+work")))

  (fp/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Make gc pause faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
