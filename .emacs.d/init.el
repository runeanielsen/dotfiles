;;; package --- Summary: init.el file.

;;; Commentary:
;; Personal Emacs setup.

;;; Code:

;; Set garbage collection threshold to 1GB to speed up startup.
(setq gc-cons-threshold #x40000000)

;;; --- Set up 'package' ---
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Load helper package
(require 'straight-x)

;; Native compilation
(setq comp-async-report-warnings-errors nil)
(setq package-native-compile t)

;; gcmh
(use-package gcmh
  :config
  (gcmh-mode 1))

;; Increase the amount of data which Emacs reads from process to 3mb
(setq read-process-output-max (* 3072 1024))

;; Font
(set-face-attribute 'default nil :font "Fira Code" :height 110)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; --- Use better defaults ---
(setq-default
 ;; Don't use the compiled code if its the older package.
 load-prefer-newer t

 ;; Do not show the startup message.
 inhibit-startup-message t

 ;; Warning minimum level
 warning-minimum-level :emergency

 ;; Change title
 frame-title-format "Free Programmers Emacs"

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
(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                sly-mrepl-mode-hook
                eshell-mode-hook
                cider-repl-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; --- Disable unnecessary UI elements ---
(progn
  ;; Do not show menu bar.
  (menu-bar-mode -1)

  ;; Do not show tool bar.
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

  ;; Do not show scroll bar.
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1)))

;; --- No littering ---
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; --- Disable mouse ---
;; (use-package disable-mouse
;;   :init (global-disable-mouse-mode))

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

;; --- license templates ---
(use-package license-templates)

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
(use-package persp-mode
  :hook (projectile-mode . persp-mode)
  :config
  (setq persp-autokill-buffer-on-remove t
        persp-nil-hidden t
        persp-auto-save-opt 0
        persp-add-buffer-on-find-file nil
        persp-switch-to-added-buffer nil
        persp-set-last-persp-for-new-frames nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-nil-name "dashboard"
        persp-auto-resume-time -1))

(use-package persp-mode-projectile-bridge
  :after (persp-mode projectile)
  :config
  (add-hook 'persp-mode-hook #'(lambda ()
                                 (persp-mode-projectile-bridge-mode 1))))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

;; --- dashboard ---
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items nil)
  (setq dashboard-center-content t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner "~/.emacs.d/banner-text.txt")
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-footer nil))

;; --- Helpful ---
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
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
  :commands (projectile-run-vterm vterm)
  :config
  (setq vterm-shell "zsh"))

;; --- Modeline ---
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-persp-name nil)
           (doom-modeline-height 24)
           (doom-modeline-buffer-file-name-style 'file-name)
           (doom-modeline-buffer-encoding nil)
           (doom-modeline-major-mode-icon nil))
  :config
  (set-face-attribute 'mode-line nil :family "Fira Code" :height 85)
  (set-face-attribute 'mode-line-inactive nil :family "Fira Code" :height 85))

;; --- Theme Magic Pywal ---
(use-package theme-magic
  :config
  (theme-magic-export-theme-mode))

;; --- themes ---
(use-package hc-zenburn-theme
  :defer t)

(use-package humanoid-themes
  :defer t)

;; --- Set theme based on time ---
(defun set-theme-based-on-time (hour-to-go-dark-mode hour-to-go-light-mode light-theme dark-theme)
  (let ((hour (nth 2 (decode-time (seconds-to-time (current-time))))))
        (if (and (<= hour hour-to-go-dark-mode) (>= hour hour-to-go-light-mode))
            (load-theme light-theme t)
          (load-theme dark-theme t))))

;; Light doing the day, dark doing the afternoon/night
(set-theme-based-on-time 17 8 'humanoid-light 'hc-zenburn)

;; --- automatically clean whitespace ---
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; --- custom functions ---
(defun fp/split-window-balanced ()
  "Split window and balance all windows after."
  (interactive)
  (split-window-right)
  (balance-windows))

(defun fp/delete-window-balanced ()
  "Delete window and balance all windows after."
  (interactive)
  (delete-window)
  (balance-windows))

(defun fp/open-init-el ()
  "Open Emacs init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun fp/flycheck-list-errors ()
  "Execute flycheck-list-errors and switch to that buffer."
  (interactive)
  (flycheck-list-errors)
  (pop-to-buffer "*Flycheck errors*"))

(defun fp/lsp-describe-thing-at-point ()
  "Execute lsp-describe-thing-at-point and switch to that buffer."
  (interactive)
  (lsp-describe-thing-at-point)
  (pop-to-buffer "*lsp-help*"))

(defun fp/projectile-switch-project ()
  "Switch to projectile project. (HACK fixes performance issues somehow??)."
  (interactive)
  (projectile-switch-project))

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
    "bB" '(counsel-projectile-switch-to-buffer :which-key "projectile-switch-buffer")
    "bf" '(counsel-switch-buffer :which-key "counsel-switch-buffer")
    "ba" '(persp-add-buffer :which-key "persp-add-buffer")
    "bd" '(bury-buffer :which-key "bury-current-buffer")
    "bk" '(persp-kill-buffer :which-key "kill-current-buffer")
    "bl" '(projectile-project-buffers-other-buffer :which-key "switch-last-buffer"))

  (fp/leader-keys
    "g" '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "magit-status"))

  (fp/leader-keys
    "s" '(:ignore t :which-key "search")
    "ss" '(swiper :which-key "swiper")
    "sg" '(counsel-projectile-rg :which-key "counsel-projectile-rg"))

  (fp/leader-keys
    "c" '(:ignore t :which-key "code")
    "ce" '(fp/flycheck-list-errors :which-key "flycheck-list-errors")
    "cc" '(comment-or-uncomment-region :which-key "comment-or-uncomment-region"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'tide-mode-map
    "ci" '(tide-goto-reference :which-key "go-to-reference")
    "cf" '(tide-organize-imports :which-key "organize-iports")
    "cr" '(tide-rename-symbol :which-key "rename")
    "cR" '(tide-restart-server :which-key "restart-tide"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'clojure-mode-map
    "cd" '(cider-doc :which-key "cider-doc")
    "cs" '(cider-jack-in :which-key "cider-jack-in")
    "cf" '(cider-format-buffer :which-key "cider-format-buffer")
    "cb" '(cider-eval-buffer :which-key "cider-eval-buffer")
    "ca" '(cider-eval-last-sexp :which-key "cider-eval-last-sexp")
    "cR" '(cider-restart :which-key "cider-restart"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'lsp-mode-map
    "co" '(lsp-organize-imports :which-key "organize-imports")
    "ct" '(lsp-goto-type-definition :which-key "goto-type-definition")
    "ci" '(lsp-find-implementation :which-key "find-implementation")
    "ce" '(lsp-ui-flycheck-list :which-key "lsp-ui-flycheck-list")
    "cf" '(lsp-find-definition :which-key "find-defintion")
    "cd" '(fp/lsp-describe-thing-at-point :which-key "describe-thing-at-point")
    "ca" '(lsp-execute-code-action :which-key "code-action")
    "cr" '(lsp-rename :which-key "rename")
    "cR" '(lsp-workspace-restart :which-key "workspace-restart"))

  (fp/leader-keys
    "w" '(:ignore t :which-key "window")
    "ww" '(other-window :which-key "other-window")
    "wk" '(evil-window-right :which-key "right-window")
    "wj" '(evil-window-left :which-key "left-window")
    "wn" '(fp/split-window-balanced :which-key "split-window-balanced")
    "ws" '(window-swap-states :which-key "window-swap-states")
    "wd" '(fp/delete-window-balanced :which-key "delete-window"))

  (fp/leader-keys
    "o" '(:ignore t :which-key "open")
    "ot" '(projectile-run-vterm :which-key "vterm")
    "oT" '(vterm :which-key "vterm")
    "oe" '(fp/open-init-el :which-key "open-init.el"))

  (fp/leader-keys
    "p" '(:ignore t :which-key "projectile")
    "pa" '(projectile-add-known-project :which-key "add-project")
    "pd" '(projectile-remove-known-project :which-key "remove-project")
    "pp" '(fp/projectile-switch-project :which-key "switch-project")
    "pi" '(projectile-invalidate-cache :which-key "invalidate-cache")
    "pb" '(projectile-switch-to-buffer :which-key "switch-buffer")
    "pk" '(persp-kill :which-key "kill-project"))

  (fp/leader-keys
    "f"  '(:ignore t :which-key "files")
    "ff" '(counsel-find-file :which-key "find-file")
    "fc" '(dired-create-empty-file :which-key "create-file")
    "fd" '(dired-create-directory :which-key "create-directory")
    "fs" '(evil-write :which-key "write")
    "fS" '(evil-write-all :which-key "write-all")
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
(use-package hydra
  :defer t)

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
  "A hook to run for buffers you want to revert automatically and silently."
  (auto-revert-mode 1)
  (setq-local auto-revert-verbose nil))

(use-package dired
  :straight nil
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (add-hook 'dired-mode-hook #'quiet-auto-revert t nil)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "f" 'dired-create-empty-file
    "F" 'dired-create-directory))

(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; --- Counsel ---
(use-package counsel)

;; --- Ivy ---
(use-package ivy
  :diminish
  :custom ((ivy-use-virtual-buffers t)
           (ivy-use-selectable-prompt t))
  :config
  (ivy-mode 1))

;; --- Magit ---
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit
  :config
  (ghub-request "GET" "/user" nil
                :forge 'github
                :host "api.github.com"
                :username "runeanielsen"
                :auth 'forge))

;; --- commmon lisp ---
(defvar inferior-lisp-program "sbcl")

(use-package sly
  :commands (sly))

;; --- Rainbow delimiters ---
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; --- Rainbow mode ---
(use-package rainbow-mode
  :hook (org-mode
         emacs-lisp-mode
         lisp-mode
         typescript-mode
         js-mode))

;; --- tree-sitter ---
(use-package tree-sitter
  :hook ((go-mode . tree-sitter-hl-mode)
         (csharp-mode . tree-sitter-hl-mode)
         (js-mode . tree-sitter-hl-mode)
         (typescript-mode . tree-sitter-hl-mode)
         (python-mode . tree-sitter-hl-mode)
         (css-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs)

;; --- Company mode ---
(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :custom ((company-idle-delay 0)
           (company-minimum-prefix-lenght 1))
  :bind (("<C-tab>" . company-complete)))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; --- which key ---
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :custom (which-key-idle-delay 1)
  :config (which-key-mode))

;; --- smartparens ---
(use-package smartparens
  :config
  (smartparens-global-mode t))

;; --- Flycheck ---
(use-package flycheck
  :hook ((lsp-mode . flycheck-mode)
         (clojure-mode . flycheck-mode)
         (emacs-lisp-mode . flycheck-mode))
  ; Hack because csharp lsp mode often bugs out
  :custom ((flycheck-checker-error-threshold 10000)))

;; --- lsp mode ---
(use-package lsp-mode
  :straight '(evil-motion-trainer :host github
                                  :repo "runeanielsen/lsp-mode"
                                  :branch "master")
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :custom ((lsp-enable-links nil)
           (lsp-log-io nil)
           (lsp-headerline-breadcrumb-enable nil))
  :commands (lsp lsp-deferred))

;; --- lsp-ivy ---
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; --- lsp-ui ---
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom ((lsp-ui-sideline-show-code-actions nil)
           (lsp-ui-doc-enable nil)))

;; --- go mode ---
(defun lsp-go-install-save-hooks ()
  "LSP Go install save hooks."
  (setq indent-tabs-mode 1)
  (setq tab-width 2)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (go-mode . lsp-go-install-save-hooks)))

;; --- clojure ---
(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :custom ((cider-repl-pop-to-buffer-on-connect 'display-only)
           (cider-repl-display-help-banner nil)))

;; --- protobuf ---
(use-package protobuf-mode
  :mode "\\.proto\\'")

;; --- json mode ---
(use-package json-mode
  :hook (json-mode . (lambda ()
                       (make-local-variable 'js-indent-level)
                       (setq js-indent-level 2))))

;; --- yaml mode ---
(use-package yaml-mode)

;; --- csharp mode ---
(defun lsp-csharp-install-save-hooks ()
  "LSP CSharp install save hooks."
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(use-package csharp-mode
  :hook ((csharp-mode . lsp-deferred)
         (csharp-mode . lsp-csharp-install-save-hooks)))

;; --- python ---
(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
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
  (add-hook 'before-save-hook 'tide-format)
  (company-mode +1))

(use-package tide
  :hook ((js-mode-hook . setup-tide-mode)
         (typescript-mode-hook . setup-tide-mode))
  :config
  (setq company-tooltip-align-annotations t)
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
  :hook (css-mode . lsp-deferred)
  :config
  (setq css-indent-offset 2))

;; --- Node modules path ---
(use-package add-node-modules-path
  :hook ((js-mode . add-node-modules-path)
         (css-mode . add-node-modules-path)
         (web-mode . add-node-modules-path)
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
  :custom
  (emmet-expand-jsx-className? t))

;; --- markdown ---
(defun fp/set-markdown-header-font-sizes ()
  "Set markdown header font sizes."
  (dolist (face '((markdown-header-face-1 . 1.1)
                  (markdown-header-face-2 . 1.05)
                  (markdown-header-face-3 . 1.0)
                  (markdown-header-face-4 . 1.0)
                  (markdown-header-face-5 . 1.0)))
    (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

(defun fp/markdown-mode-hook ()
  "Set header font sizes on setup hook."
  (fp/set-markdown-header-font-sizes))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (add-hook 'markdown-mode-hook 'fp/markdown-mode-hook))

;;; init.el ends here
