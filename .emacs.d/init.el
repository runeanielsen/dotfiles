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
(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)

;; Native compilation
(setq comp-async-report-warnings-errors nil)
(setq package-native-compile t)

;; gcmh
(use-package gcmh
  :config
  (gcmh-mode 1))

;; Increase the amount of data which Emacs reads from process to 4mb
(setq read-process-output-max (* 4096 1024))

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
 frame-title-format "Emacs"

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

;; Change fringe mode
(set-fringe-mode 1)

;; Disable recentf mode
(recentf-mode nil)

;; Display line numbers
(use-package display-line-numbers
  :straight nil
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-grow-only t)
  :config
  (global-display-line-numbers-mode)
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  shell-mode-hook
                  sly-mrepl-mode-hook
                  eshell-mode-hook
                  cider-repl-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;; --- No littering ---
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; --- Disable mouse ---
(use-package disable-mouse
  :init (global-disable-mouse-mode))

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
  (setq dashboard-set-init-info nil)
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

;; --- Ace window ---
(use-package ace-window
  :custom
  ((aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
   (aw-background nil)))

;; --- vterm ---
(use-package vterm
  :commands (projectile-run-vterm vterm)
  :config
  (setq vterm-shell "zsh"))

;; --- Modeline ---
(use-package mood-line
  :config
  (mood-line-mode))

;; --- window divider ---
(setq window-divider-default-right-width 1)
(window-divider-mode)

;; --- Theme Magic Pywal ---
(use-package theme-magic
  :config
  (theme-magic-export-theme-mode))

;; -- Load theme ---
(use-package mood-one-theme)
(load-theme 'modus-vivendi t)

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

(defun fp/split-window-balanced-horizontal ()
  "Split window horizontal and balance all windows after."
  (interactive)
  (split-window-below)
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

(defun fp/persp-kill ()
  "Persp kill and switch to dashboard."
  (interactive)
  (persp-kill nil)
  (switch-to-buffer "*dashboard*")
  (dashboard-refresh-buffer))

(defun fp/find-file-or-buffer ()
  "Find file."
  (interactive)
  (if (projectile-project-p)
      (counsel-projectile-find-file)
    (call-interactively 'ivy-switch-buffer)))

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
    "SPC" '(fp/find-file-or-buffer :which-key "find-file"))

  (fp/leader-keys
    "d" '(:ignore t :which-key "dired")
    "dd" '(dired-jump :which-key "dired-jump")
    "dp" '(projectile-dired :which-key "dired-projectile"))

  (fp/leader-keys
    "b" '(:ignore t :which-key "buffer")
    "bb" '(persp-switch-to-buffer :which-key "switch-to-buffer-persp")
    "bB" '(counsel-projectile-switch-to-buffer :which-key "projectile-switch-buffer")
    "bf" '(ivy-switch-buffer :which-key "ivy-switch-buffer")
    "ba" '(persp-add-buffer :which-key "persp-add-buffer")
    "bd" '(bury-buffer :which-key "bury-current-buffer")
    "bk" '(persp-kill-buffer :which-key "kill-current-buffer")
    "bl" '(projectile-project-buffers-other-buffer :which-key "switch-last-buffer"))

  (fp/leader-keys
    "g" '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "magit-status"))

  (fp/leader-keys
    "s" '(:ignore t :which-key "search")
    "ss" '(swiper-isearch :which-key "isearch")
    "sg" '(counsel-projectile-rg :which-key "counsel-projectile-rg"))

  (fp/leader-keys
    "c" '(:ignore t :which-key "code")
    "ce" '(fp/flycheck-list-errors :which-key "flycheck-list-errors")
    "cc" '(comment-or-uncomment-region :which-key "comment-or-uncomment-region"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'tide-mode-map
    "ci" '(tide-jump-to-implementation :which-key "go-to-reference")
    "cf" '(tide-jump-to-definition :which-key "organize-imports")
    "cr" '(tide-rename-symbol :which-key "rename")
    "cd" '(tide-documentation-at-point :which-key "documentation")
    "cR" '(tide-restart-server :which-key "restart-tide"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'clojure-mode-map
    "ct" '(cider-test-run-project-tests :which-key "cider-test-run-project-tests")
    "pi" '(cljr-add-project-dependency :which-key "cljr-add-project-dependency")
    "cd" '(cider-doc :which-key "cider-doc")
    "cs" '(cider-jack-in :which-key "cider-jack-in")
    "cf" '(cider-find-var :which-key "cider-format-buffer")
    "cF" '(cider-format-buffer :which-key "cider-format-buffer")
    "ca" '(cider-eval-last-sexp :which-key "cider-eval-last-sexp")
    "cA" '(cider-eval-buffer :which-key "cider-eval-buffer")
    "co" '(cider-load-buffer-and-switch-to-repl-buffer :which-key "cider-load-buffer-and-switch-to-repl-buffer")
    "cr" '(cljr-rename-symbol :which-key "cljr-rename-symbol")
    "cR" '(cider-restart :which-key "cider-restart")
    "cQ" '(cider-quit :which-key "cider-quit")
    "cnr" '(cider-ns-refresh :which-key "cider-ns-refresh")
    "cna" '(cider-ns-reload :which-key "cider-ns-reload")
    "cnA" '(cider-ns-reload-all :which-key "cider-ns-reload-all")
    "cnl" '(cider-load-all-project-ns :which-key "cider-load-all-project-ns")
    "cpc" '(cider-pprint-eval-last-sexp-to-comment :which-key "cider-pprint-eval-last-sexp-to-comment"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'lispyville-mode-map
    "clw" '(lispyville-wrap-round :which-key "lispyville-wrap-round"))

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
    "ww" '(ace-window :which-key "ace-window")
    "wn" '(fp/split-window-balanced :which-key "split-window-balanced")
    "wN" '(fp/split-window-balanced-horizontal :which-key "split-window-balanced-horizontal")
    "wh" '(evil-window-left :which-key "window-left")
    "wl" '(evil-window-right :which-key "window-right")
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
    "pb" '(projectile-switch-to-buffer :which-key "switch-buffer")
    "pk" '(fp/persp-kill :which-key "kill-project"))

  (fp/leader-keys
    "f"  '(:ignore t :which-key "files")
    "ff" '(counsel-find-file :which-key "find-file")
    "fc" '(dired-create-empty-file :which-key "create-file")
    "fd" '(dired-create-directory :which-key "create-directory")
    "fs" '(evil-write :which-key "write")
    "fS" '(evil-write-all :which-key "write-all")
    "ft" '(counsel-load-theme :which-key "load-theme")
    "fq" '(evil-save-and-close :which-key "save-and-close")))

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
  :init
  (evil-collection-init))

;; --- Hydra ---
(use-package hydra
  :defer t)

(defhydra hydra-text-scale ()
  "scale text"
  ("j" text-scale-decrease "out")
  ("k" text-scale-increase "in")
  ("f" nil "finished" :exit t))

(defhydra hydra-window-resize ()
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
(defun fp/quiet-auto-revert ()
  "A hook to run for buffers you want to revert automatically and silently."
  (auto-revert-mode 1)
  (setq-local auto-revert-verbose nil))

(use-package dired
  :straight nil
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (add-hook 'dired-mode-hook #'fp/quiet-auto-revert t nil)
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

;; --- Ivy posframe ---
(defun correct-color-theme-switch ()
  "Correct the fringe color."
  (set-face-attribute 'line-number nil
                      :background (face-attribute 'default :background nil t))
  (set-face-attribute 'fringe nil
                      :foreground (face-attribute 'ivy-posframe :background nil t)
                      :background (face-attribute 'ivy-posframe :background nil t)))

(defun fp/dynamic-ivy-posframe-get-size ()
  "Set the ivy-posframe size according to the current frame."
  (let ((height (or ivy-posframe-height (or ivy-height 10)))
        (width (min (or ivy-posframe-width 120) (round (* .75 (frame-width))))))
    (list :height height :width width :min-height height :min-width width)))

(setq ivy-posframe-size-function 'fp/dynamic-ivy-posframe-get-size)

(use-package ivy-posframe
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-center))
        ivy-posframe-height-alist '((t . 20)))
  (setq ivy-posframe-parameters
        '((left-fringe . 15)
          (right-fringe . 15)))
  (setq ivy-posframe-border-width 1)
  (ivy-posframe-mode 1)
  (advice-add 'counsel-load-theme :after #'posframe-delete-all)
  (advice-add 'counsel-load-theme :after #'correct-color-theme-switch)
  (correct-color-theme-switch))

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

;; --- lisp ---
(use-package lispyville
  :init
  (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook clojure-mode-hook) #'lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators c-w additional)))

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
         sly-mode
         clojure-mode
         typescript-mode
         js-mode))

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
  :hook ((go-mode . lsp)
         (go-mode . lsp-go-install-save-hooks)))

;; --- clojure ---
(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo)
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)))

(defun fp/cider-format-buffer-no-errors ()
  "Call cider-format-buffer - do not show error if cider-format fails."
  (condition-case nil
      (cider-format-buffer)
    (error nil)))

(defun cider-install-save-hooks ()
  "Cider install save hooks."
  (add-hook 'before-save-hook #'fp/cider-format-buffer-no-errors t t))

(use-package cider
  :hook ((clojure-mode . cider-mode)
         (cider-mode . cider-install-save-hooks))
  :custom ((cider-repl-pop-to-buffer-on-connect nil)
           (cider-repl-display-help-banner nil)))

 (use-package clj-refactor
   :hook (clojure-mode . clj-refactor-mode)
   :custom
   (cljr-warn-on-eval nil))

;; --- protobuf ---
(use-package protobuf-mode
  :mode "\\.proto\\'")

;; --- json mode ---
(use-package json-mode
  :hook (json-mode . (lambda ()
                       (make-local-variable 'js-indent-level)
                       (setq js-indent-level 2))))

;; --- yaml mode ---
(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

;; --- csharp mode ---
(defun lsp-csharp-install-save-hooks ()
  "LSP CSharp install save hooks."
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(use-package csharp-mode
  :hook ((csharp-mode . lsp)
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
  (company-mode +1))

(use-package tide
  :hook ((js-mode . setup-tide-mode)
         (typescript-mode . setup-tide-mode))
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
  (setq web-mode-css-indent-offset 2)
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

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package scss-mode
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
         (web-mode . prettier-js-mode)
         (css-mode . prettier-js-mode)
         (scss-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)))

;; --- markdown ---
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; --- org-mode ---
(use-package org
  :hook ((org-mode . org-indent-mode))
  :custom (org-startup-truncated nil)
  :config
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-preserve-indentation t))

(defun fp/org-mode-visual-fill ()
  "Set org mode visual fill settings."
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode)
  (visual-line-mode))

(use-package visual-fill-column
  :hook (org-mode . fp/org-mode-visual-fill))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "○" "●" "○" "●" "○" "●")))

(use-package org-tempo
  :straight nil
  :after org)

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/roam-notes")
  (org-roam-complete-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (org-roam-setup)
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

;;; init.el ends here
