;;; package --- Summary: init.el file.

;;; Commentary:
;; Personal Emacs setup.

;;; Code:
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
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

;; Native compilation
(defvar comp-async-report-warnings-errors)
(setq comp-async-report-warnings-errors nil
      package-native-compile t)
;; gcmh
(use-package gcmh
  :config
  (gcmh-mode 1))

;; Font
(set-face-attribute 'default nil :font "Jetbrains Mono" :height 130)

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

;; Do not display column number in mode-line.
(column-number-mode nil)

;; Automatically update buffers if file content on the disk has changed.
(global-auto-revert-mode t)

;; No blinking cursor
(blink-cursor-mode 0)

;; Show parens pairs
(show-paren-mode 1)

;; Change fringe mode
(fringe-mode '(8 . 8))

;; Disable recentf mode
(recentf-mode 0)

;; Disable backup
(setq backup-inhibited t)

; Disable auto save
(setq auto-save-default nil)

;; Disable minibuffer auto-raise
(setq minibuffer-auto-raise nil)

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

;; --- mode-line ---
(setq-default mode-line-format "%e %b (%l:%c)")

;; --- no littering ---
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; --- disable mouse ---
(use-package disable-mouse
  :config (global-disable-mouse-mode))

;; --- shut up ---
(use-package shut-up
  :config
  (when noninteractive
    (shut-up-silence-emacs)))

;; --- license templates ---
(use-package license-templates)

;; --- projectile ---
(use-package projectile
  :custom ((projectile-completion-system 'ivy)
           (projectile-enable-caching nil)
           (projectile-indexing-method 'alien)
           (projectile-track-known-projects-automatically nil))
  :config (projectile-mode t))

;; --- persp-mode ---
(use-package persp-mode
  :after projectile
  :init (persp-mode)
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
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
  :after persp-mode
  :init (persp-mode-projectile-bridge-mode))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

;; --- dashboard ---
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items nil
        dashboard-center-content t
        initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-show-shortcuts nil
        dashboard-set-init-info nil
        dashboard-set-footer nil))

;; --- helpful ---
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom ((counsel-describe-function-function #'helpful-callable)
           (counsel-describe-variable-function #'helpful-variable))
  :bind (([remap describe-function] . counsel-describe-function)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-key] . helpful-key)))

;; --- all-the-icons ---
(use-package all-the-icons)

;; --- ace window ---
(use-package ace-window
  :custom
  ((aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
   (aw-background nil)))

;; --- vterm ---
(use-package vterm
  :commands (projectile-run-vterm vterm))

;; --- theme Magic Pywal ---
(use-package theme-magic
  :config
  (theme-magic-export-theme-mode))

;; -- load theme ---
(use-package f)

(defvar fp/remember-last-theme-dir (concat "/home/notation/.emacs.d/" "var/remember-last-theme"))
(defvar fp/remember-last-theme-file (concat fp/remember-last-theme-dir "/remember-last"))
(defvar fp/remember-last-theme-default 'wombat)

(defun fp/get-last-theme ()
  "Gets the last set theme."
  (intern (f-read-text fp/remember-last-theme-file 'utf-8)))

(defun fp/set-last-theme ()
  "Set the last theme to the current theme."
  (f-write-text (symbol-name (car custom-enabled-themes)) 'utf-8 fp/remember-last-theme-file))

(defun fp/set-default-theme ()
  "Set the default theme when none is saved."
  (f-write-text (symbol-name fp/remember-last-theme-default) 'utf-8 fp/remember-last-theme-file))

(defun fp/create-remember-last-theme-file ()
  "Create directory and file if not exists."
  (unless (f-exists? fp/remember-last-theme-dir)
    (f-mkdir fp/remember-last-theme-dir))
  (unless (f-exists? fp/remember-last-theme-file)
      (f-touch fp/remember-last-theme-file)
      (fp/set-default-theme)))

(defun fp/remember-last-theme ()
  "Remembers last set theme."
  (fp/create-remember-last-theme-file)
  (load-theme (fp/get-last-theme) t)
  (advice-add 'counsel-load-theme :after #'fp/set-last-theme))

(fp/remember-last-theme)

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
  (call-interactively 'persp-kill)
  (when (equal (projectile-project-p) nil)
    (switch-to-buffer "*dashboard*")
    (dashboard-refresh-buffer)))

(defun fp/find-file-or-buffer ()
  "Find file or buffer depending on if project is active."
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'counsel-projectile-find-file)
    (call-interactively 'switch-to-buffer)))

(defun fp/cleanup ()
  "Kill all buffers, remove other windows and go to dashboard."
  (interactive)
  (mapcar 'kill-buffer (remove (get-buffer "*dashboard*") (buffer-list)))
  (delete-other-windows)
  (switch-to-buffer "*dashboard*"))

(defun fp/line-number-at-pos ()
  "Displays the current line number at pos."
  (interactive)
  (message (number-to-string (line-number-at-pos))))

(defun fp/pick-vterm ()
  "Decides how to run projectile."
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-run-vterm)
    (call-interactively 'vterm)))

;; --- general ---
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
    "bB" '(switch-to-buffer :which-key "switch-to-buffer")
    "bf" '(ivy-switch-buffer :which-key "ivy-switch-buffer")
    "ba" '(persp-add-buffer :which-key "persp-add-buffer")
    "bd" '(bury-buffer :which-key "bury-buffer")
    "bk" '(persp-kill-buffer :which-key "kill-current-buffer")
    "bK" '(fp/cleanup :which-key "cleanup")
    "bl" '(projectile-project-buffers-other-buffer :which-key "switch-last-buffer"))

  (fp/leader-keys
    "g" '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "magit-status"))

  (fp/leader-keys
    "s" '(:ignore t :which-key "search")
    "ss" '(counsel-grep-or-swiper :which-key "swiper-isearch")
    "sf" '(evil-avy-goto-char-timer :which-key "goto-char-timer")
    "sg" '(counsel-projectile-rg :which-key "counsel-projectile-rg"))

  (fp/leader-keys
    "c" '(:ignore t :which-key "code")
    "ce" '(fp/flycheck-list-errors :which-key "flycheck-list-errors")
    "cc" '(comment-or-uncomment-region :which-key "comment-or-uncomment-region"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'prog-mode-map
    "fi" '(indent-region :which-key "indent-region"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'emacs-lisp-mode-map
    "cd" '(describe-function :which-key "describe-function"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'lsp-mode-map
    "co" '(lsp-organize-imports :which-key "organize-imports")
    "ct" '(lsp-goto-type-definition :which-key "goto-type-definition")
    "ci" '(lsp-find-implementation :which-key "find-implementation")
    "ce" '(lsp-ui-flycheck-list :which-key "lsp-ui-flycheck-list")
    "cf" '(lsp-find-definition :which-key "find-definition")
    "cF" '(lsp-format-buffer :which-key "format-buffer")
    "cd" '(fp/lsp-describe-thing-at-point :which-key "describe-thing-at-point")
    "ca" '(lsp-execute-code-action :which-key "code-action")
    "cr" '(lsp-rename :which-key "rename")
    "cR" '(lsp-workspace-restart :which-key "workspace-restart"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps '(lsp-mode-map csharp-mode-map)
    "cqq" '(lsp-csharp-run-test-at-point :which-key "lsp-csharp-run-test-at-point"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'clojure-mode-map
    "ct" '(:ignore t :which-key "Clojure tests")
    "ctt" '(cider-test-run-test :which-key "cider-test-run-test")
    "ctn" '(cider-test-run-ns-tests :which-key "cider-tst-run-ns-tests")
    "cta" '(cider-test-run-project-tests :which-key "cider-test-run-project-tests")
    "pi" '(cljr-add-project-dependency :which-key "cljr-add-project-dependency")
    "cd" '(cider-doc :which-key "cider-doc")
    "cs" '(fp/cider-jack-in :which-key "cider-jack-in")
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
    "cp" '(cider-pprint-eval-last-sexp-to-comment :which-key "cider-pprint-eval-last-sexp-to-comment"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'lisp-mode-map
    "co" '(sly-mrepl :which-key "move-to-repl")
    "cp" '(sly-eval-print-last-expression :which-key "eval-print-last-expression")
    "ca" '(sly-eval-last-expression :which-key "eval-last-expression")
    "cA" '(sly-eval-buffer :which-key "eval-buffer")
    "cR" '(sly-restart-inferior-lisp :which-key "restart-inferior-lisp")
    "cd" '(sly-documentation-lookup :which-key "documentation-lookup")
    "cD" '(sly-documentation :which-key "documentation")
    "cs" '(sly :which-key "sly"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'geiser-mode-map
    "cm" '(geiser-doc-goto-manual :which-key "geiser-doc-goto-manual")
    "cR" '(geiser-reload :which-key "geiser-load")
    "cs" '(run-geiser :which-key "run-geiser")
    "cd" '(geiser-doc-symbol-at-point :which-key "doc-symbol-at-point")
    "cD" '(geiser-doc-goto-manual :which-key "doc-goto-manual")
    "ca" '(geiser-eval-last-sexp :which-key "eval-expression")
    "cA" '(geiser-eval-buffer :which-key "eval-buffer"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'org-mode-map
    "ca" '(flyspell-correct-wrapper :which-key "flyspell-correct-wrapper"))

  (fp/leader-keys
    :states '(normal visual)
    :keymaps 'dictionary-mode-map
    "cs" '(dictionary-search :which-key "dictionary-search"))

  (fp/leader-keys
    "w" '(:ignore t :which-key "window")
    "ww" '(ace-window :which-key "ace-window")
    "wn" '(fp/split-window-balanced :which-key "split-window-balanced")
    "wN" '(fp/split-window-balanced-horizontal :which-key "split-window-balanced-horizontal")
    "wh" '(evil-window-left :which-key "window-left")
    "wl" '(evil-window-right :which-key "window-right")
    "wj" '(evil-window-down :which-key "window-down")
    "wk" '(evil-window-up :which-key "window-down")
    "ws" '(window-swap-states :which-key "window-swap-states")
    "wd" '(fp/delete-window-balanced :which-key "delete-window"))

  (fp/leader-keys
    "o" '(:ignore t :which-key "open")
    "ot" '(fp/pick-vterm :which-key "vterm")
    "oT" '(vterm :which-key "vterm")
    "oe" '(fp/open-init-el :which-key "open-init.el"))

  (fp/leader-keys
    "t" '(:ignore t :which-key "text")
    "ts" '(hydra-text-scale/body :which-key "scale text")
    "tls" '(sort-lines :which-key "sort-lines"))

  (fp/leader-keys
    "p" '(:ignore t :which-key "projectile")
    "pa" '(projectile-add-known-project :which-key "add-project")
    "pd" '(projectile-remove-known-project :which-key "remove-project")
    "pp" '(fp/projectile-switch-project :which-key "switch-project")
    "pb" '(projectile-switch-to-buffer :which-key "switch-buffer")
    "pk" '(fp/persp-kill :which-key "kill-project"))

  (fp/leader-keys
    "wr" '(hydra-window-resize/body :which-key "resize window"))

  (fp/leader-keys
    "f"  '(:ignore t :which-key "files")
    "ff" '(counsel-find-file :which-key "find-file")
    "fc" '(dired-create-empty-file :which-key "create-file")
    "fd" '(dired-create-directory :which-key "create-directory")
    "fs" '(evil-write :which-key "write")
    "fS" '(evil-write-all :which-key "write-all")
    "ft" '(counsel-load-theme :which-key "load-theme")
    "fl" '(fp/line-number-at-pos :which-key "line number at position")
    "fq" '(evil-save-and-close :which-key "save-and-close")))

;; --- Evil mode ---
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; --- hydra ---
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

;; --- dired ---
(use-package dired
  :straight nil
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (dired-auto-revert-buffer t))
  :hook (dired-mode-hook . fp/quiet-auto-revert)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "f" 'dired-create-empty-file
    "F" 'dired-create-directory))

(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; --- counsel ---
(use-package counsel)

;; --- ivy ---
(use-package ivy
  :custom ((ivy-use-selectable-prompt t))
  :config
  (ivy-mode 1))

;; --- ivy posframe ---
(defun correct-color-theme-switch ()
  "Correct theme color on switch."
  (set-face-foreground 'window-divider (face-attribute 'mode-line :background nil t))
  (set-face-foreground 'vertical-border (face-attribute 'mode-line :background nil t))
  (set-face-attribute 'ivy-posframe nil
                      :background (face-attribute 'default :background nil t))
  (set-face-attribute 'fringe nil
                      :foreground (face-attribute 'ivy-posframe :background nil t)
                      :background (face-attribute 'ivy-posframe :background nil t)))

(defun fp/dynamic-ivy-posframe-get-size ()
  "Set the ivy-posframe size according to the current frame."
  (let ((height (or ivy-posframe-height (or ivy-height 10)))
        (width (min (or ivy-posframe-width 120) (round (* .75 (frame-width))))))
    (list :height height :width width :min-height height :min-width width)))

(use-package ivy-posframe
  :after ivy
  :config
  (setq ivy-posframe-size-function 'fp/dynamic-ivy-posframe-get-size)
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-center))
        ivy-posframe-height-alist '((t . 20)))
  (setq ivy-posframe-parameters
        '((left-fringe . 16)
          (right-fringe . 16)))
  (setq ivy-posframe-border-width 1)
  (ivy-posframe-mode 1)
  (advice-add 'counsel-load-theme :after #'posframe-delete-all)
  (advice-add 'counsel-load-theme :after #'correct-color-theme-switch)
  (correct-color-theme-switch))

;; --- magit ---
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit
  :init
  (setq forge-add-default-bindings nil)
  (ghub-request "GET" "/user" nil
                :forge 'github
                :host "api.github.com"
                :username "runeanielsen"
                :auth 'forge))

;; --- evil-cleverparens ---
(use-package evil-cleverparens
  :hook ((emacs-lisp-mode . evil-cleverparens-mode)))

;; --- commmon lisp ---
(defvar inferior-lisp-program "sbcl")

(use-package sly
  :commands (sly)
  :config (setq common-lisp-hyperspec-root "/usr/share/doc/clhs/HyperSpec/"))

;; --- rainbow delimiters ---
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; --- rainbow ---
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; --- company ---
(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :custom ((company-idle-delay 0.1)
           (company-minimum-prefix-length 1))
  :bind (("<C-tab>" . company-complete)))

;; --- which key ---
(use-package which-key
  :defer 0
  :custom (which-key-idle-delay 1)
  :config (which-key-mode))

;; --- smartparens ---
(use-package smartparens
  :config
  (let ((modes '(emacs-lisp-mode
                 scheme-mode
                 clojure-mode
                 lisp-mode)))
    (sp-local-pair modes "'" nil :actions nil)
    (sp-local-pair modes "`" nil :actions nil))
  (smartparens-global-mode t))

;; --- flycheck ---
(use-package flycheck
  :hook ((lsp-mode . flycheck-mode)
         (clojure-mode . flycheck-mode)
         (c-mode . flycheck-mode)
         (emacs-lisp-mode . flycheck-mode)
         (lisp-mode . flycheck-mode)
         (markdown-mode . flycheck-mode)
         (haskell-mode . flycheck-mode))
  :custom ((flycheck-indication-mode nil)
           (flycheck-checker-error-threshold 10000) ;; Hack Csharp mode bugs out.
           (flycheck-check-syntax-automatically '(mode-enabled save idle-buffer-switch))
           (flycheck-buffer-switch-check-intermediate-buffers t)))

;; --- lsp ---
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . yas-minor-mode))
  :custom ((lsp-enable-links nil)
           (lsp-log-io nil)
           (lsp-enable-snippet nil)
           (lsp-eldoc-enable-hover t)
           (lsp-lens-enable nil)
           (lsp-enable-folding nil)
           (lsp-keep-workspace-alive nil)
           (lsp-headerline-breadcrumb-enable nil))
  :commands (lsp lsp-deferred))

;; --- lsp-ui ---
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom ((lsp-ui-sideline-show-code-actions nil)
           (lsp-ui-doc-enable nil)))

;; --- tree-sitter ---
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

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

(defun fp/cider-jack-in ()
  "Call either cider-jack-clj in or cider-jack-in-cljs depending on current buffer file extension."
  (interactive)
  (let ((extension (file-name-extension buffer-file-name)))
    (cond ((string= extension "clj") (call-interactively 'cider-jack-in-clj))
          ((string= extension "cljs") (call-interactively 'cider-jack-in-cljs))
          (t (message (format "Extension: '%s' is not valid." extension))))))

(defun cider-install-save-hooks ()
  "Cider install save hooks."
  (add-hook 'before-save-hook #'fp/cider-format-buffer-no-errors t t))

(use-package cider
  :hook ((clojure-mode . cider-mode)
         (cider-mode . cider-install-save-hooks))
  :custom ((tab-width 2)
           (cider-repl-pop-to-buffer-on-connect nil)
           (cider-repl-display-help-banner nil)
           (cider-use-fringe-indicators nil)
           (safe-local-variable-values '((cider-clojure-cli-aliases . "test")))))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :custom
  (cljr-warn-on-eval nil))

;; --- scheme mode ---
(use-package scheme-mode
  :straight nil
  :hook (scheme-mode . prettify-symbols-mode))

(use-package geiser-guile
  :commands (geiser))

;; --- haskell mode ---
(use-package haskell-mode
  :hook ((haskell-mode . lsp-deferred)
         (haskell-literate-mode . lsp-deferred)))

(use-package lsp-haskell
  :custom (lsp-haskell-server-path "/usr/bin/haskell-language-server"))

;; --- json mode ---
(defun fp/setup-json-mode ()
  "Setup json mode."
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

(use-package json-mode
  :mode ("\\.json\\'")
  :hook (json-mode . fp/setup-json-mode))

;; --- csv mode ---
(use-package csv-mode
  :mode ("\\.csv\\'"))

;; --- yaml mode ---
(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

;; --- csharp mode ---
(defun fp/process-sort-usings (usings)
  "Sort CSharp USINGS."
  (->> usings
       (-map (lambda (x) (replace-regexp-in-string "using \\|;\\|\\." "" x)))
       (-zip-lists usings)
       (seq-sort-by (lambda (x) (-> x cdr car)) #'string<)
       (-map #'car)))

(defun fp/sort-usings-csharp ()
  "Sort using statements i C#."
  (interactive)
  (let* ((buffer-string (buffer-substring-no-properties (point-min) (point-max)))
         (matches (flatten-list (s-match-strings-all "using [A-Za-z\.]+;" buffer-string)))
         (usings (fp/process-sort-usings matches)))
    (unless (equal matches usings)
      (save-excursion
        (goto-char 1)
        (let ((x 0))
          (while (search-forward-regexp "using [A-Za-z\.]+;\n" nil t)
            (replace-match (concat (nth x usings) "\n"))
            (setq x (+ 1 x))))))))

(defun lsp-csharp-install-save-hooks ()
  "LSP CSharp install save hooks."
  (add-hook 'before-save-hook #'fp/sort-usings-csharp)
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(use-package csharp-mode
  :hook ((csharp-mode . lsp-deferred)
         (csharp-mode . lsp-csharp-install-save-hooks)))

;; --- rust mode ---
(use-package rust-mode
  :hook ((rust-mode . lsp-deferred))
  :custom (rust-format-on-save t))

;; --- c ---
(use-package cc-mode
  :straight nil
  :config (c-set-offset 'case-label '+))

;; --- zig ---
(defun lsp-zig-install-save-hooks ()
  "LSP Zig install save hooks."
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(use-package zig-mode
  :hook ((zig-mode . lsp-deferred)
         (zig-mode . lsp-zig-install-save-hooks))
  :custom (zig-format-on-save nil))

;; --- typescript mode ---
(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred)
  :custom (typescript-indent-level 2))

;; --- web mode ---
(defun fp/web-mode-setup ()
  "Web-mode setup."
  (let ((file-extension (file-name-extension buffer-file-name)))
    (when (or (string-equal "tsx" file-extension)
              (string-equal "jsx" file-extension))
      (lsp-deferred))))

(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :hook (web-mode . fp/web-mode-setup)
  :custom ((web-mode-enable-auto-quoting nil)
           (web-mode-markup-indent-offset 2)
           (web-mode-css-indent-offset 2)
           (web-mode-code-indent-offset 2)))

;; --- css mode ---
(use-package css-mode
  :custom (css-indent-offset 2))

;; --- scss mode ---
(use-package scss-mode
  :custom (css-indent-offset 2))

;; --- node modules path ---
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
        org-src-preserve-indentation t
        org-hide-emphasis-markers t))

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

;; Flyspell
(use-package flyspell
  :hook ((org-mode . flyspell-mode)
         (markdown-mode . flyspell-mode)))

(use-package flyspell-correct
  :commands (flycheck-correct-wrapper))

(use-package dictionary
  :straight nil
  :custom (dictionary-server "dict.org"))

;; init.el ends here
