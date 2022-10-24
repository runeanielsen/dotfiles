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
(set-face-attribute 'default nil :font "Jetbrains Mono" :height 120)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Do not show menu bar.
(menu-bar-mode -1)

;; Do not show tool bar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Do not show scroll bar.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

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
(fringe-mode '(0 . 0))

;; Disable recentf mode
(recentf-mode 0)

;; Disable backup
(setq backup-inhibited t)

; Disable auto save
(setq auto-save-default nil)

;; Disable minibuffer auto-raise
(setq minibuffer-auto-raise nil)

;; --- mode-line ---
(setq-default mode-line-format "%e %b (%l:%c)")

;; -- load theme ---
(use-package f)

(defvar fp/remember-last-theme-dir (concat "/home/notation/.emacs.d/" "var/remember-last-theme"))
(defvar fp/remember-last-theme-file (concat fp/remember-last-theme-dir "/remember-last"))
(defvar fp/remember-last-theme-default 'modus-operandi)

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
(use-package license-templates
  :commands (license-templates-new-file))

;; --- project ---
(use-package project
  :straight nil
  :custom (project-switch-commands '((project-find-file "Find file"))))

 ;; --- dashboard ---
(use-package dashboard
  :custom ((dashboard-items nil)
           (dashboard-center-content t)
           (dashboard-show-shortcuts nil)
           (dashboard-set-init-info nil)
           (dashboard-set-footer nil)
           (initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))
  :config
  (dashboard-setup-startup-hook))

;; --- all-the-icons ---
(use-package all-the-icons)

;; --- vterm ---
(use-package vterm
  :commands (vterm))

;; --- theme Magic Pywal ---
(use-package theme-magic
  :config
  (theme-magic-export-theme-mode))

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

(defun fp/flymake-list-errors ()
  "Execute flymake and switch to that buffer."
  (interactive)
  (if (project-current)
      (progn
        (flymake-show-project-diagnostics)
        (other-window 1))
    (progn
      (flymake-show-diagnostics-buffer)
      (other-window 1))))

(defun fp/eldoc-doc-buffer-pop-to ()
  "Execute 'eldoc-doc-buffer' and switch to that buffer."
  (interactive)
  (call-interactively 'eldoc-doc-buffer)
  (pop-to-buffer "*eldoc*"))

(defun fp/kill-all-buffers ()
  "Kill all buffers, remove other windows and go to dashboard buffer."
  (interactive)
  (let ((goto-buffer-name "*dashboard*"))
    (mapcar 'kill-buffer (remove (get-buffer goto-buffer-name) (buffer-list)))
    (delete-other-windows)
    (switch-to-buffer goto-buffer-name)))

(defun fp/switch-to-buffer ()
  "Switch buffer depending on being in project or not."
  (interactive)
  (if (project-current)
      (call-interactively 'project-switch-to-buffer)
    (call-interactively 'switch-to-buffer)))

(defun fp/find-file ()
  "Find file depending on being in project or not."
  (interactive)
  (if (project-current)
      (call-interactively 'project-find-file)
    (call-interactively 'find-file)))

(defun fp/project-name-prefix (name)
  "Get project name prefix using NAME."
  (when (project-current)
    (let ((project-name (car (cdr (reverse (split-string (cdr (project-current)) "/"))))))
      (concat "*" project-name "-" name "*"))))

(defun fp/project-vterm ()
  "Vterm based on project name."
  (interactive)
  (if (project-current)
      (let* ((default-project-vterm-name (fp/project-name-prefix "vterm"))
             (vterm-buffer (get-buffer default-project-vterm-name)))
        (if vterm-buffer
            (switch-to-buffer vterm-buffer)
          (let ((default-directory (cdr (project-current))))
            (vterm (generate-new-buffer-name default-project-vterm-name)))))
    (vterm)))

;; --- general ---
(use-package general
  :config
  (general-create-definer fp/default-normal-visual
    :keymaps '(normal visual))

  (fp/default-normal-visual
    ";" '(counsel-M-x :which-key "execute-extended-command")
    "," '(fp/switch-to-buffer :which-key "switch-buffer"))

  (general-create-definer fp/leader-keys
    :keymaps '(normal visual)
    :prefix "SPC")

  (fp/leader-keys
    "TAB" '(:ignore t :which-key "project")
    "TAB TAB" '(project-switch-project :which-key "switch-project")
    "TAB n" '(project-remember-project-under :which-key "remember-project"))

  (fp/leader-keys
    "SPC" '(fp/find-file :which-key "find-file"))

  (fp/leader-keys
    "d" '(:ignore t :which-key "dired")
    "dd" '(dired-jump :which-key "dired-jump")
    "dj" '(counsel-dired-jump :which-key "dired-jump")
    "dp" '(project-dired :which-key "project-dired"))

  (fp/leader-keys
    "j" '(:ignore t :which-key "buffer")
    "ji" '(ibuffer :which-key "ibuffer")
    "jj" '(switch-to-buffer :which-key "switch-to-buffer")
    "jz" '(bury-buffer :which-key "bury-buffer")
    "jm" '(bookmark-set :which-key "bookmark-set")
    "jM" '(bookmark-delete :which-key "bookmark-set")
    "jk" '(kill-buffer :which-key "kill-current-buffer")
    "jK" '(fp/kill-all-buffers :which-key "kill-all-buffers")
    "jl" '(evil-switch-to-windows-last-buffer :which-key "switch-to-last-buffer"))

  (fp/leader-keys
    "g" '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "magit-status"))

  (fp/leader-keys
    "r" '(:ignore t :which-key "git")
    "rr" '(replace-regexp :which-key "replace-regexp"))

  (fp/leader-keys
    "s" '(:ignore t :which-key "search")
    "sj" '(evil-show-jumps :which-key "evil-show-jumps")
    "sm" '(bookmark-jump :which-key "bookmark-jump")
    "si" '(counsel-imenu :which-key "imenu")
    "ss" '(counsel-grep-or-swiper :which-key "grep-or-swiper")
    "sw" '(avy-goto-word-0 :which-key "goto-char-timer")
    "sf" '(avy-goto-char-timer :which-key "goto-char-timer")
    "sl" '(avy-goto-line :which-key "goto-char-timer")
    "sg" '(counsel-rg :which-key "rg"))

  (fp/leader-keys
    "c" '(:ignore t :which-key "code")
    "ce" '(flymake-goto-next-error :which-key "flymake-goto-next-error")
    "cE" '(fp/flymake-list-errors :which-key "flymake-list-errors")
    "cf" '(xref-find-definitions :which-key "find-definition")
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
    :keymaps 'eglot-mode-map
    "ct" '(eglot-find-typeDefinition :which-key "goto-type-definition")
    "ci" '(eglot-find-implementation :which-key "find-implementation")
    "cF" '(eglot-format-buffer :which-key "format-buffer")
    "cd" '(fp/eldoc-doc-buffer-pop-to :which-key "fp/eldoc-doc-buffer-pop-to")
    "ca" '(eglot-code-actions :which-key "code-action")
    "cr" '(eglot-rename :which-key "rename")
    "cR" '(eglot-reconnect :which-key "reconnect"))

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
    "cS" '(cider-connect-clj :which-key "cider-connect-clj")
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
    "ww" '(ace-window :which-key "other-window")
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
    "ot" '(fp/project-vterm :which-key "vterm")
    "oT" '(vterm :which-key "vterm")
    "oe" '(fp/open-init-el :which-key "open-init.el"))

  (fp/leader-keys
    "t" '(:ignore t :which-key "text")
    "ts" '(hydra-text-scale/body :which-key "scale text")
    "tls" '(sort-lines :which-key "sort-lines"))

  (fp/leader-keys
    "p" '(:ignore t :which-key "project")
    "pd" '(project-forget-project :which-key "remove-project")
    "pa" '(project-remember-project :which-key "remember-project")
    "pA" '(project-remember-projects-under :which-key "remember-projects-under")
    "pk" '(project-kill-buffers :which-key "kill-project"))

  (fp/leader-keys
    "wr" '(hydra-window-resize/body :which-key "resize window"))

  (fp/leader-keys
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find-file")
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
  "Scale text."
  ("j" text-scale-decrease "out")
  ("k" text-scale-increase "in")
  ("f" nil "finished" :exit t))

(defhydra hydra-window-resize ()
  "Window-resize."
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

;; --- avy ---
(use-package avy)

;; --- ivy ---
(use-package ivy
  :custom ((ivy-use-selectable-prompt t))
  :config
  (ivy-mode 1))

;; --- ivy posframe ---
(defun correct-color-theme-switch ()
  "Correct theme color on switch."
  (fringe-mode '(16 . 16))
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
  (setq ivy-posframe-border-width 1)
  (setq ivy-posframe-parameters
        '((left-fringe . 16)
          (right-fringe . 16)))
  (ivy-posframe-mode 1)
  (advice-add 'counsel-load-theme :after #'posframe-delete-all)
  (advice-add 'counsel-load-theme :after #'correct-color-theme-switch)
  (correct-color-theme-switch))

;; --- magit ---
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; --- forge ---
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
  :hook ((emacs-lisp-mode . evil-cleverparens-mode)
         (clojure-mode . evil-cleverparens-mode)
         (scheme-mode . evil-cleverparens-mode)
         (common-lisp-mode . evil-cleverparens-mode)))

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
  :hook (prog-mode . company-mode)
  :custom ((company-idle-delay 0)
           (company-minimum-prefix-length 1))
  :bind (("<C-tab>" . company-complete)))

;; --- ace window ---
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background t)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 1.0))))))

;; --- which key ---
(use-package which-key
  :custom (which-key-idle-delay 1)
  :config (which-key-mode))

;; --- smartparens ---
(use-package smartparens
  :config
  (let ((modes '(emacs-lisp-mode
                 scheme-mode
                 clojure-mode
                 lisp-mode
                 rust-mode)))
    (sp-local-pair modes "'" nil :actions nil)
    (sp-local-pair modes "`" nil :actions nil))
  (smartparens-global-mode t))

;; --- flymake ---
(use-package flymake
  :straight nil
  :hook ((prog-mode . flymake-mode))
  :custom ((flymake-fringe-indicator-position nil)
           (flymake-start-on-save-buffer t)))

;; --- eldoc ----
(use-package eldoc
  :straight nil
  :config
  (add-hook 'eglot-managed-mode-hook
            #'(lambda ()
                (setq-local eldoc-idle-delay 0)
                (setq-local eldoc-echo-area-use-multiline-p 1)
                (setq-local eldoc-documentation-strategy
                            #'eldoc-documentation-compose))))

;; --- eglot ---
(use-package eglot
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (add-to-list 'eglot-server-programs
	             '(web-mode . ("typescript-language-server" "--stdio"))))

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
  (add-hook 'before-save-hook #'eglot-format-buffer))

(use-package go-mode
  :hook ((go-mode . eglot-ensure)
         (go-mode . lsp-go-install-save-hooks)))

;; --- clojure ---
(use-package clojure-mode
  :config
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
  "Call 'cider-format-buffer' - do not show error if cider-format fails."
  (condition-case nil
      (cider-format-buffer)
    (error nil)))

(defun fp/cider-jack-in ()
  "Call 'cider-jack-in' depending on current buffer file extension."
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

;; --- scheme mode ---
(use-package scheme-mode
  :straight nil
  :hook (scheme-mode . prettify-symbols-mode))

(use-package geiser-guile
  :commands (geiser))

;; --- haskell mode ---
(use-package haskell-mode)

;;; --- protobuf mode ---
(use-package protobuf-mode
  :mode ("\\.proto\\'"))

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
  (add-hook 'before-save-hook #'eglot-format-buffer))

(use-package csharp-mode
  :hook ((csharp-mode . eglot-ensure)
         (csharp-mode . lsp-csharp-install-save-hooks)))

;; --- rust mode ---
(use-package rust-mode
  :hook ((rust-mode . eglot-ensure))
  :custom ((rust-format-on-save nil)
           (lsp-rust-analyzer-cargo-watch-command "clippy")))

;; --- c ---
(use-package cc-mode
  :straight nil
  :hook ((c-mode . eglot-ensure))
  :config (c-set-offset 'case-label '+))

;; --- zig ---
(defun lsp-zig-install-save-hooks ()
  "LSP Zig install save hooks."
  (add-hook 'before-save-hook #'eglot-format-buffer))

(use-package zig-mode
  :hook ((zig-mode . eglot-ensure)
         (zig-mode . lsp-zig-install-save-hooks))
  :custom (zig-format-on-save nil))

;; --- typescript mode ---
(use-package typescript-mode
  :hook (typescript-mode . eglot-ensure)
  :custom (typescript-indent-level 2))

;; --- web mode ---
(defun fp/web-mode-setup ()
  "Web-mode setup."
  (let ((file-extension (file-name-extension buffer-file-name)))
    (when (or (string-equal "tsx" file-extension)
              (string-equal "jsx" file-extension))
      (eglot-ensure))))

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
  :mode ("\\.css\\'")
  :custom (css-indent-offset 2))

;; --- scss mode ---
(use-package scss-mode
  :mode ("\\.scss\\'")
  :custom (css-indent-offset 2))

;; --- Prettier ---
(use-package prettier-js
  :hook ((js-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)
         (css-mode . prettier-js-mode)
         (scss-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)))

;; --- markdown ---
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; --- org-mode ---
(use-package org
  :custom (org-startup-truncated nil)
  :config (setq org-edit-src-content-indentation 0
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

;;; init.el ends here
