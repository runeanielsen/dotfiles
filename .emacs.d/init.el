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
(set-face-attribute 'default nil :font "Iosevka-14")

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

;; --- Set defaults ---
(setq-default
 ;; Don't use the compiled code if its the older package.
 load-prefer-newer t

 ;; Do not show the startup message.
 inhibit-startup-message t

 ;; Avoid resize windows when using `pop-to-buffer`
 even-window-heights nil

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
 enable-recursive-minibuffers t

 ;; Disable backups
 backup-inhibited t

 ;; Disable titlebar
 default-frame-alist '((undecorated . t))

 ;; Mode-line format
 mode-line-format '("%e" mode-line-front-space "%b (%l:%c)" mode-line-end-spaces))

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
(fringe-mode '(16 . 16))

;; Disable recentf mode
(recentf-mode 0)

;; -- load theme ---
(use-package f)

(use-package modus-themes)

(defvar fp/remember-last-theme-dir
  (concat (file-truename user-emacs-directory) "var/remember-last-theme"))

(defvar fp/remember-last-theme-file
  (concat fp/remember-last-theme-dir "/last-theme"))

(defvar fp/remember-last-theme-default 'modus-operandi)

(defun fp/get-last-theme ()
  "Gets the last set theme."
  (intern (f-read-text fp/remember-last-theme-file 'utf-8)))

(defun fp/set-last-theme ()
  "Set the last theme to the current theme."
  (f-write-text (symbol-name (car custom-enabled-themes))
                'utf-8 fp/remember-last-theme-file))

(defun fp/set-default-theme ()
  "Set the default theme when none is saved."
  (f-write-text (symbol-name fp/remember-last-theme-default)
                'utf-8 fp/remember-last-theme-file))

(defun fp/create-remember-last-theme-file ()
  "Create directory and file if not exists."
  (unless (f-exists? fp/remember-last-theme-dir)
    (f-mkdir fp/remember-last-theme-dir))
  (unless (f-exists? fp/remember-last-theme-file)
    (f-touch fp/remember-last-theme-file)))

(defun fp/remember-last-theme ()
  "Remembers last set theme."
  (fp/create-remember-last-theme-file)
  (load-theme (let ((theme (fp/get-last-theme)))
                (if theme theme fp/remember-last-theme-default))
              t)
  (advice-add 'counsel-load-theme :after #'fp/set-last-theme))

(defun fp/set-fringe-background-color ()
  "Set the fringe to the background color"
  (let ((background-color (face-background 'default)))
    (set-face-attribute 'fringe nil :background background-color)))

;; The fringe color should always be the background color.
(advice-add 'counsel-load-theme :after #'fp/set-fringe-background-color)

(fp/remember-last-theme)
(fp/set-fringe-background-color)

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

;; --- vterm ---
(use-package vterm
  :commands (vterm))

;; --- theme Magic Pywal ---
(use-package theme-magic
  :config
  (theme-magic-export-theme-mode))

;; --- custom functions ---
(defun fp/kill-buffer-name ()
  "Takes the current buffer name and add it to the 'kill-ring'."
  (interactive)
  (kill-new (buffer-name)))

(defun fp/last-item (list)
  "Get the last item of a LIST."
  (car (last list)))

(defun fp/delete-window-balanced ()
  "Delete window and balance all windows after."
  (interactive)
  (delete-window)
  (balance-windows))

(defun fp/open-init-el ()
  "Open Emacs init.el file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun fp/flymake-list-errors ()
  "Execute flymake and switch to that buffer."
  (interactive)
  (if (project-current)
      (flymake-show-project-diagnostics)
    (flymake-show-diagnostics-buffer))
  (other-window 1))

(defun fp/eldoc-doc-buffer-pop-to ()
  "Execute 'eldoc-doc-buffer' and switch to that buffer."
  (interactive)
  (call-interactively 'eldoc-doc-buffer)
  (pop-to-buffer "*eldoc*"))

(defun fp/filter-non-matching-buffers (names buffers)
  "Filters non-matching buffers depending on names.
'NAMES' should include the buffer-names that you want to be filtered away.
'BUFFERS' is the list of buffers that should be filtered.
The return value is a list of buffers."
  (seq-filter (lambda (elt) (not (seq-contains names (buffer-name elt))))
              buffers))

(defun fp/kill-all-buffers ()
  "Kill all buffers, remove other windows and go to dashboard buffer."
  (interactive)
  (let ((do-not-kill '("*dashboard*" "*scratch*" "*Messages*")))
    (mapcar 'kill-buffer (fp/filter-non-matching-buffers do-not-kill (buffer-list)))
    (delete-other-windows)
    (switch-to-buffer (car do-not-kill))))

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

(defun fp/sudo-find-file (file)
  "Open FILE as root."
  (interactive
   (list (read-file-name "Open as root: ")))
  (find-file (if (file-writable-p file)
                 file
               (concat "/sudo:root@localhost:" file))))

(defun fp/project-name-prefix (name)
  "Get project name prefix using NAME."
  (when (project-current)
    (let ((project-name (fp/last-item (split-string (fp/last-item (project-current)) "/" t))))
      (concat "*" project-name "-" name "*"))))

(defun fp/project-vterm ()
  "Create or switch to a vterm buffer based on the built in 'project.el' mode.
If the current buffer is not associated with a project.
When the buffer is not associated with a project it switches to the default vterm buffer."
  (interactive)
  (if (project-current)
      ;; When a project is found, try to find a buffer with that name,
      ;; if no buffer is found we instead create a new vterm buffer with that buffer name.
      (let* ((default-project-vterm-name (fp/project-name-prefix "vterm"))
             (vterm-buffer (get-buffer default-project-vterm-name)))
        (if vterm-buffer
            (switch-to-buffer vterm-buffer)
          (let ((default-directory (car (last (project-current)))))
            (vterm default-project-vterm-name))))
    ;; When no project is found, call the default vterm command.
    (vterm)))

(defun fp/execute-eshell-minibuffer (command)
  "Execute an eshell COMMAND and displays it in the minibuffer."
  (interactive (list (read-shell-command "Eshell command: ")))
  (condition-case err
      (message "%s" (eshell-command-result command))
    (error (message "Error: %s" error))))

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
    "r" '(:ignore t :which-key "replace")
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
    :keymaps 'emacs-lisp-mode-map
    "cd" '(describe-symbol :which-key "describe-symbol"))

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
    :keymaps 'clojure-mode-map
    "ct" '(:ignore t :which-key "Clojure tests")
    "ctt" '(cider-test-run-test :which-key "cider-test-run-test")
    "ctn" '(cider-test-run-ns-tests :which-key "cider-tst-run-ns-tests")
    "cta" '(cider-test-run-project-tests :which-key "cider-test-run-project-tests")
    "pi" '(cljr-add-project-dependency :which-key "cljr-add-project-dependency")
    "cd" '(cider-doc :which-key "cider-doc")
    "cs" '(fp/automatic-cider-jack-in :which-key "cider-jack-in")
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
    :keymaps 'dictionary-mode-map
    "cs" '(dictionary-search :which-key "dictionary-search"))

  (fp/leader-keys
    "w" '(:ignore t :which-key "window")
    "wb" '(balance-windows-area :which-key "other-window")
    "ww" '(ace-window :which-key "other-window")
    "wn" '(split-window-right :which-key "split-window-right")
    "wN" '(split-window-below :which-key "split-window-below")
    "wh" '(evil-window-left :which-key "window-left")
    "wl" '(evil-window-right :which-key "window-right")
    "wj" '(evil-window-down :which-key "window-down")
    "wk" '(evil-window-up :which-key "window-down")
    "ws" '(window-swap-states :which-key "window-swap-states")
    "wd" '(delete-window :which-key "delete-window")
    "wD" '(fp/delete-window-balanced :which-key "delete-window-balanced"))

  (fp/leader-keys
    "o" '(:ignore t :which-key "open")
    "ot" '(fp/project-vterm :which-key "vterm")
    "oT" '(vterm :which-key "vterm")
    "oe" '(fp/open-init-el :which-key "open-init.el")
    "od" '(dictionary :which-key "dictionary"))

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
    "fF" '(fp/sudo-find-file :which-key "sudo-find-file")
    "fc" '(dired-create-empty-file :which-key "create-file")
    "fd" '(dired-create-directory :which-key "create-directory")
    "fs" '(evil-write :which-key "write")
    "fS" '(evil-write-all :which-key "write-all")
    "ft" '(counsel-load-theme :which-key "load-theme")
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
(defun fp/dired-eshell-minibuffer-refresh ()
  "Execute a shell COMMAND and refreshes the Dired buffer when the operation is completed."
  (interactive)
  (call-interactively 'fp/execute-eshell-minibuffer)
  (revert-buffer))

(use-package dired
  :straight nil
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (dired-auto-revert-buffer t))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "f" 'dired-create-empty-file
    "F" 'dired-create-directory
    "s" 'fp/dired-eshell-minibuffer-refresh))

(use-package dired-single
  :after dired)

;; --- counsel ---
(use-package counsel)

;; --- avy ---
(use-package avy)

;; --- ivy ---
(use-package ivy
  :custom ((ivy-use-selectable-prompt t))
  :config
  (ivy-mode 1))

;; --- magit ---
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; --- forge ---

;; sqlite3 is needed for forge.
;; In Emacs 29.* sqlite will be part of Emacs and this package won't be needed anymore.
(use-package sqlite3)

(use-package forge
  :after (magit sqlite3)
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
  :custom ((company-idle-delay nil)
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
           (flymake-no-changes-timeout nil)))

;; --- eldoc ----
(use-package eldoc
  :straight nil
  :config
  (add-hook 'eglot-managed-mode-hook
            #'(lambda ()
                (setq-local eldoc-idle-delay 0.5)
                (setq-local eldoc-echo-area-use-multiline-p 1)
                (setq-local eldoc-documentation-strategy
                            #'eldoc-documentation-compose))))

;; --- eglot ---
(use-package eglot
  :straight nil
  :config
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)))
  (setq eglot-autoshutdown t)
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider
          :codeLensProvider
          :signatureHelpProvider
          :inlayHintProvider
          :foldingRangeProvider
          :documentOnTypeFormattingProvider)))

(use-package eglot-booster
	:after eglot
  :straight (:host github :repo "jdtsmith/eglot-booster" :branch "main")
	:config	(eglot-booster-mode))

;; --- purescript mode ---
(use-package purescript-mode
  :hook ((purescript-mode . turn-on-purescript-indentation)))

;; --- go mode ---
(defun setup-go-mode ()
  "LSP Go install save hooks."
  (setq indent-tabs-mode 1)
  (setq tab-width 2)
  (eglot-ensure))

(use-package go-mode
  :hook (go-mode . setup-go-mode))

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

(defun fp/automatic-cider-jack-in ()
  "Automatically call either 'cider-jack-in-clj' or 'cider-jack-in-cljs'.
The call depends on the current buffers file-extension,
if the extension is .clj 'cider-jack-in-clj' is called,
if the extension is .cljs 'cider-jack-in-cljs' is called."
  (interactive)
  (let ((extension (file-name-extension buffer-file-name)))
    (cond ((string= extension "clj") (call-interactively 'cider-jack-in-clj))
          ((string= extension "cljs") (call-interactively 'cider-jack-in-cljs))
          (t (message (format "Extension: '%s' is not valid." extension))))))

(use-package cider
  :hook (clojure-mode . cider-mode)
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
  :mode ("\\.yaml\\'"
         "\\.yml\\'"
         "\\.yaml.tpl\\'"
         "\\.yml.tpl\\'"))

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
  (let* ((find-using-reg-exp "using [A-Za-z\.]+;")
         (buffer-string (buffer-substring-no-properties (point-min) (point-max)))
         (matches (flatten-list (s-match-strings-all find-using-reg-exp buffer-string)))
         (usings (fp/process-sort-usings matches)))
    (unless (equal matches usings)
      (save-excursion
        (goto-char 1)
        (let ((current-match-number 0))
          (while (search-forward-regexp (concat find-using-reg-exp "\n") nil t)
            (replace-match (concat (nth current-match-number usings) "\n"))
            (setq current-match-number (1+ current-match-number))))))))

(defun csharp-mode-setup()
  "LSP CSharp install save hooks."
  (setq tab-width 4)
  (eglot-ensure)
  (advice-add 'eglot-format-buffer :before #'fp/sort-usings-csharp))

(use-package csharp-mode
  :straight nil
  :hook (csharp-mode . csharp-mode-setup))

;; --- powershell mode ---
(use-package powershell-mode
  :mode ("\\.ps1\\'" . powershell-mode))

;; --- rust mode ---
(defun rust-mode-setup ()
  "Setup 'rust-mode'."
  (setq-default eglot-workspace-configuration
                '((:rust-analyzer . (:checkOnSave (:command "clippy")))))
  (eglot-ensure))

(use-package rust-mode
  :hook (rust-mode . rust-mode-setup)
  :custom
  (rust-format-on-save nil))

;; --- c ---
(use-package c-mode
  :straight nil
  :mode ("\\.c\\'" . c-mode)
  :config (c-set-offset 'case-label '+))

;; --- zig ---
(use-package zig-mode
  :hook ((zig-mode . eglot-ensure))
  :custom (zig-format-on-save nil))

;; --- typescript mode ---
(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :hook (typescript-mode . eglot-ensure)
  :custom (typescript-indent-level 2))

;; --- css mode ---
(use-package css-mode
  :straight nil
  :mode ("\\.css\\'" . css-mode)
  :custom (css-indent-offset 2))

;; --- scss mode ---
(use-package scss-mode
  :mode ("\\.scss\\'")
  :custom (css-indent-offset 2))

;; --- Prettier ---
(use-package prettier-js
  :hook ((css-mode . prettier-js-mode)
         (scss-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)
         (html-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)))

;; --- markdown ---
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; --- docker ---
(use-package dockerfile-mode)

;; --- org-mode ---
(use-package olivetti
  :hook (org-mode . olivetti-mode)
  :custom (olivetti-body-width 90))

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

;;; init.el ends here
