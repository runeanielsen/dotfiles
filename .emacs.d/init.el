;; --- Set up 'package' ---
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Load and activate emacs packages. Do this first so that the packages are loaded before
;; you start trying to modify them.  This also sets the load path.
(package-initialize)
;;(package-refresh-contents)

;; --- install packages ---
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'evil)
  (package-install 'evil))

(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))

(unless (package-installed-p 'lsp-ui)
  (package-install 'lsp-ui))

(unless (package-installed-p 'company)
  (package-install 'company))

(unless (package-installed-p 'lso-ivy)
  (package-install 'lsp-ivy))

(unless (package-installed-p 'which-key)
  (package-install 'which-key))

(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))

(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))

(unless (package-installed-p 'csharp-mode)
  (package-install 'csharp-mode))

(unless (package-installed-p 'web-mode)
  (package-install 'web-mode))

(unless (package-installed-p 'js2-mode)
  (package-install 'js2-mode))

(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(unless (package-installed-p 'doom-modeline)
  (package-install 'doom-modeline))

(unless (package-installed-p 'doom-themes)
  (package-install 'doom-themes))

(unless (package-installed-p 'theme-magic)
  (package-install 'theme-magic))

(unless (package-installed-p 'treemacs)
  (package-install 'treemacs))

(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(unless (package-installed-p 'treemacs-projectile)
  (package-install 'treemacs-projectile))

(unless (package-installed-p 'evil-leader)
  (package-install 'evil-leader))

(unless (package-installed-p 'evil-snipe)
  (package-install 'evil-snipe))

(unless (package-installed-p 'evil-snipe)
  (package-install 'tide))

(unless (package-installed-p 'lsp-python-ms)
  (package-install 'lsp-python-ms))

(unless (package-installed-p 'tide)
  (package-install 'tide))

;; --- load use-packages ---

(eval-when-compile
  (require 'use-package))

(add-to-list 'default-frame-alist
             '(font . "Monospace-13"))

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

 ;; Allow commands to be run on minibuffers.
 enable-recursive-minibuffers t)

;; Change all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Delete whitespace just when a file is saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable narrowing commands.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Display column number in mode line.
(column-number-mode t)

;; Automatically update buffers if file content on the disk has changed.
(global-auto-revert-mode t)

;; Automatically auto-fill mode on in all major modes
(setq-default auto-fill-function 'do-auto-fill)

;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)

;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 4)

;; make indent commands use space only (never tab character)
(progn
  (setq-default indent-tabs-mode nil)
  )

;; display line numbers
(global-display-line-numbers-mode)

;; relative line numbers
(setq-default display-line-numbers 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 2
              display-line-numbers-widen t)

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

;; --- Modeline ---
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

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
(require 'theme-magic)
(theme-magic-export-theme-mode)

;; --- Evil mode ---
(use-package evil
  :init
  :config
  (evil-mode 1))

;; --- Evil leader ---
(use-package evil-leader
  :ensure t
  :defer t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

;; --- Keybindings ---
(evil-leader/set-key
  ;; files
  "ff" 'find-file
  "fd" 'dired'
  "SPC" 'projectile-find-file
  ;; buffer
  "bb" 'switch-to-buffer
  "bd" 'kill-current-buffer
  "bS" 'evil-write-all
  ;; lsp
  "cr" 'lsp-rename
  ;; window
  "ww" 'other-window
  "wn" 'split-window-right
  "wd" 'delete-window
  ;; open
  "ot" 'treemacs
  ;; project
  "pa" 'projectile-add-known-project
  "pp" 'projectile-switch-project)

;; --- Evil snipe ---
(use-package evil-snipe
  :config
  (evil-snipe-override-mode +1))

;; --- Electrical pair ---
(electric-pair-mode 1)

;; --- lsp mode ---
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
    :hook (
            (csharp-mode . lsp)
            (go-mode . lsp)
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))



;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode :ensure t)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; Ivy
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;; --- Projectile ---
(projectile-mode +1)
(setq projectile-completion-system 'ivy)

;; --- Treemacs ---
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         45
          treemacs-workspace-switch-cleanup      'all))
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(add-hook 'projectile-after-switch-project-hook #'treemacs-display-current-project-exclusively)


;; --- Tide ---
(defun setup-tide-mode ()
  (interactive)
  ;;  (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  (tide-setup)
  (if (file-exists-p (concat tide-project-root "node_modules/typescript/bin/tsserver"))
    (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver"))
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq tide-format-options '(:indentSize 2 :tabSize 2 :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
  (local-set-key (kbd "C-c d") 'tide-documentation-at-point)
  (company-mode +1)
  (setq company-minimum-prefix-length 1))

(require 'use-package)
(use-package tide
  :ensure t
  :config
  (progn
    (company-mode +1)
    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  ))

;; use web-mode + tide-mode for javascript instead
(use-package js2-mode
  :ensure t
  :config
  (progn
    (add-hook 'js2-mode-hook #'setup-tide-mode)
    ;; configure javascript-tide checker to run after your default javascript checker
    (setq js2-basic-offset 2)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))))

;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(use-package json-mode
  :ensure t
  :config
  (progn
    (flycheck-add-mode 'json-jsonlint 'json-mode)
    (add-hook 'json-mode-hook 'flycheck-mode)
    (setq js-indent-level 2)
    (add-to-list 'auto-mode-alist '("\\.json" . json-mode))))

(use-package web-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js"     . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html"   . web-mode))
    ;; this magic incantation fixes highlighting of jsx syntax in .js files
    (setq web-mode-content-types-alist
          '(("jsx" . "\\.js[x]?\\'")))
    (add-hook 'web-mode-hook
              (lambda ()
                (setq web-mode-code-indent-offset 2)
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))
                (when (string-equal "jsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))
                (when (string-equal "js" (file-name-extension buffer-file-name))
                  (progn
                    (setup-tide-mode)
                    (with-eval-after-load 'flycheck
                      (flycheck-add-mode 'typescript-tslint 'web-mode)
                      (flycheck-add-mode 'javascript-tide 'web-mode))))))
    ))
