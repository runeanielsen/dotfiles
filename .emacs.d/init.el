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

(unless (package-installed-p 'doom-modeline)
  (package-install 'doom-modeline))

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
(setq display-line-numbers 'relative
      display-line-numbers-current-absolute t)

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

;; --- Evil mode ---

(use-package evil
  :init
  :config
  (evil-mode 1))

;; --- Electrical pair ---

(electric-pair-mode 1)

;; --- lsp mode ---

(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
            (go-mode . lsp)
            ;; if you want which-key integration
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

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

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))
