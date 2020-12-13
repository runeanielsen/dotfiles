;;; Commentary:
;;; --- Set up 'package' ---

;;; Code:
(load "~/.emacs.d/init-packages")

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

;; Automatically auto-fill mode on in all major modes
auto-fill-function 'do-auto-fill

;; Do not autosave.
auto-save-default nil

;; set default tab char's display width to 4 spaces
tab-width 4

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

;; --- dashboard ---
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-items '((projects . 10)
                        (agenda . 5)))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Set the banner
(setq dashboard-startup-banner "~/.emacs.d/banner-text.txt")
(setq dashboard-show-shortcuts nil)
(setq dashboard-set-footer nil)

;; --- Modeline ---
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; --- doom-themes ---
(use-package doom-themes
  :ensure t
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
  :ensure t
  :config
  (theme-magic-export-theme-mode))

;; --- Evil mode ---
(use-package evil
  :ensure t
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
  "ff" 'counsel-find-file
  "fd" 'dired'
  "SPC" 'projectile-find-file
  ;; buffer
  "bb" 'switch-to-buffer
  "bd" 'kill-current-buffer
  "bS" 'evil-write-all
  "bl" 'evil-switch-to-windows-last-buffer
  ;; code
  "cr" 'lsp-rename
  "cR" 'lsp-workspace-restart
  "lsp" 'comment-or-uncomment-region
  "ce" 'flycheck-list-errors
  "ca" 'lsp-execute-code-action
  "co" 'lsp-organize-imports
  ;; window
  "ww" 'other-window
  "wn" 'split-window-right
  "wd" 'delete-window
  ;; open
  "op" 'treemacs
  ;; project
  "pa" 'projectile-add-known-project
  "pd" 'projectile-remove-known-project
  "pp" 'projectile-switch-project
  "pi" 'projectile-invalidate-cache
  "pb" 'projectile-switch-to-buffer
  "pk" 'projectile-kill-buffers)

;; Counsel
(use-package counsel
  :ensure t)

;; Ivy
(use-package ivy
  :ensure t)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; --- Evil snipe ---
(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-override-mode +1))

;; --- Electrical pair ---
(electric-pair-mode 1)

;; --- go mode ---
(use-package go-mode
    :ensure t)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; --- json mode ---
(use-package json-mode
    :ensure t)

;; --- csharp mode ---
(use-package csharp-mode
    :ensure t)

;; --- python-ms ---
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))

;; --- lsp mode ---
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
    :ensure t
    :hook (
            (csharp-mode . lsp)
            (go-mode . lsp)
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

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
(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

;; optional if you want which-key integration
(use-package which-key
    :ensure t
    :config
    (which-key-mode))

;; --- Projectile ---
(use-package projectile
    :ensure t
    :config
    (projectile-mode +1)
    (setq projectile-completion-system 'ivy))

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
    (treemacs-resize-icons 20)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t))


(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(add-hook 'projectile-after-switch-project-hook #'treemacs-display-current-project-exclusively)

;; --- Flycheck ---
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;; --- Tide ---
(defun setup-tide-mode ()
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
  :ensure t
  :config
    (defvar company-tooltip-align-annotations)
    (setq company-tooltip-align-annotations t)
    (add-hook 'js-mode-hook #'setup-tide-mode))

(use-package js2-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)
(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

(use-package add-node-modules-path
    :ensure t)
(use-package web-mode
    :ensure t)

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

(setq-default
;; set default indent offset on modes
  js-indent-level 2
  css-indent-offset 2
  web-mode-code-indent-offset 2
  web-mode-css-indent-offset 2
  web-mode-markup-indent-offset 2)

(use-package prettier-js
    :ensure t
    :init
    (add-hook 'js2-mode-hook 'prettier-js-mode)
    (add-hook 'web-mode-hook 'prettier-js-mode)
    (add-hook 'scss-mode-hook 'prettier-js-mode))

;;; init.el ends here
