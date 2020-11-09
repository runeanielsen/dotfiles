;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; clients, file templates and snippets.
(setq user-full-name "Rune Nielsen"
      user-mail-address "runenielsen@runbox.com")


;; Theme --------------------------------------------------------------
(setq doom-theme 'doom-spacegrey)


;; Default Browser ----------------------------------------------------
(setq browse-url-browser-function 'browse-url-firefox)


;; Path ---------------------------------------------------------------
(setenv "PATH"
        (concat
         "/usr/bin/go/" ":"
         "/home/notation/go/bin/" ":"
         (getenv "PATH")
         )
        )

(setenv "GOPATH" "/usr/bin/go")


;; Relative line numbers ----------------------------------------------
(setq display-line-numbers-type 'relative)


;; Font ---------------------------------------------------------------
(setq
 doom-font (font-spec :family "monospace" :size 17 :weight 'bold)
 doom-variable-pitch-font (font-spec :family "monospace" :size 17))


;; Pandoc Mode --------------------------------------------------------
(add-hook 'markdown-mode-hook 'pandoc-mode)


;; Don't cache if file not exist---------------------------------------
(defadvice! dont-cache-if-file-doesnt-exist-a (&rest _)
  :before-until #'projectile-cache-files-find-file-hook
  (and buffer-file-name (file-exists-p buffer-file-name)))


;; Theme Magic Pywal --------------------------------------------------
(require 'theme-magic)
(theme-magic-export-theme-mode)


;; Tide ---------------------------------------------------------------
(defun setup-tide-mode ()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq company-tooltip-align-annotations t)

(add-hook 'js-mode-hook #'setup-tide-mode)


;; Python MS ----------------------------------------------------------
(require 'lsp-python-ms)
(setq lsp-python-ms-auto-install-server t)
(add-hook 'python-mode-hook #'lsp) ; or lsp-deferred


;; Treemacs
(add-hook 'projectile-after-switch-project-hook #'treemacs-display-current-project-exclusively)

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
          treemacs-width                         50
          treemacs-workspace-switch-cleanup      'all))
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t))
