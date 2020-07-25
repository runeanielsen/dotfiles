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
 doom-font (font-spec :family "monospace" :size 20 :weight 'bold)
 doom-variable-pitch-font (font-spec :family "monospace" :size 20))


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


;; Prettier -----------------------------------------------------------
 (add-hook 'js-mode-hook 'prettier-js-mode)


;; C# -----------------------------------------------------------------
(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile)
  (local-set-key (kbd "C-c f f"), 'omnisharp-go-to-definition))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
