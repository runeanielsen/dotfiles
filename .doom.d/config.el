;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; clients, file templates and snippets.
(setq user-full-name "Rune Nielsen"
      user-mail-address "runenielsen@runbox.com")


(setq doom-theme 'doom-spacegrey)
(setq browse-url-browser-function 'browse-url-firefox)

;; Pandoc Mode --------------------------------------------------------
(add-hook 'markdown-mode-hook 'pandoc-mode)


;; Theme Magic Pywal --------------------------------------------------------------
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


;; Font ---------------------------------------------------------------
(setq
 doom-font (font-spec :family "monospace" :size 20 :weight 'bold)
 doom-variable-pitch-font (font-spec :family "monospace" :size 20))


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

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile)
  (local-set-key (kbd "C-c f f"), 'omnisharp-go-to-definition))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
