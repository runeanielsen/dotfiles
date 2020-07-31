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
 doom-font (font-spec :family "monospace" :size 15 :weight 'bold)
 doom-variable-pitch-font (font-spec :family "monospace" :size 15))


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#171717" "#BF616A" "#A3BE8C" "#ECBE7B" "#8FA1B3" "#c678dd" "#46D9FF" "#c0c5ce"])
 '(custom-safe-themes
   (quote
    ("d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "82360e5f96244ce8cc6e765eeebe7788c2c5f3aeb96c1a765629c5c7937c0b5b" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" default)))
 '(fci-rule-color "#65737E")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#D08770"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#A3BE8C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#4f5b66"))
 '(objed-cursor-color "#BF616A")
 '(pdf-view-midnight-colors (cons "#c0c5ce" "#2b303b"))
 '(rustic-ansi-faces
   ["#2b303b" "#BF616A" "#A3BE8C" "#ECBE7B" "#8FA1B3" "#c678dd" "#46D9FF" "#c0c5ce"])
 '(vc-annotate-background "#2b303b")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A3BE8C")
    (cons 40 "#bbbe86")
    (cons 60 "#d3be80")
    (cons 80 "#ECBE7B")
    (cons 100 "#e2ab77")
    (cons 120 "#d99973")
    (cons 140 "#D08770")
    (cons 160 "#cc8294")
    (cons 180 "#c97db8")
    (cons 200 "#c678dd")
    (cons 220 "#c370b6")
    (cons 240 "#c16890")
    (cons 260 "#BF616A")
    (cons 280 "#a35f69")
    (cons 300 "#875e68")
    (cons 320 "#6b5c67")
    (cons 340 "#65737E")
    (cons 360 "#65737E")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
