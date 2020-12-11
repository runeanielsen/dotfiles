(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

; list the packages you want
(setq package-list
      '(
        use-package
        evil
        lsp-mode
        lsp-ui
        company
        lsp-ivy
        which-key
        flycheck
        go-mode
        lsp-python-ms
        csharp-mode
        web-mode
        js2-mode
        json-mode
        doom-modeline
        doom-themes
        theme-magic
        treemacs
        projectile
        treemacs-projectile
        evil-leader
        evil-snipe
        tide
        eslint-fix
        prettier-js
        add-node-modules-path))

; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
