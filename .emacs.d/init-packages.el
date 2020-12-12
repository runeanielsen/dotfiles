(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

; list the packages you want
(setq package-list
      '(
        use-package))

; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
