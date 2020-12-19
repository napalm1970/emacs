(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa2" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa3" . "http://www.mirrorservice.org/sites/stable.melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))



(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (spaceline smartparens hungry-delete neotree ggtags yasnippet-snippets yasnippet flycheck company-c-headers company-quickhelp company dired-quick-sort zerodark-theme rainbow-delimiters beacon helm-themes helm-gtags helm-swoop helm window-numbering avy try use-package org-bullets))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("08ef1356470a9d3bf363ffab0705d90f8a492796e9db489936de4bde6a4fdb19" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" default))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(platformio-mode jedi evil-collection company-emacs-eclim eclim evil-leader evil qt-pro-mode projectile magit expand-region auto-yasnippet auto-complete company-c-headers compamy-c-headers spaceline zerodark-theme sdcv dashboard monokai-theme color-themes smartparens yasnippet-snippets window-numbering which-key use-package try tabbar stickyfunc-enhance rainbow-delimiters paredit org-bullets neotree hungry-delete helm-themes helm-swoop helm-gtags helm-cscope gruvbox-theme ggtags flycheck dracula-theme disaster dired-quick-sort company-quickhelp cmake-mode clang-format cider beacon ace-window))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Droid Sans Mono Dotted for Powerline" :foundry "1ASC" :slant normal :weight normal :height 128 :width normal)))))
