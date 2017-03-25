;;; Code:

(require 'package)

(when (>= emacs-major-version 24)
  (setq package-archives '(
			   ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")
                           ("melpa-stable" . "http://stable.melpa.org/packages/"))))

(package-initialize)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(set-face-attribute 'default nil :height 150)
(setq default-frame-alist
      '((top . 20) (left . 800)
	(width .100) (height . 40)))



(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))


(use-package company
  :ensure t
  :init
  (global-company-mode t))


(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))


(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))
(global-hl-line-mode t)


(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1))


;; (use-package hydra
;;   :ensure hydra
;;   :init
;;   (global-set-key (kbd "M-SPC")
;; 		  (defhydra file (:color blue)
;; 		    "file"
;; 		    ("f" hydra-outline/body "find file"))))

(use-package helm
  :ensure t
  :config
  (require 'helm-config))



; flashes the cursor's line when you scroll
(use-package beacon
:ensure t
:config
(beacon-mode 1)
; this color looks good for the zenburn theme but not for the one
; I'm using for the videos
; (setq beacon-color "#666600")
)


(use-package undo-tree
:ensure t
  :init
    (global-undo-tree-mode))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))


(use-package iedit
  :ensure t)


(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when derived-mode-p 'c-mode 'c++-mode)
	      (ggtags-mode 1))))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
)


(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

(setq select-enable-clipboard t)

(use-package paredit
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'paredit-mode))

(use-package rainbow-delimiters
	     :ensure t
	     :init
	     (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(show-paren-mode t)

(require 'cl-lib)
(setq-default inferior-lisp-program "sbcl")

(use-package slime
  :ensure t
  :config
  (require 'slime)
  (require 'slime-autoloads)
  (slime-setup '(slime-asdf
		 slime-fancy
		 slime-indentation
		 slime-company))
  (setq-default slime-net-coding-system 'utf-8-unix)
  (setq-default lisp-body-indent 2)
  (setq-default lisp-indent-function 'common-lisp-indent-function))

(use-package lua-mode
	     :ensure t
	     :config
	     (autoload 'lua-mode "lua-mode" "Lua editing mode")
	     (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
	     (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))



(use-package smart-mode-line-powerline-theme
	     :ensure smart-mode-line-powerline-theme)
(use-package smart-mode-line
	     :ensure smart-mode-line
	     :init
	     (progn
	      (setq sml/no-confirm-load-theme t)
	       (sml/setup)
	       (sml/apply-theme 'powerline))
	     )


















	     
	     (provide 'init)




;;;
	     ;;End:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(package-selected-packages
   (quote
    (smart-mode-line-powerline-theme lua-mode slime-company slime rainbow-delimiters paredit move-text smartparens ggtags yasnippet iedit expand-region undo-tree beacon helm zenburn-theme which-key use-package try org-bullets jedi flycheck counsel company-jedi ace-window)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "CYEL" :slant normal :weight normal :height 122 :width normal))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
