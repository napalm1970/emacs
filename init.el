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
;; (desktop-save-mode t)
(load-file "/home/napalm/emacs/function.el")


(server-start)

(global-set-key [f9] 'bookmark-set)
(global-set-key [f10] 'bookmark-jump)


(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; (use-package ace-window
;;   :ensure t
;;   :init
;;   (progn
;;     (global-set-key [remap other-window] 'ace-window)
;;     (custom-set-faces
;;      '(aw-leading-char-face
;;        ((t (:inherit ace-jump-face-foreground :height 3.0)))))))


(use-package window-numbering
	     :ensure t
	     :config
	     (progn
	       (setq window-numbering-auto-assign-0-to-minibuffer nil)
	       (global-set-key (kbd "M-0") 'select-window-0)
	       (global-set-key (kbd "M-1") 'select-window-1)
	       (global-set-key (kbd "M-2") 'select-window-2)
	       (global-set-key (kbd "M-3") 'select-window-3)
	       (global-set-key (kbd "M-4") 'select-window-4)
	       (global-set-key (kbd "M-5") 'select-window-5)
	       (global-set-key (kbd "M-6") 'select-window-6)
	       (global-set-key (kbd "M-7") 'select-window-7)
	       (global-set-key (kbd "M-8") 'select-window-8)
	       (global-set-key (kbd "M-9") 'select-window-9)
	       (window-numbering-mode 1)
	       )
	     )


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
;;     (global-set-key (kbd "C-c C-r") 'ivy-resume)
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
    ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
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
	     (progn
	       (global-undo-tree-mode)
	       (setq undo-tree-visualizer-timestamps t)
	       (setq undo-tree-visualizer-diff t)))

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


(use-package shell-pop
	     :ensure t
	     :init
	     (require 'shell-pop)
	     '(shell-pop--set-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))
	     '(shell-pop-term-shell "/bin/bash")
	     '(shell-pop-window-size 60)
	     '(shell-pop-window-position "bottom")
	     :bind ("<f8>" . shell-pop)
	     :config (setq shell-pop-internal-mode "eshell"))

(defalias 'list-buffers 'ibuffer-other-window)
	     
(use-package magit
	     :ensure t
	     :init
	     (progn
	       (bind-key "C-c g" 'magit-status)))


(use-package projectile
	     :ensure t
	     :init (progn
		     (projectile-global-mode)
		     (setq projectile-completion-system 'ivy))
	     )


(use-package dired+
	     :ensure t
	     :config (require 'dired+))

(use-package dired-quick-sort
	     :ensure t
	     :config
	     (dired-quick-sort-setup))

(use-package pdf-tools
	     :ensure t
	     :mode (("\\.pdf\\*" . pdf-view-mode))
	     :config
	     (pdf-tools-install))
(recentf-mode t)
(global-set-key (kbd "C-c r f") 'recentf-open-files)

(use-package clojure-mode
	     :ensure t
	     :config
	     ;; (clojure-mode)
	     (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
	     )

(use-package cider
     :ensure t)

(global-set-key [f7] 'compile)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config 
  ;; (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
:init
(dumb-jump-mode)
  :ensure
)


(use-package shell-switcher
  :ensure t
  :config 
  (setq shell-switcher-mode t)
  :bind (("C-'" . shell-switcher-switch-buffer)
	   ("C-x 4 '" . shell-switcher-switch-buffer-other-window)
	   ("C-M-'" . shell-switcher-new-shell)))

;; Visual commands
(setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
				 "ncftp" "pine" "tin" "trn" "elm" "vim"
				 "nmtui" "alsamixer" "htop" "el" "elinks"
				 ))
(setq eshell-visual-subcommands '(("git" "log" "diff" "show")))
(setq eshell-list-files-after-cd t)
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
	    '(lambda()
	       (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(defun eshell/magit ()
  "Function to open magit-status for the current directory"
  (interactive)
  (magit-status default-directory)
  nil)


(use-package origami
	     :ensure t)

(use-package desktop+
	     :ensure t
	     :config
	     (setq desktop+-special-buffer-handlers
		   '(term-mode
		     compilation-mode
		     org-agenda-mode
		     indirect-buffer
		     Man-mode
		     eshell-mode))
	     :bind
	     (("<f12> c" . desktop+-create)
	      ("<f12> l" . desktop+-load)))

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
 '(doc-view-continuous t)
 '(package-selected-packages
   (quote
    (desctop+ origami s dumb-jump shell-switcher dired-quick-sort cider clojure-mode window-numbering pdf-tools dired+ projectile migit shell-pop smart-mode-line-powerline-theme lua-mode slime-company slime rainbow-delimiters paredit move-text smartparens ggtags yasnippet iedit expand-region undo-tree beacon helm zenburn-theme which-key use-package try org-bullets jedi flycheck counsel company-jedi ace-window)))
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "CYEL" :slant normal :weight normal :height 122 :width normal))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
