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
(load-file "/home/napalm/emacs/function.el")


;; (server-start)

(global-set-key [f9] 'bookmark-set)
(global-set-key [f10] 'bookmark-jump)


(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


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

(use-package elpy
			 :ensure t
			 :init
			 (elpy-enable)
			 (elpy-use-ipython)
			 (setq elpy-rpc-backend "jedi"))

(defun my/python-mode-hook ()
  (elpy-mode)
  ;; (jedi:setup)
  ;; (setq jedi:complete-on-dot t)
  ;; (setq elpy-rpc-python-command "python")
  ;; (python-shell-interpreter "ipython")
  (company-quickhelp-mode) )

(add-hook 'python-mode-hook (lambda ()
							  (run-hooks 'my/python-mode-hook)))

(use-package company
  :ensure t
  :init
  (global-company-mode t))

(use-package company-quickhelp
			 :ensure t)


(use-package counsel
  :ensure t
  )



(use-package zenburn-theme
	     :ensure t
	     :config (load-theme 'zenburn -1))

(global-hl-line-mode t)


(use-package monokai
	     :ensure t
	     :config (load-theme 'monokai t))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1))


(use-package helm
  :ensure t
  :config
	(require 'helm-config)
			 (global-set-key (kbd "C-x C-f") 'helm-find-files)
			 (global-set-key (kbd "M-x") 'helm-M-x)
			 (global-set-key (kbd "M-y") 'helm-show-kill-ring)
			 (global-set-key (kbd "C-x b") 'helm-mini)
			 (define-key help-map (kbd "<tab>") 'helm-execute-persistent-action)
			 (global-set-key (kbd "M-i") 'helm-swoop)
			 (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
			 (global-set-key (kbd "C-c h o") 'helm-occur)
			 (setq helm-locate-fuzzy-match t) 
			 (setq helm-apropos-fuzzy-match t)
			 (setq helm-lisp-fuzzy-completion t) 

			 
			 )


(use-package helm-swoop
			 :ensure t
			 :config
			 (require 'helm-swoop))

(use-package helm-themes
			 :ensure t
			 )


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


(use-package lua-mode
	     :ensure t
	     :config
	     (autoload 'lua-mode "lua-mode" "Lua editing mode")
	     (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
	     (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))



(use-package shell-pop
	     :ensure t
	     :init
	     (require 'shell-pop)
	     '(shell-pop--set-shell-type (quote ("shell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))
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
	     :init
			 (progn
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

(global-set-key [f7] 'compile)

(use-package evil
			 :ensure t
			 :init
			 (evil-mode t))


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

(global-set-key (kbd "RET") 'newline-and-indent)
(setq-default tab-width 4)
(setq-local eldoc-documentation-function #'ggtags-eldoc-function)


(use-package hl-line+
			 :ensure t)

;; Linum mode

(setq-default left-fringe-width  10)
(setq-default right-fringe-width  0)
(set-face-attribute 'fringe nil :background "black")


(semantic-mode 1) 
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t) 


(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode t)
  (setq evilmi-may-jump-by-percentage nil) ) 


(use-package evil-easymotion
  :ensure t
  :config
  (evilem-default-keybindings "SPC") 
   ) 

(provide 'init)




;;;
	     ;;End:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-c-headers-path-system
   (quote
	("/usr/include/" "/usr/local/include/" "/usr/include/c++/6.3.1/")))
 '(compilation-always-kill t)
 '(compile-command "make")
 '(custom-safe-themes
   (quote
	("3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(doc-view-continuous t)
 '(nlinum-highlight-current-line t)
 '(nrepl-message-colors
   (quote
	("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
	(evil-easymotion evil-matchit evil-surround monokai eshell-prompt-extras evil-leader evil monokai-theme helm-themes helm-helm-commands helm-gtags helm-swoop company-quickhelp elpy hl-line+ nlinum-hl nlinum company-irony irony company-c-headers desctop+ origami s dumb-jump shell-switcher dired-quick-sort cider clojure-mode window-numbering pdf-tools dired+ projectile migit shell-pop smart-mode-line-powerline-theme lua-mode slime-company slime rainbow-delimiters paredit move-text smartparens ggtags yasnippet iedit expand-region undo-tree beacon helm zenburn-theme which-key use-package try org-bullets jedi flycheck counsel company-jedi ace-window)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(show-paren-mode t)
 '(sml/mode-width
   (if
	   (eq
		(powerline-current-separator)
		(quote arrow))
	   (quote right)
	 (quote full)))
 '(sml/pos-id-separator
   (quote
	(""
	 (:propertize " " face powerline-active1)
	 (:eval
	  (propertize " "
				  (quote display)
				  (funcall
				   (intern
					(format "powerline-%s-%s"
							(powerline-current-separator)
							(car powerline-default-separator-dir)))
				   (quote powerline-active1)
				   (quote powerline-active2))))
	 (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
	(""
	 (:propertize " " face powerline-active1)
	 (:eval
	  (propertize " "
				  (quote display)
				  (funcall
				   (intern
					(format "powerline-%s-%s"
							(powerline-current-separator)
							(cdr powerline-default-separator-dir)))
				   (quote powerline-active1)
				   (quote sml/global))))
	 (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
	(""
	 (:propertize " " face sml/global)
	 (:eval
	  (propertize " "
				  (quote display)
				  (funcall
				   (intern
					(format "powerline-%s-%s"
							(powerline-current-separator)
							(car powerline-default-separator-dir)))
				   (quote sml/global)
				   (quote powerline-active1))))
	 (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
	(""
	 (:propertize " " face powerline-active2)
	 (:eval
	  (propertize " "
				  (quote display)
				  (funcall
				   (intern
					(format "powerline-%s-%s"
							(powerline-current-separator)
							(cdr powerline-default-separator-dir)))
				   (quote powerline-active2)
				   (quote powerline-active1))))
	 (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Slab" :foundry "CYEL" :slant normal :weight normal :height 143 :width normal))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
