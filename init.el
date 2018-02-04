;;; Code:
;;;Commentary:

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
(electric-indent-mode -1)

(fringe-mode '(8 . 0)) 
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Linum plugin

(require 'linum) 

(line-number-mode   t) 

(global-linum-mode  t) 

(column-number-mode t) 
(setq linum-format " %d") 


;; Disable backup/autosave files
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil) 




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
  (elpy-mode t)
   (jedi:setup)
   (setq jedi:complete-on-dot t)
   (setq elpy-rpc-python-command "python")
   (python-shell-interpreter "python")
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

(global-hl-line-mode t)

(use-package dracula-theme
  :ensure t
  :config (load-theme 'dracula t))


;; (use-package flycheck
;;   :ensure t
;;   :init
;;   ;; (global-flycheck-mode 1)
;;   ;; (setq-default flycheck-disabled-checkers
;;   ;; (append flycheck-disabled-checkers '(javascript-jshint)))
;;   (flycheck-add-mode 'javascript-eslint 'web-mode)
;;   (setq-default flycheck-temp-prefix ".flycheck")
;;   (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers '(json-jsonlist))
;;   ))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
        (when (and eslint (file-executable-p eslint))
            (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


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



;; (use-package undo-tree
;;     :ensure t
;;     :init
;;     (progn
;;         (global-undo-tree-mode)
;;         (setq undo-tree-visualizer-timestamps t)
;;         (setq undo-tree-visualizer-diff t))
;;     )

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

;; (use-package ggtags
;;   :ensure t
;;   :config
;;   (add-hook 'c-mode-common-hook
;; 	    (lambda ()
;; 	      (when derived-mode-p 'c-mode 'c++-mode)
;; 	      (ggtags-mode 1))))

(use-package helm-gtags
  :ensure t
  :config
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
   helm-gtags-prefix-key "\C-cg"
   helm-gtags-suggested-key-mapping t
   )
;; Enable helm-gtags-mode
(add-hook 'lua-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))

(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)

(use-package avy
  :ensure t)


(global-set-key (kbd "M-g c") 'avy-goto-char)
(global-set-key (kbd "M-g l") 'avy-goto-line)


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
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)
  (ggtags-mode 1))

  (bind-keys :map lua-mode-map
			 ("C-c s l" . lua-send-current-line)
			 ("C-c s r" . lua-send-region)
			 ("C-c s d" . lua-send-defun)))

(add-hook 'lua-mode-hook
	    '(lambda()
	       (linum-mode 1)
		   (hs-minor-mode 1)))


(use-package love-minor-mode
  :ensure t)

(use-package company-lua
  :ensure t
  )

(defun my-lua-mode-company-init ()
  (setq-local company-backends '((company-lua
                                  company-etags
                                  company-dabbrev-code
                                  company-yasnippet))))
(add-hook 'lua-mode-hook #'my-lua-mode-company-init)


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
		     (projectile-mode)
		     (setq projectile-completion-system 'ivy))
	     )


(use-package dired+
	     :ensure t
	     :config (require 'dired+))

(use-package dired-quick-sort
	     :ensure t
	     :config
	     (dired-quick-sort-setup))


(recentf-mode t)
(global-set-key (kbd "C-c r f") 'recentf-open-files)

(global-set-key [f7] 'compile)



(defun eshell-clear-buffer()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
	    '(lambda()
	       (local-set-key (kbd "C-l") 'eshell-clear-buffer)))


(setq-default indent-tabs-mode nil) 
(setq-default tab-width          4) 
(setq-default c-basic-offset     2)
(setq-default standart-indent    4) 
(setq-default lisp-body-indent   4) 
(setq-local eldoc-documentation-function #'ggtags-eldoc-function)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Scrolling settings
(setq scroll-step               1) 
(setq scroll-margin            10) 
(setq scroll-conservatively 10000)



(use-package hl-line+
			 :ensure t
             :config
             (hl-line-highlight-now)
             (setq line-show-period 12000))

;; Linum mode

(setq-default left-fringe-width  10)
(setq-default right-fringe-width  0)
(set-face-attribute 'fringe nil :background "black")


(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t) 


(use-package counsel
  :ensure t)

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper))) 


(global-set-key (kbd "M-/") #'hippie-expand)
(save-place-mode t)


(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (use-package treemacs-evil
      :ensure t
      :demand t)
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind
  (:map global-map
        ([f12]        . treemacs-toggle)
        ("M-0"       . treemacs-select-window)
        ("C-c 1"     . treemacs-delete-other-windows)
        ;; ("M-m ft"    . treemacs-toggle)
        ;; ("M-m fT"    . treemacs)
        ;; ("M-m f C-t" . treemacs-find-file)
		))
(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  ;; :bind (:map global-map
  ;;             ("M-m fP" . treemacs-projectile)
  ;;             ("M-m fp" . treemacs-projectile-
  )


(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)
(global-set-key (kbd "<C-return>")  'cider-eval-last-sexp)


(use-package slime
    :ensure t
    :config
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (setq slime-contribs '(slime-fancy)))

(use-package slime-company
    :ensure t
    :config
    (slime-setup '(slime-fancy slime-company)))

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)


(use-package emmet-mode
    :ensure t
    :config
    (add-hook 'css-mode-hook  'emmet-mode)
    (add-hook 'html-mode-hook  'emmet-mode)
    (progn
	       (bind-key "C-c e" 'emmet-expand-yas))
    )

(use-package helm-emmet
    :ensure t)

(use-package web-mode
    :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-css-colorization t))


(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(use-package json-mode
    :ensure t)

(use-package js2-mode
    :ensure t)

(use-package ac-js2
    :ensure t)


(use-package coffee-mode
    :ensure t)

(use-package tern
    :ensure t)

(use-package tern-auto-complete
    :ensure t)

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))


(use-package exec-path-from-shell
    :ensure t
    :config
    (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))


(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)



(require 'helm-rtags)
(setq rtags-use-helm t)


(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))


(setq company-idle-delay 0)
(define-key c-mode-map [(tab)] 'semantic-complete-symbol)
(define-key c++-mode-map [(tab)] 'semantic-complete-symbol)

(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))


(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

(require 'flycheck-rtags)

(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)


(cmake-ide-setup)

(add-hook 'c++-mode-hook 'semantic-mode)
(add-hook 'c-mode-hook 'semantic-mode)



(provide 'init)




;;;
	     ;;End:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   (quote
    ("8ed752276957903a270c797c4ab52931199806ccd9f0c3bb77f6f4b9e71b9272" default)))
 '(doc-view-continuous t)
 '(fci-rule-color "#383838")
 '(flycheck-c/c++-clang-executable "/usr/bin/clang++")
 '(flycheck-c/c++-gcc-executable "/usr/bin/g++")
 '(flycheck-clang-args (quote ("")))
 '(flycheck-clang-include-path
   (quote
    ("/usr/include/" "/usr/include/linux/" "/usr/include/c++/7.2.1/" "/usr/include/c++/7.2.1/x86_64-pc-linux-gnu")))
 '(flycheck-clang-language-standard "c++14")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (cmake-ide flycheck-apertium company-irony-c-headers company-irony irony helm-rtags company-rtags rtags exec-path-from-shell tern-auto-complete tern coffee-mode cofee-mode ac-js2 js2-mode json-mode web-mode helm-emmet emmet-mode emet-mode love-minor-mode dracula-theme helm-gtags company-lua git-gutter treemacs-projectile evil-escape evil-tutor ivy-gitlab monokai zenburn-theme window-numbering which-key use-package try smartparens smart-mode-line-powerline-theme slime-company shell-switcher shell-pop rainbow-delimiters projectile pdf-tools paredit origami org-bullets monokai-theme lua-mode iedit hl-line+ helm-themes helm-swoop ggtags flycheck expand-region evil-surround evil-matchit evil-magit evil-leader evil-easymotion eshell-prompt-extras elpy dumb-jump dired-quick-sort dired+ desktop+ counsel company-quickhelp cider beacon)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Slab" :foundry "CYEL" :slant normal :weight normal :height 120 :width normal)))))
