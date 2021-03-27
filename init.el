;;; .emacs --- -*- lexical-binding: t -*-
;;; Comentary:



(setq inhibit-startup-message t)
(show-paren-mode 1)

;; we will call `blink-matching-open` ourselves...
(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)

;; this still needs to be set for `blink-matching-open` to work
(setq blink-matching-paren 'show)
(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
    (defun show-paren--off-screen+ (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp ov)
        (delete-overlay ov))
      ;; check if it's appropriate to show match info,
      ;; see `blink-paren-post-self-insert-function'
      (when (and (overlay-buffer show-paren--overlay)
                 (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; rebind `minibuffer-message' called by
        ;; `blink-matching-open' to handle the overlay display
        (cl-letf (((symbol-function #'minibuffer-message)
                   (lambda (msg &rest args)
                     (let ((msg (apply #'format-message msg args)))
                       (setq ov (display-line-overlay+
                                 (window-start) msg))))))
          (blink-matching-open))))))

;; (add-hook 'prog-mode-hook (linum-mode 1))

;; (global-linum-mode t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(global-hl-line-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-prettify-symbols-mode t)


(setq scroll-step 1)
(setq scroll-margin 25)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; Set up the visible bell
(setq visible-bell t)


  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


(set-face-attribute 'default nil :font "Fira Code Retina" :height 113)
;; (load-theme 'wombat)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)

(use-package which-key
  :config
  (which-key-mode))

(use-package page-break-lines
  :ensure t
  :diminish
  :init (global-page-break-lines-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  ;; (define-key help-map (kbd "<tab>") 'helm-execute-persistent-action)
  (global-set-key (kbd "M-i") 'helm-swoop)
  (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (setq helm-locate-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t))

(use-package helm-swoop
  :config
  (require 'helm-swoop))

(use-package helm-make)


(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)


(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(setq doom-themes-treemacs-theme "doom-colors")
(doom-themes-treemacs-config)
(doom-themes-org-config)

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package helm-themes)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))



(use-package evil-leader
  :diminish global-evil-leader-mode
  ;; :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-key
    "e" 'helm-find-files
    "b" 'helm-mini
    "k" 'kill-buffer
    "m" 'helm-make))

(evil-leader/set-leader ",")

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))



(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-items '((recents . 10)
			    (projects .10)
			    (bookmarks . 10))))
  :config
  (dashboard-setup-startup-hook))


(use-package window-numbering
  :config
  (progn
    (setq window-numbering-auto-assign-0-to-minibuffer nil)
    ;; (global-set-key (kbd "M-0") 'select-window-0)
    (global-set-key (kbd "M-1") 'select-window-1)
    (global-set-key (kbd "M-2") 'select-window-2)
    (global-set-key (kbd "M-3") 'select-window-3)
    (global-set-key (kbd "M-4") 'select-window-4)
    (global-set-key (kbd "M-5") 'select-window-5)
    (global-set-key (kbd "M-6") 'select-window-6)
    (global-set-key (kbd "M-7") 'select-window-7)
    (global-set-key (kbd "M-8") 'select-window-8)
    (global-set-key (kbd "M-9") 'select-window-9)
    (window-numbering-mode 1)))


(use-package flycheck
  :config
  (global-flycheck-mode t))




(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))


(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (use-package yasnippet-snippets :after yasnippet)
  :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  (:map yas-keymap
        (("TAB" . smarter-yas-expand-next-field)
         ([(tab)] . smarter-yas-expand-next-field)))
  :config
  (yas-reload-all)
  (defun smarter-yas-expand-next-field ()
    "Try to `yas-expand' then `yas-next-field' at current cursor position."
    (interactive)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (yas-expand)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (ignore-errors (yas-next-field))))))


;; (use-package yasnippet
;;   :init
;;   (yas-global-mode 1))

;; (use-package yasnippet-snippets
;;   :init
;;   (setq yas-snippet-dirs
;; 	(progn
;; 	  (dired  '("/home/napalm/.emacs.d/elpa/"))
;; 	  (switch-to-buffer (other-buffer))
;; 	  (set-buffer (other-buffer))
;; 	  (dired-mark-files-regexp "yasnippet-snippets-[0-9]*\.[0-9]*")
;; 	  (setq fname (dired-get-filename))
;; 	  (kill-buffer (other-buffer))
;; 	  (list fname))))



(use-package treemacs
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         25
          treemacs-workspace-switch-cleanup      nil
	  treemacs-show-hidden-files             nil)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)))


(use-package treemacs-evil
  :after treemacs evil
  :ensure t)


(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)


(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;; (use-package eglot
;;   :hook
;;   (c++-mode . eglot-ensure)
;;   (c-mode . eglot-ensure)
;;   ;; (python-mode . eglot-ensure)
;;   (kotlin-mode . eglot-ensure)
;;   ;; (java-mode . eglot-ensure)
;;   )

(use-package platformio-mode
  :config
  (add-hook 'c++-mode-hook (lambda ()
			     (platformio-conditionally-enable)
			     (platformio-mode 1))))


(use-package sly
  :config
  (setq inferior-lisp-program "sbcl")
  (setq sly-contribs '(sly-scratch sly-mrepl))

  (add-hook 'sly-mode-hook
            (lambda ()
              (unless (sly-connected-p)
		(save-excursion (sly))))))


(use-package sly-macrostep
  :after sly)

(use-package sly-repl-ansi-color
  :init (push 'sly-repl-ansi-color sly-contribs))


(use-package beacon
  :config
  (beacon-mode 1))


(use-package eshell-toggle
  :bind ("C-M-'" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))



(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))



(use-package kotlin-mode
  :hook ((kotlin-mode . (lambda () (lsp))))
  :init
  (setenv "PATH" (concat (getenv "PATH") ":/home/napalm/build/kotlin-language-server/server/build/install/server/bin:/home/napalm/build/kotlin-language-server/server/build/install/server/"))

(setq exec-path (append exec-path '("/home/napalm/build/kotlin-language-server/server/build/install/server/")))
(setq exec-path (append exec-path '("/home/napalm/build/kotlin-language-server/server/build/install/server/bin/")))
  )

(use-package quickrun
  :bind
  (("<f5>" . quickrun)
   ("M-<f5>" . quickrun-shell)
   ("C-c e" . quickrun)
   ("C-c C-e" . quickrun-shell)))



(setq lsp-java-jdt-download-url  "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")

(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))
(use-package lsp-ui)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode)
  :hook
  (sh-mode . lsp))
(use-package dap-java :ensure nil)
(use-package helm-lsp)
(use-package lsp-treemacs)



(use-package ccls
  :defer t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :custom
  (ccls-executable (executable-find "ccls")) ; Add ccls to path if you haven't done so
  (ccls-sem-highlight-method 'font-lock)
  (ccls-enable-skipped-ranges nil)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection (cons ccls-executable ccls-args))
    :major-modes '(c-mode c++-mode cuda-mode objc-mode)
    :server-id 'ccls-remote
    :multi-root nil
    :remote? t
    :notification-handlers
    (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
            ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
    :initialization-options (lambda () ccls-initialization-options)
    :library-folders-fn nil)))


;; (use-package lsp-jedi
;;   :ensure t
;;   :hook ((python-mode . (lambda () (lsp))))
;;   )

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package magit
  :if (executable-find "git")
  :bind
  (("C-x g" . magit-status)
   (:map magit-status-mode-map
         ("M-RET" . magit-diff-visit-file-other-window)))
  :config
  (defun magit-log-follow-current-file ()
    "A wrapper around `magit-log-buffer-file' with `--follow' argument."
    (interactive)
    (magit-log-buffer-file t)))


(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))

(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))


(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

;; (use-package aweshell
;;   :load-path (lambda () (expand-file-name "aweshell" user-emacs-directory))
;;   :commands (aweshell-new aweshell-dedicated-open)
;;   :bind
;;   (("M-#" . aweshell-dedicated-open)
;;    (:map eshell-mode-map ("M-#" . aweshell-dedicated-close))))



;; Remove useless whitespace before saving a file
(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.

The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(defun smart-delete-trailing-whitespace ()
  "Invoke `delete-trailing-whitespace-except-current-line' on selected major modes only."
  (unless (member major-mode '(diff-mode))
    (delete-trailing-whitespace-except-current-line)))

(add-hook 'before-save-hook #'smart-delete-trailing-whitespace)

;; Replace selection on insert
(delete-selection-mode 1)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(helm-themes discover-my-major undo-tree popup-kill-ring magit lsp-clients yasnippet-snippets ws-butler window-numbering which-key use-package treemacs-projectile treemacs-evil smartparens sly-repl-ansi-color sly-macrostep rainbow-delimiters quickrun platformio-mode lsp-ui lsp-jedi lsp-java kotlin-mode helm-swoop helm-projectile helm-make helm-lsp flycheck evil-surround evil-leader evil-collection eshell-toggle doom-themes doom-modeline diminish dashboard company-box ccls beacon)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
