(require 'package)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;;(add-to-list 'load-path "/home/napalm/.emacs.d/")
 (add-to-list 'load-path "/home/napalm/.emacs.d/lisp")
;; (add-to-list 'load-path "/home/napalm/.emacs.d/elpa/ido-ubiquitous")
;; (add-to-list 'load-path "/home/napalm/.emacs.d/elpa/smex")
;; (add-to-list 'load-path "/home/napalm/.emacs.d/elpa/lua-mode")
;; (add-to-list 'load-path "/home/napalm/.emacs.d/elpa/dash-20151021.113")
;; (add-to-list 'load-path "/home/napalm/.emacs.d/elpa/smartparens-20151129.1003")


;; (add-hook 'after-init-hook 'global-company-mode)


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'manoj-dark t)
(load-theme 'tsdh-dark t)

(require 'smartparens-config) 
(smartparens-global-mode t)

(setq inhibit-splash-screen t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(require 'smex) ;; Not needed if you use package.el
(smex-initialize) ;; Can be omitted. This might cause a (minimal) delay
					; when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'ido-vertical-mode)
(ido-mode t)
(setq ido-vertical-show-count t)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
(ido-everywhere 1)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(require 'ido-yes-or-no)
(ido-yes-or-no-mode 1)

(setq ido-use-faces t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background nil
                    :foreground "orange")
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background nil
                    :foreground nil)
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground nil)
(ido-vertical-mode 1)


;; (setq show-paren-style 'expression)
;; (show-paren-mode 1)
;; (electric-pair-mode 1)

(global-hl-line-mode 1)

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(global-set-key (kbd "C-c SPC") ' ace-jump-word-mode)
(global-set-key (kbd "C-c C-c SPC") ' ace-jump-char-mode)
(global-set-key (kbd "C-c C-c C-c SPC") ' ace-jump-line-mode)



;; Usage:

;; "C-c SPC" ==> ace-jump-word-mode

;; enter first character of a word, select the highlighted key to move to it.
;; "C-u C-c SPC" ==> ace-jump-char-mode

;; enter a character for query, select the highlighted key to move to it.
;; "C-u C-u C-c SPC" ==> ace-jump-line-mode


;;;; Не надо набирать 'yes' или 'no'. Теперь 'y' либо 'n'
;; (fset 'yes-or-no-p 'y-or-n-p)
;;;; Показывать номер колонки
(column-number-mode t)
;;;; Показывать время
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)
;;;; навигация между окнами при помощи M-`Arrow keys`
(windmove-default-keybindings 'meta)
;;;; copypaste to X buffer
(setq x-select-enable-clipboard t)



(require 'yasnippet)
;;(setq yas-snippet-dirs
;;       "/home/napalm/.emacs.d/lisp/elpa/yasnippet-0.9.0.1/snippets/")
;; ;; (defalias 'yas/get-snippet-table 'yas--get-snippet-tables)
;; ;; (defalias 'yas/table-hash 'yas--table-hash)
;; ;; (delq 'ac-source-yasnippet ac-sources)
;; ;;      
(yas--initialize)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

(global-set-key (kbd "C-c d l") 'duplicate-line)

(defun quick-copy-line ()
  "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
	(end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))

(global-set-key (kbd "C-c q l") 'quick-copy-line)

(defun my--copy-word()
  (interactive)
  (forward-word)
  (setq beg (point))
  ;;  (message "Begin %d" beg)
  (call-interactively 'set-mark-command)
  (backward-word)
  (setq end (point))
  ;;(message "End %d" end)
  (kill-ring-save beg end)
  )

(global-set-key (kbd "C-c c w") 'my--copy-word)

;;; Comment line or region

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

(global-set-key (kbd "C-c c l ") 'comment-or-uncomment-region-or-line)


(add-hook 'c++-mode-hook '(lambda ()
        (setq ac-sources (append '(ac-source-semantic) ac-sources))
        (local-set-key (kbd "RET") 'newline-and-indent)
        (linum-mode t)
        (semantic-mode t)
	(global-ede-mode)
	(global-semanticdb-minor-mode 1)
	(global-semantic-idle-scheduler-mode 1)
	(global-semantic-idle-completions-mode 1)
	(global-semantic-idle-summary-mode 1)
	;; (global-semantic-idle-local-symbol-highlight-mode 1)
	(global-semantic-decoration-mode)
	(global-semantic-highlight-func-mode)
	(global-semantic-show-unmatched-syntax-mode t)
	(global-semantic-highlight-edits-mode 1)
	(semantic-enable-code-helpers)
	(require 'semantic/ia)
	))

(add-hook 'c-mode-hook '(lambda ()
        (setq ac-sources (append '(ac-source-semantic) ac-sources))
        (local-set-key (kbd "RET") 'newline-and-indent)
        (linum-mode t)
        (semantic-mode t)
	(global-ede-mode)
	(global-semanticdb-minor-mode 1)
	(global-semantic-idle-scheduler-mode 1)
	(global-semantic-idle-completions-mode 1)
	(global-semantic-idle-summary-mode 1)
	;; (global-semantic-idle-local-symbol-highlight-mode 1)
	(global-semantic-decoration-mode)
	(global-semantic-highlight-func-mode)
	(global-semantic-show-unmatched-syntax-mode t)
	(global-semantic-highlight-edits-mode 1)
	(semantic-enable-code-helpers)
	(require 'semantic/ia)
	))

(global-ede-mode t)


(defun my-c-mode-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert))


(add-hook 'c++-mode-common-hook 'my-c-mode-cedet-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)


;; (require 'auto-complete)
;; (require 'auto-complete-config)

;; ;; (global-auto-complete-mode t)
;; (add-to-list 'ac-dictionary-directories "/home/napalm/.emacs.d/lisp/ac-dict")
;; (ac-config-default)

(require 'semantic/ia)
;; (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)


(require 'helm-mode)
(require 'helm-config)

(helm-autoresize-mode 1)

(global-set-key (kbd"C-c o") 'helm-occur)
(global-set-key (kbd"C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-X C-f") 'helm-find-files)


(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(setq helm-autoresize-max-height 40)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t)


(require 'helm-gtags)
(helm-gtags-mode t)


(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
(define-key helm-gtags-mode-map (kbd "C-c g c") 'helm-gtags-create-tags)

(helm-mode 1)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

;; (semantic-add-system-include "/usr/include/" 'c++-mode)
;; (semantic-add-system-include "/usr/include/c++/5.2.1" 'c++-mode)


(require 'semantic/bovine/c)
(add-to-list 'semantic-lex-c-preprocessor-symbol-file
    "/usr/lib/gcc/x86_64-linux-gnu/5/include/stddef.h")

(global-flycheck-mode t)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))



;; (set-face-background 'hl-line "#dcdcdc")

(add-hook 'c-mode-hook 'projectile-mode)
(add-hook 'c++-mode-hook 'projectile-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)


(add-to-list 'load-path "~/.emacs.d/function-args")
(require 'function-args)
(fa-config-default)

(require 'semantic/bovine/c)
(add-to-list 'semantic-lex-c-preprocessor-symbol-file
	     "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h")

(set-default 'semantic-case-fold t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(display-time-mode t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")
     ("Marmalaade" . "http://marmalade-repo.org/packages/"))))
 '(shell-pop-default-directory "/Users/kyagi/git")
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   (quote
    ("shell" "*shell*"
     (lambda nil
       (shell shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "C-x t")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 30)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "unknown" :slant normal :weight normal :height 122 :width normal))))

 )



;;; Lua mode

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


 (require 'auto-highlight-symbol)
     (global-auto-highlight-symbol-mode t)

;; (global-set-key (kbd "M-p") 'ace-window)
;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; (setq aw-background nil)


(window-numbering-mode 1)


(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))

(global-highlight-parentheses-mode t)


(require 'shell-pop)



(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'eldoc-mode)

(eval-after-load "company"
 '(progn
    (add-to-list 'company-backends 'company-anaconda)))

(defun my-annotation-function (candidate)
  (let ((description (get-text-property 0 'description candidate)))
    (when description
      (concat "<" (substring description 0 1) ">"))))

(setq company-anaconda-annotation-function 'my-annotation-function)
(add-hook 'python-mode-hook 'eldoc-mode)
(setq python-shell-virtualenv-path "/usr/bin/virtualenv")

(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
(load "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
;; /home/napalm/go/src/golang.org/x/tools/cmd/oracle/oracle.el

(add-hook 'go-mode-hook 'oracle)

(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-g") 'go-goto-imports)))

(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-k") 'godoc)))


(add-to-list 'load-path "/home/napalm/go/src/github.com/dougm/goflymake")
(require 'go-flycheck)

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
  (set (make-local-variable 'company-backends) '(company-go))
  ;; (company-mode)
  ))


