;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code

(load "~/.emacs.d/init_install_package")
(load "~/.emacs.d/bookmarks-settings.el")
(load "~/.emacs.d/global_settings")
(load "~/.emacs.d/function")
(load "~/.emacs.d/doc-view-mode.el")
(load "~/.emacs.d/c-mode-settings.el")
(load "~/.emacs.d/c++-mode-settings.el")



;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


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

(global-set-key (kbd "C-c SPC") 'ace-jump-word-mode)
(global-set-key (kbd "C-c C-c SPC") 'ace-jump-char-mode)
(global-set-key (kbd "C-c C-c C-c SPC") 'ace-jump-line-mode)



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
(yas/initialize)
(global-set-key (kbd "C-c k") 'yas/expand)







;; (require 'auto-complete)
;; (require 'auto-complete-config)

;; ;; (global-auto-complete-mode t)
;; (add-to-list 'ac-dictionary-directories "/home/napalm/.emacs.d/lisp/ac-dict")
;; (ac-config-default)

;; (require 'semantic/ia)
;; (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)

(require 'helm)
(require 'helm-config)

(helm-autoresize-mode 1)

;; (global-set-key (kbd"C-c o") 'helm-occur)
(global-set-key (kbd"C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-X C-f") 'helm-find-files)
(global-set-key (kbd "M-i") 'helm-swoop)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)



(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(setq helm-M-x-fuzzy-match t)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)


(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))


(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(helm-mode 1)

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



(global-flycheck-mode t)
;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))


(add-hook 'c-mode-hook 'projectile-mode)
(add-hook 'c++-mode-hook 'projectile-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)


(add-to-list 'load-path "~/.emacs.d/function-args")
(require 'function-args)
(fa-config-default)

;; (require 'semantic/bovine/c)
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file
;; 	     "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h")

(set-default 'semantic-case-fold t)


;;; Lua mode

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'highlight-symbol)
(global-set-key [(control f9)] 'highlight-symbol)
(global-set-key [f9] 'highlight-symbol-next)
(global-set-key [(shift f9)] 'highlight-symbol-prev)
(global-set-key [(meta f9)] 'highlight-symbol-query-replace)

(window-numbering-mode 1)

;; (require 'highlight-parentheses)
;; (define-globalized-minor-mode global-highlight-parentheses-mode
;;   highlight-parentheses-mode
;;   (lambda ()
;;     (highlight-parentheses-mode t)))

;; (global-highlight-parentheses-mode t)


(require 'shell-pop)

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


(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

(slime-setup '(slime-company))


(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(add-hook 'lisp-mode-hook (lambda ()
			    (eldoc-mode)))


(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "C-c f g") 'grep-find)
(global-set-key (kbd "C-c r f") 'recentf-open-files)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(column-number-mode t)
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(tool-bar-mode nil))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(recentf-mode t)
(recentf-open-files)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "unknown" :slant normal :weight normal :height 120 :width normal))))
 '(show-paren-match ((t (:background "powder blue")))))


;Функция для файлов .fb2 в режиме просмотра
 (defun fb2-mode-view()
     (vc-toggle-read-only)
     (interactive)
     (sgml-mode)
     (sgml-tags-invisible 0))

;Функция для файлов .fb2 в режиме редактирования
(defun fb2-mode-edit()
     (vc-toggle-read-only nil)
     (interactive)
     (sgml-mode)
     (sgml-tags-invisible 0))


;Авто определение формата по расширению файла
(add-to-list 'auto-mode-alist '(".fb2$" . fb2-mode-view))

(show-paren-mode 1)
;; (setq show-paren-style 'parenthesis)
(setq show-paren-style 'expression)
;; (setq show-paren-style 'mixed)
