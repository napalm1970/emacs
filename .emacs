;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code

(load "~/.emacs.d/init_install_package")
(load "~/.emacs.d/global_settings")


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

(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun vi-open-line (&optional abovep)
  "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
  (interactive "P")
  (if abovep
      (vi-open-line-above)
    (vi-open-line-below)))

(global-set-key (kbd "C-S-o") 'vi-open-line-above)
(global-set-key (kbd "C-o") 'vi-open-line-below)


(defun my-mark-current-word (&optional arg allow-extend)
    "Put point at beginning of current word, set mark at end."
    (interactive "p\np")
    (setq arg (if arg arg 1))
    (if (and allow-extend
             (or (and (eq last-command this-command) (mark t))
                 (region-active-p)))
        (set-mark
         (save-excursion
           (when (< (mark) (point))
             (setq arg (- arg)))
           (goto-char (mark))
           (forward-word arg)
           (point)))
      (let ((wbounds (bounds-of-thing-at-point 'word)))
        (unless (consp wbounds)
          (error "No word at point"))
        (if (>= arg 0)
            (goto-char (car wbounds))
          (goto-char (cdr wbounds)))
        (push-mark (save-excursion
                     (forward-word arg)
                     (point)))
        (activate-mark))))

(defun xah-select-current-line ()
  "Select current line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-02-07
"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))


(defun xah-select-current-block ()
  "Select the current block of text between blank lines.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-02-07
"
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))
    (set-mark p1)))

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command does not properly deal with nested brackets.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-05-16"
  (interactive)
  (let (ξp1
        ξp2
        (ξskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
    (skip-chars-backward ξskipChars)
    (setq ξp1 (point))
    (skip-chars-forward ξskipChars)
    (setq ξp2 (point))
    (set-mark ξp1)))

(defun xah-semnav-up (φarg)
"Called by `xah-extend-selection'.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-11-13.
Written by Nikolaj Schumacher, 2008-10-20. Released under GPL 2"
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> φarg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (setq φarg (1- φarg) ))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (setq φarg (1+ φarg) )))
  (up-list φarg))

(defun xah-extend-selection (φarg &optional φincremental-p)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit.

This command works mostly in lisp syntax.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-11-13.
Written by Nikolaj Schumacher, 2008-10-20. Released under GPL 2."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (or (use-region-p)
             (eq last-command this-command))))
  (if φincremental-p
      (progn
        (xah-semnav-up (- φarg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> φarg 1)
        (xah-extend-selection (1- φarg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))


(global-set-key (kbd "C-M-5") 'xah-extend-selection)
(global-set-key (kbd "C-M-6") 'xah-select-current-line)
(global-set-key (kbd "C-M-7") 'xah-select-current-block)
(global-set-key (kbd "M-\"") 'xah-select-text-in-quote)


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
	(add-to-list 'company-backends 'company-c-headers)
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
	(add-to-list 'company-backends 'company-c-headers)
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


(require 'semantic/bovine/c)
(add-to-list 'semantic-lex-c-preprocessor-symbol-file
    "/usr/lib/gcc/x86_64-linux-gnu/5/include/stddef.h")

(global-flycheck-mode t)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))


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

(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))

(global-highlight-parentheses-mode t)


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
 '(default ((t (:family "Iosevka" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))
