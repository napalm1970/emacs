(require 'semantic/bovine/c)
(add-to-list 'semantic-lex-c-preprocessor-symbol-file
    "/usr/lib/gcc/x86_64-linux-gnu/5/include/stddef.h")


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


(defun my-c++-mode-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert))


(add-hook 'c++-mode-common-hook 'my-c++-mode-cedet-hook)
