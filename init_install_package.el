(setq cfg-var:packages
  '(ace-jump-mode
    company
    company-anaconda
    company-go
    flycheck
    flycheck-pos-tip
   ;; go-mode
    helm
    helm-gtags
    helm-themes
    helm-go-package
    helm-swoop
    magit
    smartparens
   ;; ido-vertical-mode
    ;; ido-ubiquitous
    ido-yes-or-no
    ;; lua-mode
    shell-pop
    smex
    window-numbering
    yasnippet
    function-args
    highlight-symbol
    highlight-parentheses
    neotree
    ;;undo-tree
    nlinum
   ;; powerline
    ))

(defun cfg:install-packages ()
  (let ((pkgs (remove-if #'package-installed-p cfg-var:packages)))
    (when pkgs
      (message "%s" "Emacs is now refreshing its package database...")
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (p cfg-var:packages)
        (package-install p)))))


(require 'package)
(require 'cl)


(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)
(cfg:install-packages)

(add-to-list 'load-path "/home/napalm/.emacs.d/lisp")
