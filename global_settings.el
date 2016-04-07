(defun system-is-linux ()
  (string-equal system-type "gnu/linux")
  )

(when (system-is-linux)
  (require 'server)
  (unless (server-running-p)
    (server-start)))


(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'manoj-dark t)
;; (load-theme 'tsdh-dark t)
;; (load-theme 'deeper-blue t)
(load-theme 'dichromacy t)

(require 'smartparens-config) 
(smartparens-global-mode t)

(setq inhibit-splash-screen t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;; Delete selection
(delete-selection-mode t)
(tooltip-mode -1)
(setq use-dialog-box nil)

(require 'ido-yes-or-no)
(ido-yes-or-no-mode 1)

(global-hl-line-mode 1)


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

(recentf-mode t)
(recentf-open-files)

(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(global-ede-mode t)
