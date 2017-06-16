;;; Package --- Summary

;;; This is the initialization file for my autopair configuration

;;; Commentary:

;; https://gitlab.com/python-mode-devs/python-mode

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'expand-region)

(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path (concat sk:emacs-plugin-path (symbol-name sk:current-plugin)))
  (message "plugin-path not defined") ;else
    )
;; require the module
(require sk:current-plugin)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-=") 'er/contract-region)

;;; expand-region.el ends here
