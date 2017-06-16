;;; Package --- Summary

;;; This is the initialization file for my autopair configuration

;;; Commentary:

;; https://github.com/magnars/s.el


;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 's)

(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path (concat sk:emacs-plugin-path (symbol-name sk:current-plugin)))
  (message "plugin-path not defined") ;else
    )
;; require the module
(require sk:current-plugin)



;;; s.el ends here
