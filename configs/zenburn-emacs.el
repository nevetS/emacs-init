;;; Package --- Summary

;;; This is the initialization file for my zenburn-emacs configuration

;;; Commentary:

;; Compatibility: Emacs 24.x or newer.  May work in older versions, may not

;; https://github.com/jonnay/org-beautify-theme

;; In Emacs 24.4 or newer, it's recommended to use electric-pair-mode instead

;;; Code:
(message "I got here buddy")
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'zenburn-emacs)
(message "I got here buddy")
(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'custom-theme-load-path (concat sk:emacs-plugin-path (symbol-name sk:current-plugin)))
  (message "plugin-path not defined") ;else
    )



;; require the module
;(require sk:current-plugin)

(load-theme 'zenburn t)


;;; org-beautify-theme.el ends here


