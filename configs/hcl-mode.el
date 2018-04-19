;;; Package --- Summary

;;; This is the initialization file for my hcl-mode configuration

;;; Commentary:

;; Compatibility: Emacs 24.x or newer.  May work in older versions, may not

;; https://github.com/joaotavora/autopair

;; In Emacs 24.4 or newer, it's recommended to use electric-pair-mode instead

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'hcl-mode)



;; (if (boundp 'sk:emacs-plugin-path)
;;     (add-to-list 'load-path (concat sk:emacs-plugin-path (symbol-name sk:current-plugin)))
;;   (message "plugin-path not defined") ;else
;;     )
;; require the module
(require sk:current-plugin)


;; auto-activate hcl-mode on files named "*.tf"
(add-to-list 'auto-mode-alist '("\\.tf\\'" . hcl-mode))
;; turn on auto-fill mode along with hcl-mode
(add-hook 'hcl-mode-hook 'turn-on-auto-fill)
;;; hcl-mode.el ends here
