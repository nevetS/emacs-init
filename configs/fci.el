;;; Package --- Summary

;;; This is the initialization file for my fill-column-indicator configuration

;;; Commentary:

;; Compatibility: Emacs 24.x or newer.  May work in older versions, may not


; Important keys and commands:
; - describe-fill-column-indicator-keys
;   - key complexity should be at least 5


;; In Emacs 24.4 or newer, it's recommended to use electric-pair-mode instead

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'fill-column-indicator)

(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path (concat sk:emacs-plugin-path "fci"))
  (message "plugin-path not defined") ;else
    )

;; turn on autofill in text-mode and python-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'python-mode-hook 'turn-on-auto-fill)
;; default fill-column is 79
(setq-default fill-column 79)

;; require the module
(require sk:current-plugin)

;; set width of indicator and color
(setq fci-rule-width 1)
(setq fci-rule-color "#7f9f7f")
(add-hook 'after-change-major-mode-hook 'fci-mode)

;;; fill-column-indicator.el ends here
