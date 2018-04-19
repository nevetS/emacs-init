;;; Package --- Summary

;;; This is the initialization file for my flycheck-yamllint

;;; Commentary:

;; Compatibility: Emacs 24.x or newer.  May work in older versions, may not

;; https://github.com/krzysztof-magosa/flycheck-yamllint

;; In Emacs 24.4 or newer, it's recommended to use electric-pair-mode instead

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'flycheck-yamllint)

;; require the module
(require sk:current-plugin)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))
;;; flycheck-yamllint.el ends here
