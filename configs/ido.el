;;; Package --- Summary

;;; This is the initialization file for my autopair configuration

;;; Commentary:

;; https://gitlab.com/python-mode-devs/python-mode

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'ido)

;; require the module
(require sk:current-plugin)
(ido-mode t)
;;; ido.el ends here
