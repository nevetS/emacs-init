;;; Package --- Summary

;;; This is the initialization file for my spaceline-all-the-icons configuration

;;; Commentary:

;; https://github.com/domtronn/spaceline-all-the-icons.el

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

;; require the module
(require 'spaceline)
(require 'all-the-icons)
(require sk:current-plugin)
(spaceline-all-the-icons-theme)


;;; spaceline-all-the-icons.el ends here
