;;; Package --- Summary

;;; This is the initialization file for my js2-mode configuration

;;; Commentary:


;;; Code:

;JEDI AUTO-COMPLETIONS
(setq jedi:complete-on-dot t)
(setq jedi:setup-keys t)
;; alternatively jedi:ac-setup for only autocompletion
(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)
;;; jedi.el ends here
