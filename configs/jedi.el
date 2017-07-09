;;; Package --- Summary

;;; This is the initialization file for my js2-mode configuration

;;; Commentary:


;;; Code:

;JEDI AUTO-COMPLETIONS
(setq jedi:complete-on-dot t)
(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:ac-setup)
;(setq jedi:complete-on-dot t)
;;; jedi.el ends here
