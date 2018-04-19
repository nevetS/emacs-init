;;; Package --- Summary

;;; This is the initialization file for my sqlup-mode configuration

;;; Commentary:

;; Compatibility: Emacs 24.x or newer.  May work in older versions, may not

;; https://github.com/trevoke/sqlup-mode.el (though in my repo, it's installed via package)

;; In Emacs 24.4 or newer, it's recommended to use electric-pair-mode instead

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'sqlup-mode)

;; require the module
(require sk:current-plugin)

;; Capitalize keywords in SQL mode
(add-hook 'sql-mode-hook 'sqlup-mode)
;; Capitalize keywords in an interactive session (e.g. psql)
;;(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
;; Set a global keyword to use sqlup on a region
;;(global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)
;;; sqlup-mode.el ends here
