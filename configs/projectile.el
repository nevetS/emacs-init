;;; Package --- Summary

;;; This is the initialization file for my projectile configuration

;;; Commentary:

;; url

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

;; require the module
(require sk:current-plugin)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-tags-command "~/bin/etags_python")
(counsel-projectile-mode)

;;; 3.el ends here
