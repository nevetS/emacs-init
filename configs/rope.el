;;; Package --- Summary

;;; This is the initialization file for my ropemacs configuration

;;; Commentary:

;; Installation instructions are in ../install/pymacs-rope

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(add-to-list 'load-path (concat sk:emacs-plugin-path "pymacs"))

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

;; the following section is for ropemode
;; in case rope mode f's up my keybindings
(setq ropemacs-enable-shortcust nil)
(setq ropemacs-local-prefix "C-c C-p")

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;;; rope.el ends here
