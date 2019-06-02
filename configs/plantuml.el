;;; Package --- Summary

;;; This is the initialization file for my plantuml-mode configuration

;;; Commentary:

;; This enables UML diagrams from within Emacs

;; Compatibility: Emacs 24.x or newer.  May work in older versions, may not

;; https://github.com/skuro/plantuml-mode
;; http://plantuml.com/

;; In Emacs 24.4 or newer, it's recommended to use electric-pair-mode instead

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'plantuml-mode)
;; require the module
(if (boundp 'sk:current-plugin)
    (require sk:current-plugin))



(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;;; plantuml.el ends here
