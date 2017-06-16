;;; Package --- Summary

;;; This is the initialization file for my yasnippet configuration

;;; Commentary:

;YASNIPPET
; Important keys and commands:
; - yas-mode
; - TAB (to insert a snippet)

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el
(eval-when-compile (defvar yas-snippet-dirs)) ; defined in ~/yasnippet.el
(eval-when-compile (defvar yas-global-mode)) ; defined in ~/yasnippet.el

(setq sk:current-plugin 'yasnippet)

(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path (concat sk:emacs-plugin-path (symbol-name sk:current-plugin)))
  (message "plugin-path not defined") ;else
    )

;; require the module if cl-lib is available
(when (require 'cl-lib nil ':noerror)
  (require sk:current-plugin)
;    (setq yas/triggers-in-field 'True)
;    (yas/initialize)
;    (yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")
(setq yas-snippet-dirs
      '("~/.yas/snippets/collections/crotti"
	"~/.yas/snippets/collections/personal"
	))
(yas-global-mode 1)
)


;;; yasnippet.el ends here
