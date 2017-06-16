;;; Package --- Summary

;;; This is the initialization file for my autopair configuration

;;; Commentary:

;; https://gitlab.com/python-mode-devs/python-mode

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'python-mode.el)

(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path (concat sk:emacs-plugin-path (symbol-name sk:current-plugin)))
  (message "plugin-path not defined") ;else
    )
;; require the module
(require 'python-mode)

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq-default py-python-command "/usr/local/bin/ipython")
;; (setq-default py-python-command-args '("--simple-prompt" "-i"))
;; (setq py-python-command-args '("--simple-prompt"))

; use the wx backend, for both mayavi and matplotlib
 (setq py-python-command-args
   '( "--simple-prompt" "--gui=wx" "--pylab=wx" "--colors" "Linux" ))
(setq py-force-py-shell-name-p t)
; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p nil)
(setq py-switch-buffers-on-execute-p nil)
; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)

;;; python-mode.el.el ends here
