;;; Package --- Summary

;;; This is the initialization file for my autopair configuration

;;; Commentary:

;; https://github.com/porterjamesj/virtualenvwrapper.el

;;  Use M-x venv-workon to activate virtualenvs and M-x venv-deactivate deactivate them.

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'virtualenvwrapper)

(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path (concat sk:emacs-plugin-path (symbol-name sk:current-plugin)))
  (message "plugin-path not defined") ;else
    )
;; require the module
(require sk:current-plugin)


(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place
(setq venv-location "~/venvs/")

(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))
;;; virtualenvwrapper.el ends here
