;;; Package --- Summary

;;; This is the initialization file for my js2-mode configuration

;;; Commentary:


;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'js2-mode)

(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path (concat sk:emacs-plugin-path (symbol-name sk:current-plugin)))
  (message "plugin-path not defined") ;else
    )


;; require the module if cl-lib is available
(when (require 'cl-lib nil ':noerror)
  (require sk:current-plugin)
  (eval-after-load "js2-mode"
    (setq js2-mirror-mode nil)
    )

  ;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
  ;; add any symbols to a buffer-local var of acceptable global vars
  ;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
  ;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
  ;; you can;t have a symbol called "someName:false"
  (add-hook 'js2-post-parse-callbacks
	    (lambda ()
	      (when (> (buffer-size) 0)
		(let ((btext (replace-regexp-in-string
			      ": *true" " "
			      (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
		  (mapc (apply-partially 'add-to-list 'js2-additional-externs)
			(split-string
			 (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
			 " *, *" t))
		  ))))

  ;; get rid of annoying node errors
  (setq js2-include-node-externs t)
  (setq js2-bounce-indent-p t)
)


;;; js2-mode.el ends here
