;;; Package --- Summary

;;; This is the initialization file for my highlight-parentheses-mode configuration

;;; Commentary:



;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'highlight-parentheses)

(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path (concat sk:emacs-plugin-path (symbol-name sk:current-plugin)))
  (message "plugin-path not defined") ;else
    )


(if (boundp 'sk:emacs-plugin-path)

    (add-to-list 'load-path (concat sk:emacs-plugin-path (symbol-name sk:current-plugin))
  (message "plugin-path not defined") ;else
    )
)
;; require the module
(require sk:current-plugin)

;;integrate highlight-parentheses with autopair mode
;; from https://www.emacswiki.org/emacs/HighlightParentheses
(add-hook 'highlight-parentheses-mode-hook
          '(lambda ()
             (setq autopair-handle-action-fns
                   (append
					(if autopair-handle-action-fns
						autopair-handle-action-fns
					  '(autopair-default-handle-action))
					'((lambda (action pair pos-before)
						(hl-paren-color-update)))))))


;; enables highlight-parentheses-mode on all buffers
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;; highlight-parentheses-mode.el ends here
