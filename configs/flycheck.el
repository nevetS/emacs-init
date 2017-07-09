;;; Package --- Summary

;;; This is the initialization file for my flycheck configuration

;;; Commentary:

;; 

;;; Code:
;; TODO - declare variables and only run this if flycheck is installed


;FLYCHECK-GLOBAL-MODE
(add-hook 'after-init-hook 'global-flycheck-mode)

;FLYCHECK keymap prefix conflicts with Python Mode
; change C-c ! to C-c @
(defun change_flycheckprefix()
 (define-key flycheck-mode-map flycheck-keymap-prefix nil)
; (setq flycheck-keymap-prefix (kbd "C-c @"))
 (define-key flycheck-mode-map flycheck-keymap-prefix
                flycheck-command-map)
)

(eval-after-load "flycheck"
    '(progn
        (defun my-flycheck-mode-hook()
            (define-key flycheck-mode-map flycheck-keymap-prefix nil)
            (setq flycheck-keymap-prefix (kbd "C-c @"))
            (define-key flycheck-mode-map flycheck-keymap-prefix
                flycheck-command-map)
        )
        (add-hook 'flycheck-mode-hook 'my-flycheck-mode-hook)
     )
)


;;; flycheck.el ends here
