;;; Package --- Summary

;;; This is the initialization file for my window-number configuration

;;; Commentary:


;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'window-number)

(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path (concat sk:emacs-plugin-path (symbol-name sk:current-plugin)))
  (message "plugin-path not defined") ;else
    )

;; require the module
(require sk:current-plugin)
;turn it on
(window-number-mode)
 (defhydra hydra-splitter ()
    "splitter"
    ("h" hydra-move-splitter-left)
    ("j" hydra-move-splitter-down)
    ("k" hydra-move-splitter-up)
    ("l" hydra-move-splitter-right)
    ("q" nil :color blue))
(global-set-key (kbd "C-c s") 'hydra-splitter/body)
;;; window-number.el ends here
