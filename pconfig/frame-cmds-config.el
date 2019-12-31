;;; Package --- Summary

;;; This package provides configuration for frame-cmds

;;; Commentary:
;;;    This sets up personal configuration for the use of the
;;;    frame-cmds package.

;;; Compatibility: Emacs 27 or newer.  Older versions may work.

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in init.el
"sk:emacs-plugin-path is the folder where repo based packages are found"

(if (boundp 'sk:emacs-plugin-path)
  (add-to-list 'load-path (concat sk:emacs-plugin-path "frame-cmds"))
  (message (concat "frame-cmds" " plugin-path not defined")))

;; documentation for use-package: https://github.com/jwiegley/use-package
(use-package frame-cmds
  ;; bind keys for this mode
  :bind (("C-c w" . hydra-frame-cmds/body))
  ;; define a hydra without "defhydra"
  :hydra (hydra-frame-cmds (:hint nil)
  ("s" sk/single-wide "single" :color blue)
  ("d" sk/double-wide "double" :color blue)
  ("t" sk/triple-wide "triple" :color blue)
  )
  ;; init executes before a package is loaded

  :init
  
  ;; config executes after a package is loaded
  :config
  (defun sk/single-wide ()
    "sets the frame width and moves the position to the far left"
    (interactive)
    (set-frame-width (selected-frame) 82)
    (set-frame-height (selected-frame) 56)
    (set-frame-position (selected-frame) 0 0))
  (defun sk/double-wide ()
    "sets the frame width for 2 windows wide and moves the position to the far left"
    (interactive)
    (set-frame-width (selected-frame) 164)
    (set-frame-height (selected-frame) 56)
    (set-frame-position (selected-frame) 0 0))
  (defun sk/triple-wide ()
    "sets the frame width for 3 windows wide and moves the position to the far left"
    (interactive)
    (set-frame-width (selected-frame) 246)
    (set-frame-height (selected-frame) 56)
    (set-frame-position (selected-frame) 0 0)))
(provide 'frame-cmds-config)
;;; frame-cmds-config.el ends here










































