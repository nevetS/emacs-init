;;; Package --- Summary

;;; This is the initialization file for my fill-column-indicator configuration

;;; Commentary:



;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'fill-column-indicator)

(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path (concat sk:emacs-plugin-path "fci"))
  (message "plugin-path not defined") ;else
    )

;; turn on autofill in text-mode and python-mode
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'python-mode-hook 'turn-on-auto-fill)
;; default fill-column is 79
(setq-default fill-column 79)

;; from https://emacs.stackexchange.com/questions/147/how-can-i-get-a-ruler-at-column-80/155#155
;; suppress fci when pop-ups are displayed
;; to prevent garbling on python completion suggestions
(defun sanityinc/fci-enabled-p ()
  "Seems like wrapping symbols for no good reason, but what do I know.  It works."
  (symbol-value 'fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible."
    (let ((fci-enabled (sanityinc/fci-enabled-p)))
      (when fci-enabled
        (setq sanityinc/fci-mode-suppressed fci-enabled)
        (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed."
    (when (and sanityinc/fci-mode-suppressed
               (null popup-instances))
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

;; require the module
(require sk:current-plugin)

;; set width of indicator and color
(setq fci-rule-width 1)
(setq fci-rule-color "#7f9f7f")
;;(add-hook 'after-change-major-mode-hook 'fci-mode)

;;; fci.el ends here
