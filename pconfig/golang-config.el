;;; Package --- Summary

;;; This package provides an environment for programming
;;; in golang.  https://golang.org/

;;; Commentary:
;;;   This sets up personal configuration for the use of the
;;;   go-mode package.

;;; Compatibility:  Emacs 27 or newer.  Older versions may work.

;;; Code:
(use-package go-mode
  ;; bind keys for this mode
  :bind (("C-c C-c" . compile))
  :hydra (hydra-go-cmds (:hint nil))
  ;; init executes before a package is loaded
  :init
  ;; (ac-config-default)
  ;; (require 'auto-complete-config)
  ;; (require 'go-autocomplete)
  (require 'go-rename)
  ;; config executes after a package is loaded
  :config
  (add-hook 'go-mode-hook (lambda() (setq tab-width 2)))
  (setq compile-command "echo Building... &&\
                         go build -v &&\
                         echo Testing... &&\
                         go test -v &&\
                         echo Linter... && golint")
  (setq compilation-read-command nil)
  (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/golang.org/x/lint/misc/emacs/"))
  (require 'golint);;Smaller compilation buffer
  (setq compilation-window-height 14)
  (defun my-compilation-hook ()
    (when (not (get-buffer-window "*compilation*"))
      (save-selected-window
	(save-excursion
          (let* ((w (split-window-vertically))
		 (h (window-height w)))
            (select-window w)
            (switch-to-buffer "*compilation*")
            (shrink-window (- h compilation-window-height)))))))
  (add-hook 'compilation-mode-hook 'my-compilation-hook)

  )

;; (use-package lsp-mode
;;   :ensure t
;;   :commands (lsp lsp-deferred)
;;   :hook (go-mode . lsp-deferred)
;;   :init
;;   (lsp-register-custom-settings
;;    '(("gopls.completeUnimported" nil nil)
;;      ("gopls.staticcheck" t t)
;;      ("gopls.completionDocumentation" t t)))
;;   )

;; (defun lsp-go-install-save-hooks ()
;;   "Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks
;; enabled."
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; ;; Optional - provides fancier overlays.
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; ;; Company mode is a standard completion package that works well with lsp-mode.
;; (use-package company
;;   :ensure t
;;   :config
;;   ;; Optionally enable completion-as-you-type behavior.
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 1))

;; ;; company-lsp integrates company mode completion with lsp-mode.
;; ;; completion-at-point also works out of the box but doesn't support snippets.
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))
(provide 'golang-config)
;;; golang-config.el ends here
