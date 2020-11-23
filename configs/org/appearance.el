;;; Package --- Summary
;;; Commentary:
;;; Code:
;; reduce flycheck errors
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in org mode
(eval-when-compile (defvar org-src-fontify-natively)) ; defined org mode

;; enable bullets in org-mode instead of asterisks
(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path
		 (concat sk:emacs-plugin-path (symbol-name 'org-bullets)))
  (message "plugin-path not defined") ;else
    )
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook 'variable-pitch-mode)
;; turn on syntax highlighting for code blocks
(setq org-src-fontify-natively t)

;; org mode heading fonts and sizes
;; from org-mode-beautify and http://howardism.org/Technical/Emacs/orgmode-wordprocessor.html
(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (variable-fixed (cond ((x-list-fonts "Source Code Variable") '(:font "Source Code Variable"))
			     ((x-list-fonts "Source Code Pro") '(:font "Source Code Pro"))
			     (nil (warn "Cannot find a good mono font, install Source Code Variable or Pro"))))
       ;; This removes heading coloring
       ;;       (base-font-color     (face-foreground 'default nil 'default))
       (headline `(:inherit default :weight semi-bold ))
       (midline `(:inherit default :weight normal))
       (notheadline `(:inherit default)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@midline ,@variable-tuple))))
                          `(org-level-7 ((t (,@midline ,@variable-tuple))))
                          `(org-level-6 ((t (,@midline ,@variable-tuple))))
                          `(org-level-5 ((t (,@midline ,@variable-tuple))))
                          `(org-level-4 ((t (,@midline ,@variable-tuple :height 1.0))))
                          `(org-level-3 ((t (,@midline ,@variable-tuple :height 1.0))))
                          `(org-level-2 ((t (,@midline ,@variable-tuple :height 1.0 :width normal))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.0))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.1 :underline nil))))
			  `(org-block ((t (,@notheadline ,@variable-fixed))))
			  `(org-table ((t (,@notheadline ,@variable-fixed))))
			  `(org-indent ((t (,@notheadline ,@variable-fixed))))
			  `(org-tag ((t (,@notheadline ,@variable-fixed))))
			  `(org-verbatim ((t (,@notheadline ,@variable-fixed))))
			  `(org-code ((t (,@notheadline ,@variable-fixed))))))



;;; appearance.el ends here
