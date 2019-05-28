;;; Package --- Summary

;;; This file sets configuration variables for org-mode

;;; Commentary:

;; Compatibility: Emacs 26.2 or newer.  May work in older versions, may not

;;; Code:

;; declare variables defined elsewhere to avoid warnings during lint.
;;  org-mode is expected to be loaded before anything in this file is evaluated
(eval-when-compile (defvar org-directory)) ; defined by org-mode
(eval-when-compile (defvar org-highest-priority)) ; defined by org-mode
(eval-when-compile (defvar org-lowest-priority)) ; defined by org-mode
(eval-when-compile (defvar org-default-priority)) ; defined by org-mode
(eval-when-compile (defvar org-use-speed-commands)) ; defined by org-mode
(eval-when-compile (defvar org-return-follows-link)) ; defined by org-mode
(eval-when-compile (defvar org-agenda-log-mode-items)) ; defined by org-mode
(eval-when-compile (defvar org-agenda-files)) ; defined by org-mode
(eval-when-compile (defvar org-refile-targets)) ; defined by org-mode
(eval-when-compile (defvar org-default-notes-file)) ; defined by org-mode

;add latex to PATH and exec-path
(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
(setq exec-path (append '("/Library/TeX/texbin") exec-path))
; custom lisp files for org mode will go here
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/org-mode/lisp"))


(setq org-directory "~/Documents/tasks")

(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?C)
(setq org-use-speed-commands t)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; AGENDA related configuration

; show closed tasks in agenda view
(setq org-agenda-log-mode-items (quote (closed state)))
(setq org-agenda-files (quote ("~/Documents/tasks/")))

;; TASKS related configuration

;; use these files, upto maxlevel deep for refiling tasks.
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
;; when capturing and not refiling tasks, place them in this file.
(setq org-default-notes-file "~/Documents/tasks/refile.org")
(provide 'org_config_variables)
;org_config_variables.el ends here
