;;; Package --- Summary

;;; This file sets configuration variables for org-mode

;;; Commentary:

;;; Compatibility: Emacs 26.2 or newer.  May work in older versions, may not

;;; Code:

;; declare variables defined elsewhere to avoid warnings during lint.
;;  org-mode is expected to be loaded before anything in this file is evaluated
(eval-when-compile (defvar org-agenda-files)) ; defined by org-mode
(eval-when-compile (defvar org-agenda-log-mode-items)) ; defined by org-mode
(eval-when-compile (defvar org-confirm-babel-evaluate)) ; defined by org-mode
(eval-when-compile (defvar org-default-notes-file)) ; defined by org-mode
(eval-when-compile (defvar org-default-priority)) ; defined by org-mode
(eval-when-compile (defvar org-directory)) ; defined by org-mode
(eval-when-compile (defvar org-ditaa-jar-path)) ; defined by org-mode
(eval-when-compile (defvar org-file-apps)) ; defined by org-mode
(eval-when-compile (defvar org-highest-priority)) ; defined by org-mode
(eval-when-compile (defvar org-log-into-drawer)) ; defined by org-mode
(eval-when-compile (defvar org-lowest-priority)) ; defined by org-mode
(eval-when-compile (defvar org-refile-targets)) ; defined by org-mode
(eval-when-compile (defvar org-return-follows-link)) ; defined by org-mode
(eval-when-compile (defvar org-use-speed-commands)) ; defined by org-mode

;; add latex to PATH and exec-path
;; TODO make this OS specific
(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
(setq exec-path (append '("/Library/TeX/texbin") exec-path))


;; INTERFACE related configuration
;; file types that trigger org-mode
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
;; When a capture template specifies a target file that is not an
;;   absolute path.  The path will then be ‘org-directory’.
;; Also relative paths in the ‘org-agenda-files’ are relative to ‘org-directory’
(setq org-directory "~/Documents/tasks")
(setq org-log-into-drawer t) ; timestamps go into LOG drawer
(setq org-return-follows-link t) ; enter or click follows links
(setq org-use-speed-commands t) ; C- and M- free hotkeys before tasks

;; turn on auto-fill mode by default in org files
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; AGENDA related configuration
;; show closed tasks in agenda view
(setq org-agenda-log-mode-items (quote (closed state)))

;; files that are used for building agenda
(setq org-agenda-files (quote ("~/Documents/tasks/")))

;; TASKS related configuration
(setq org-highest-priority ?A) ; priority settings
(setq org-lowest-priority ?E)
(setq org-default-priority ?C)

;; when capturing and not refiling tasks, place them in this file.
(setq org-default-notes-file "~/Documents/tasks/refile.org")
;; use these files, upto maxlevel deep for refiling tasks.
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

;; BABEL related configuration
;; location of ditaa.jar
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
(setq org-confirm-babel-evaluate t) ; confirm before executing src blocks

;; open in
(setq org-file-apps '((auto-mode . emacs)
		     ("\\.mm\\'" . default)
		     ("\\.x?html?\\'" . "firefox %s")
		     ("\\.pdf\\'" . default)))
(provide 'org_config_variables)
;;; org_config_variables.el ends here
