;add latex to PATH and exec-path
(setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
(setq exec-path (append '("/Library/TeX/texbin") exec-path))
; custom lisp files for org mode will go here
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/org-mode/lisp"))
(setq org-agenda-files (quote ("~/Documents/tasks/")))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
(setq org-directory "~/Documents/tasks")
(setq org-default-notes-file "~/Documents/tasks/refile.org")
(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?C)
