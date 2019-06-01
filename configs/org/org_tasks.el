;;; Package --- Summary
;;; Commentary:
;;; Code:
;; reduce flycheck errors
(eval-when-compile (defvar org-todo-keywords)) ; defined in org mode
(eval-when-compile (defvar org-todo-state-tags-triggers)) ; defined org mode
(eval-when-compile (defvar org-log-into-drawer)) ; defined by source

;; add states for tasks
;; http://orgmode.org/manual/Workflow-states.html
(setq org-todo-keywords
      (quote ((sequence "TODO(t!)"
			"IN PROGRESS(i!)"
			"|"
			"DONE(d!)")
	      (sequence "WAITING(w@/!)"
			"|"
			"CANCELLED(c@/!)"
			"PHONE(p!)"
			"MEETING(m!)"))))




;; Tag changes that should be triggered by TODO state changes.
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              (done ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("NEXT" ("WAITING") ("CANCELLED"))
              ("DONE" ("WAITING") ("CANCELLED")))))

(provide 'org_tasks)
;;; org_tasks.el ends here
