;;; Package --- Summary
;;; Commentary:
;;; Code:
;; reduce flycheck errors
(eval-when-compile (defvar org-todo-keywords)) ; defined in org mode
(eval-when-compile (defvar org-todo-state-tags-triggers)) ; defined org mode


;; add states for tasks
;; States are:
;;  TODO | IN PROGRESS
;;  DONE
;;  - or
;;  WAITING | HOLD
;;  CANCELLED | PHONE | MEETING
;; http://orgmode.org/manual/Workflow-states.html
(setq org-todo-keywords
      (quote ((sequence "TODO(t)"
			"IN PROGRESS(i!)"
			"|"
			"DONE(d!)")
	      (sequence "WAITING(w@/!)"
			"HOLD(h@/!)"
			"|"
			"CANCELLED(c@/!)"
			"PHONE"
			"MEETING"))))

;; Tag changes that should be triggered by TODO state changes.
;; org-todo-state-tags-triggers is a list.  Each entry is

;;   (state-change (tag . flag) .......)

;; State-change can be a string with a state, and empty string to indicate the
;; state that has no TODO keyword, or it can be one of the symbols `todo'
;; or `done', meaning any not-done or done state, respectively.

;; TODO - experiment with these to make sure it works right
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;;; tasks.el ends here
