;; refrence: http://doc.norang.ca/org-mode.html

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of 'truncate-lines' and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))



(defun endless/insert-key (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((orgp (derived-mode-p 'org-mode))
         (tag (if orgp "~%s~" "<kbd>%s</kbd>")))
    (if (null (equal key "\C-m"))
        (insert 
         (format tag (help-key-description key nil)))
      ;; If you just hit RET.
      (insert (format tag ""))
      (forward-char (if orgp -1 -6)))))

(add-hook 'org-mode-hook
	  (lambda()
	    (define-key org-mode-map "\C-ck" 'endless/insert-key)))
