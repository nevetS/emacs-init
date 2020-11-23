;;; Package --- Summary

;;; This is the initialization file for my markdown-mode configuration

;;; Commentary:


;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el
(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path (concat sk:emacs-plugin-path (symbol-name sk:current-plugin)))
  (message "plugin-path not defined") ;else
    )
;; from: https://prathamesh.tech/2019/06/21/creating-pull-requests-from-emacs/
;; original
(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

;;; modified for gitlab
(defun endless/visit-pull-request-url-gitlab ()
  "Visit the current branch's PR on Gitlab."
  (interactive)
  (browse-url
   (format "https://gitlab.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+gitlab\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))
;;; modified for bitbucket
(defun endless/visit-pull-request-url-bitbucket ()
  "Visit the current branch's PR on Gitlab."
  (interactive)
  (browse-url
   (format "https://bitbucket.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+gitlab\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "h"
     #'endless/visit-pull-request-url-bitbucket))

(eval-after-load 'magit
  '(define-key magit-mode-map "v"
     #'endless/visit-pull-request-url))


;;; markdown-mode.el ends here
