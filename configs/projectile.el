;;; Package --- Summary

;;; This is the initialization file for my projectile configuration

;;; Commentary:

;; url

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

(setq sk:current-plugin 'projectile)
;; require the module
(require sk:current-plugin)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-tags-command "~/bin/etags_python")
(defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

(defun sk/find-rn-test-or-source-files (path)
  (if (string-match (rx (group (or "src" "__tests__")) (group "/" (1+ anything) ".js")) path)
      (let ((dir (match-string 1 path))
            (file-name (match-string 2 path)))
        (if (equal dir "__tests__")
            (list :impl (concat "src" file-name))
          (list :test (concat "__tests__" file-name)
                :other (concat "src" file-name ".def"))))))

(with-eval-after-load "projectile"
  (projectile-register-project-type 'reactnative '("index.android.js")
                  :compile "npm install"
                  :test "npm test"
                  :run "npm start"
                  :test-suffix ""
		  :test-prefix ""
		  :related-files-fn #'sk/find-rn-test-or-source-files))


(counsel-projectile-mode)
(global-set-key (kbd "C-\\") 'projectile-toggle-between-implementation-and-test)
;;; 3.el ends here
