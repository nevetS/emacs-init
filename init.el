;;; Package --- Steve's Emacs Initialization
;;
;;; Commentary:
;;     Steve's Emacs configuration, modularized to take the bite out of an
;;     entirely too large init.el



;;; Code:

;; default xemacs configuration directory
(defconst sk:emacs-config-dir "~/.emacs.d/configs/" "")

;; utility function to auto-load my package configurations
(defun sk:load-config-file (filelist)
    (dolist (file filelist)
      (load (expand-file-name
             (concat sk:emacs-config-dir file)))
       (message "Loaded config file:%s" file)
       ))

(sk:load-config-file '(;;base
		       "yasnippet"   ;provides templated completions
		       "color-theme" ;change default color-scheme
		       "unbound"     ;find unbound keys
		       "autopair"    ;close parens/brackets/etc
		       "fci"         ;fill column indicator
		       "linum"       ;line number mode
		       "window-number" ; window numbers
		       "packages"    ;set up package archives
		       ;;
		       "markdown"    ;markdown mode
		       "graphviz"    ;graphviz mode
		       "docker"      ;docker mode
		       
		       ;;python
		       "virtualenvwrapper"
		       "Pymacs"
		       "python-mode"
		       "rope"
		       "jedi"
		       ))
(provide 'init)
;;; init.el ends here
