;;; Package --- Steve's Emacs Initialization
;;
;;; Commentary:
;;     Steve's Emacs configuration, modularized to take the bite out of an
;;     entirely too large init.el



;;; Code:


(defconst sk:emacs-config-dir (expand-file-name "configs/"
	                      (file-name-directory load-file-name))
                              "Path to configuration and init files")

(defconst sk:emacs-plugin-path (expand-file-name "plugins/"
	                       (file-name-directory load-file-name))
                               "Path to plugins")

;; utility function to auto-load my package configurations
(defun sk:load-config-file (filelist)
    (dolist (file filelist)
      (load (expand-file-name
             (concat sk:emacs-config-dir file)))
       (message "Loaded config file:%s" file)
       ))

(sk:load-config-file '(;;base
		       "yasnippet"   ;provides templated completions
;; superseded in emacs24      "color-theme" ;change default color-scheme
		       "unbound"     ;find unbound keys
		       "autopair"    ;close parens/brackets/etc
		       "fci"         ;fill column indicator
;; superseded in emacs22		       "linum"       ;line number mode
		       "window-number" ; window numbers
;		       "packages"    ;set up package archives
		       ;;
		       "markdown-mode"    ;markdown mode
		       "graphviz"    ;graphviz mode
		       "dockerfile-mode"      ;dockerfile-mode
		       
		       ;;python
		       "s" ; virtualenvwrapper dependency
		       "dash" ; virtualenvwrapper dependency
		       "virtualenvwrapper"
		       "rope" ; Pymacs and rope-mode, requires installation
		              ;  see emacs-init/install/pymacs-rope
		       "python-mode.el"
		       "expand-region"
		       "ido"
		       "org"
		       "js2-mode"
;		       "jedi"
		       ))
(provide 'init)
;;; init.el ends here
