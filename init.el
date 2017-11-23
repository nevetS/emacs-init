;;; Package --- Steve's Emacs Initialization
;;
;;; Commentary:
;;     Steve's Emacs configuration, modularized to take the bite out of an
;;     entirely too large init.el
;;
;;     Packages are installed by configs/package.el, which is the first config
;;     file to be loaded. If additional packages are desired, add the package
;;     name to the variable sk:package-list at the top of that configuration
;;     file. 
;;
;;     There are many packages that are manually installed.  I tend to prefer
;;     manual installation over package installation because that allows for the
;;     privelege of installing updates prior to release of a package to one of
;;     the package repositories.  There are times when the package repositories
;;     are sorely out of date.
;;
;;     Each package installation consists of a config file in
;;     /configs/package-name.el and a similarly named directory in
;;     plugins/package-name that contains the plugin content.  Where possible
;;     the plugins/package-name directory is a git submodule pointed directly
;;     at the source repository of the official package.  Updating a package to
;;     the newest release should be a matter of 'git checkout tags/<release_name>'
;;     To view the installed release for a given submodule, the command
;;     'git branch' should respond with " *(Head detached at <release name>)"
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
;; these packages are installed by the package config file
(setq sk:package-list '(
			expand-region
			flycheck ; syntax checking for python
			jedi  ; jedi-mode for python development
			magit ; git integration
			web-beautify ; in order to use this, install
					; js-beautify with
					; npm -g install js-beautify
			yaml-mode ; yaml-mode
			highlight-parentheses ; highlight-parentheses-mode

;;removed for now	company ; auto-complistions with company mode
			))


;; packages to be loaded configured are in alphabetical order except in the
;; following situations:
;; * package always comes first
;; * personal always comes last
;; * packages with dependencies are grouped together after the alphabetical list
(sk:load-config-file '(
		       "package"     ; installs any missing packages,
				     ; initializes packages
;; main packages
		       "autopair"    ;close parens/brackets/etc
		       "dockerfile-mode"      ;dockerfile-mode
		       "expand-region"
		       "fci"         ;fill column indicator
		       "graphviz"    ;graphviz mode
		       "ido"         ;ido directory and buffer completion
		       "js2-mode"    ;js2-mode for javascript development
		       "markdown-mode"    ;markdown mode
		       "org"         ;org-mode and custom org-mode configuration
		       "unbound"     ;find unbound keys
		       "window-number" ; window numbers
		       "yasnippet"   ;provides templated completions
		       "highlight-parentheses-mode"
;; python-mode.el and dependencies
		       "python-mode.el"
		       "dash" ; virtualenvwrapper dependency
		       "jedi" ; jedi-mode configuration
		       "rope" ; Pymacs and rope-mode, requires installation
		              ;  see emacs-init/install/pymacs-rope
		       "s" ; virtualenvwrapper dependency
		       "virtualenvwrapper"


;; personal settings
		       "personal"

;; removed pacakges
		       ;; old configurations that are still here in case I want to re-enable them
		       ;; superseded in emacs24      "color-theme" ;change default color-scheme
		       ;; superseded in emacs22		       "linum"       ;line number mode
		       ))
(provide 'init)
;;; init.el ends here
