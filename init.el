;;; Package --- Steve's Emacs Initialization
;;
;;; Commentary:
;;     Steve's Emacs configuration, modularized to take the bite out of an
;;     entirely too large init.el
;;
;;     Packages are installed by configs/package.el, which is the first config
;;     file to be loaded.  If additional packages are desired, add the package
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
                              "Path to configuration and init files.")

(defconst sk:emacs-plugin-path (expand-file-name "plugins/"
	                       (file-name-directory load-file-name))
                               "Path to plugins.")


;; utility function to auto-load my package configurations
(defun sk:load-config-file (filelist)
    "Iterate FILELIST and load each file."
    (dolist (file filelist)
      (load (expand-file-name
             (concat sk:emacs-config-dir file)))
       (message "Loaded config file: %s" file)
       ))

(defvar sk:package-list)
;; these packages are installed by the package config file
(setq sk:package-list '(
			all-the-icons
;			auctex ;
			counsel ;
			counsel-etags ; etags for counsel
			counsel-projectile ;
			expand-region ;
;			emacs-ess ; emacs speaks statistics
			flycheck ; syntax checking for python
			ggtags ;
			gnuplot ; gnuplot
			gnuplot-mode ; support for gnu-plot
			google-translate ; google-translate
			hydra ; hydras
			;; jedi  ; jedi-mode for python development
			plantuml-mode ; uml diagrams - this may require manual installation with M-x package-install
			magit ; git integration
			sass-mode ; sass for css
;			slime ; lisp development
			web-beautify ; in order to use this, install
					; js-beautify with
					; npm -g install js-beautify
			yaml-mode ; yaml-mode
			highlight-parentheses ; highlight-parentheses-mode
			ivy ; ido-mode replacement
			ob-blockdiag ;block diag for babel
			ob-browser ; screenshots in org-mode
			ob-http ; http request in org-mode
			ob-translate ; google translate in org-mode
			ox-hugo ; org mode to Hugo exporter
			projectile ; projects in emacs
			rainbow-delimiters ; rainbow ([{
			;; rjsx-mode ; react / react-native
			scss-mode ; scss / sass css files
			salt-mode ; SaltStack
			;; spaceline ; mode line customizations
			;; spaceline-all-the-icons ; mode line update
			sqlup-mode ; make sql keywords uppercase
			company ; auto-completions with company mode
			hcl-mode ; hcl-mode for terraform
			htmlize ; org-export agenda
			flycheck-yamllint ; linting for yaml files via flycheck
			use-package ; tools for loading packages
			use-package-hydra ; define hydras within use-package blocks
			selected ; helper for multiple-cursors
			wsd-mode ; web sequence diagrams https://github.com/josteink/wsd-mode
			))
;;

;; packages to be loaded configured are in alphabetical order except in the
;; following situations:
;; * package always comes first
;; * personal always comes last
;; * packages with dependencies are grouped together after the alphabetical list
(sk:load-config-file '(
		       "package"     ; installs any missing packages,
				     ; initializes packages
;; main packages
		       "autopair"    ;close parens/brackets/etc - turn off autopair in favor of electric-pair
		       ;; "dockerfile-mode"      ;dockerfile-mode
		       "expand-region"
		       "erc"
		       "fci"         ;fill column indicator
		       "flycheck" ;multi=language linting
		       "flycheck-yamllint" ;linting for yaml
		       "graphviz"    ;graphviz mode
		       "gnus" ; gnus mail reader
		       "hcl-mode" ; hcl-mode configuration
		       "highlight-parentheses-mode"
		       "ivy" ; ido replacement
		       "ido"         ;ido directory and buffer completion - removed to try out ivy-mode
		       ;; "js2-mode"    ;js2-mode for javascript development
		       "markdown-mode"    ;markdown mode
		       "multiple-cursors" ; multiple-cursors mode
		       "org/org_config_variables" ; org config variables
		       "org/org_babel" ; source code in org
		       "org/capture-templates" ; capture templates in org
		       "org/org_tasks" ; task configuration in org
		       "org/keybindings" ; keybindings in org
		       "org/appearance" ; org look and feel
		       "org/exports" ; export templates
		       "plantuml" ; uml diagrams
		       "projectile"
                       ;; "spaceline-all-the-icons" ; mode line update
		       "sqlup-mode" ; sqlup-mode configuration
		       "unbound"     ;find unbound keys
		       "window-number" ; window numbers
		       "yasnippet"   ;provides templated completions

;; python-mode.el and dependencies
		       ;; "python-mode.el"
		       ;; "dash" ; virtualenvwrapper dependency
		       ;; ;; "jedi" ; jedi-mode configuration
		       ;; ;; "rope" ; Pymacs and rope-mode, requires installation
		       ;; ;;        ;  see emacs-init/install/pymacs-rope
		       ;; "s" ; virtualenvwrapper dependency
		       ;; "virtualenvwrapper"
;; personal settings

		       "labburn-theme" ;color-theme
		       "personal"

;; removed pacakges
		       ;; old configurations that are still here in case I want to re-enable them
		       ;; superseded in emacs24      "color-theme" ;change default color-scheme
		       ;; superseded in emacs22		       "linum"       ;line number mode
		       ))
;; (load "~/.emacs.d/configs/frame-cmds-config.el")
(add-to-list 'load-path "~/.emacs.d/configs/")
(require 'dockerfile-mode-config)
(require 'frame-cmds-config)
;;(add-to-list 'load-path '~/.emacs.d/configs/dockerfile-mode-config.el)


(provide 'init)
;;; init.el ends here
