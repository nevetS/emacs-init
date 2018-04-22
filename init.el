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
			expand-region
			flycheck ; syntax checking for python
			jedi  ; jedi-mode for python development
			magit ; git integration
			web-beautify ; in order to use this, install
					; js-beautify with
					; npm -g install js-beautify
			yaml-mode ; yaml-mode
			highlight-parentheses ; highlight-parentheses-mode
			rainbow-delimiters ; rainbow ([{
			scss-mode ; scss / sass css files
			sqlup-mode ; make sql keywords uppercase
			company ; auto-completions with company mode
			hcl-mode ; hcl-mode for terraform
			flycheck-yamllint ; linting for yaml files via flycheck
			use-package ; tools for loading packages
			selected ; helper for multiple-cursors
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
		       "flycheck" ;multi=language linting
		       "flycheck-yamllint" ;linting for yaml
		       "graphviz"    ;graphviz mode
		       "hcl-mode" ; hcl-mode configuration
		       "highlight-parentheses-mode"
		       "ido"         ;ido directory and buffer completion
		       "js2-mode"    ;js2-mode for javascript development
		       "markdown-mode"    ;markdown mode
		       "multiple-cursors" ; multiple-cursors mode
		       "org"         ;org-mode and custom org-mode configuration
		       "sqlup-mode" ; sqlup-mode configuration
		       "unbound"     ;find unbound keys
		       "window-number" ; window numbers
		       "yasnippet"   ;provides templated completions

;; python-mode.el and dependencies
		       "python-mode.el"
		       "dash" ; virtualenvwrapper dependency
		       "jedi" ; jedi-mode configuration
		       "rope" ; Pymacs and rope-mode, requires installation
		              ;  see emacs-init/install/pymacs-rope
		       "s" ; virtualenvwrapper dependency
		       "virtualenvwrapper"


;; personal settings

		       "labburn-theme" ;color-theme
		       "personal"

;; removed pacakges
		       ;; old configurations that are still here in case I want to re-enable them
		       ;; superseded in emacs24      "color-theme" ;change default color-scheme
		       ;; superseded in emacs22		       "linum"       ;line number mode
		       ))
(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector
;;    [default default default italic underline success warning error])
;;  '(ansi-color-names-vector
;;    ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
;;  '(custom-safe-themes
;;    (quote
;;     ("57bb4729fc81d7b68e140dedb361b51572525c31007e1facf981751929a7fafe" default)))
;;  '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face) t)
;;  '(rainbow-identifiers-cie-l*a*b*-color-count 1024 t)
;;  '(rainbow-identifiers-cie-l*a*b*-lightness 80 t)
;;  '(rainbow-identifiers-cie-l*a*b*-saturation 25 t))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-document-title ((t (:inherit default :weight bold :font "Source Sans Pro" :height 1.5 :underline nil))))
;;  '(org-level-1 ((t (:inherit default :weight bold :font "Source Sans Pro" :height 1.75))))
;;  '(org-level-2 ((t (:inherit default :weight bold :font "Source Sans Pro" :height 1.5))))
;;  '(org-level-3 ((t (:inherit default :weight bold :font "Source Sans Pro" :height 1.25))))
;;  '(org-level-4 ((t (:inherit default :weight bold :font "Source Sans Pro" :height 1.1))))
;;  '(org-level-5 ((t (:inherit default :weight bold :font "Source Sans Pro"))))
;;  '(org-level-6 ((t (:inherit default :weight bold :font "Source Sans Pro"))))
;;  '(org-level-7 ((t (:inherit default :weight bold :font "Source Sans Pro"))))
;;  '(org-level-8 ((t (:inherit default :weight bold :font "Source Sans Pro")))))
