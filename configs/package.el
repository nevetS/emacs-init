;;; Package --- Summary

;;; This is the initialization file for my autopair configuration

;;; Commentary:

;; On a first run, this will update the package list.  On future runs, you will
;; want to run M-x package-refresh-contents

;;; Code:

;; Note on jedi - there are further installation setps that need to be followed
;; on a first run:  https://tkf.github.io/emacs-jedi/latest/
(eval-when-compile (defvar gnutls-algorithm-priority)) ; defined in 'package
(eval-when-compile (defvar sk:package-list)) ; defined in 'package
(when (require 'package nil ':noerror)

;;; any packages that are being installed or tried out belong here


  (require 'package)
  (package-initialize)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  ;; elpa archive is installed by default, add melpa, melpa-stable, and gnu
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

  ;; make sure to have downloaded archive description.
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))

  (package-initialize)

  (dolist (sk:package sk:package-list)
    (unless (package-installed-p sk:package)
      (message "installing %s" sk:package)
      (package-install sk:package)))
  
)
;;; package.el ends here
