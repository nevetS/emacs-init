;;; Package --- Summary

;;; This file sets configuration associated with babel for org-mode

;;; Commentary:
;;; for more information on babel see
;;; https://orgmode.org/worg/org-contrib/babel/

;;; Compatibility: Emacs 26.2 or newer.  May work in older versions, may not

;; add language support to org-mode source blocks

;;; Code:

(eval-when-compile (defvar org-babel-do-load-languages)) ; defined by source
(eval-when-compile (defvar org-confirm-babel-evaluate)) ; defined by source
(eval-when-compile (defvar org-src-fontify-natively)) ; defined by source
(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk . t)
   (browser . t) ; requires phantomjs
   (blockdiag . t) ; blockdiag
   (calc . t)
   (css . t)
   (ditaa . t) ; ascii boxes to flow charts
   (dot . t) ; requires graphviz
   (gnuplot . t) ; requires gnuplot
   (http . t)
   (java . t) ;
   (js . t) ; requires node
   (lisp . t)
   (makefile . t)
   (org . t)
   (python . t)
   (plantuml . t)
   (R . t) ; requires R, and install.packages('tikzDevice')
   (sass . t) ; requires node, npm, npm module sass,
   (sed . t)
   (shell . t)
   (sql . t) ;mysql, postgres, osql, dbish, or monetdb
   (wsdmode . t)))

;; don't require confirmation on C-c C-c for source code block
(setq org-confirm-babel-evaluate nil)

;; turn on syntax highlighting for code blocks
(setq org-src-fontify-natively t)
