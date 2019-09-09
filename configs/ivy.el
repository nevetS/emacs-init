;;; Package --- Summary

;;; This is the initialization file for my ivy configuration

;;; Commentary:

;; https://github.com/abo-abo/swiper

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el
(require 'counsel)
;; require the module
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-x l") 'counsel-locate)


(global-set-key (kbd "C-c C-r") 'ivy-resume)

(defhydra hydra-counsel ()
  "counsel"
  ("p" counsel-etags-find-tag-at-point :color blue )
  ("t" counsel-etags-find-tag :color blue )
  ("f" counsel-find-file :color blue)
  ("g" counsel-git :color blue )
  ("r" counsel-rg :color blue )
  ("s" swiper :color blue)
  ("e" eval-region :color blue)
  ("q" nil :color blue))

(global-set-key (kbd "C-c i") 'hydra-counsel/body)
;;; ivy.el ends here
