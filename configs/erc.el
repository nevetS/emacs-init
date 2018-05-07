;;; Package --- Summary

;;; This is the initialization file for my ERC configuration

;;; Commentary:

;; url

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el

;; require the module
(require sk:current-plugin)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-prompt-for-password nil)
(setq erc-default-server "irc.freenode.com")
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#erc")))
(setq erc-interpret-mirc-color t)
(setq erc-kill-buffer-on-part t)
(setq erc-rename-buffers t)
(setq erc-kill-server-buffer-on-quit t)
;; (setq erc-part-reason nil)
;; (setq erc-quit-reason nil)

;;; erc.el ends here
