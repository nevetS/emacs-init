;;; Package --- Summary

;;; This is the configuration file for dockerfile mode.

;;; Commentary:

;; Compatibility: Emacs 24.x or newer.  May work in older versions, may not

;; https://github.com/spotify/dockerfile-mode

;;; Code:
(use-package dockerfile-mode
  :load-path "~/.emacs.d/plugins/dockerfile-mode"
  :mode "Dockerfile\\'")

(provide 'dockerfile-mode-config)
;;; dockerfile-mode.el ends here
