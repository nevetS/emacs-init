;;; Package --- Summary

;;; This is the initialization file for my autopair configuration

;;; Commentary:

;; 

;;; Code:

;;

;; I don't think this is necessary, but it's a good example of
;; executing something only on OSX
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

;; leave cursor in same spot when paging up or down (C-v/M-v)
(setq scroll-preserve-screen-position t)

;disable scroll bars
(scroll-bar-mode -1)

;disable tool bar
(tool-bar-mode -1)

;menu bar available on ctrl-right click
(menu-bar-mode -99)

;TODO look up what this does
(setq x-select-enable-clipboard-manager nil)

;enables replacing of highlighted text
(delete-selection-mode 1)

;;; backups and auto-saves.

;The following pushes backups from the current directory into a backups
; directory
(setq
  backup-directory-alist '((".*" . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 6   ; how many of the newest versions to keep
  kept-old-versions 2    ; and how many of the old
  )



;The following pushes autosaves from the current directory into an
; autosaves directory
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))

;The following stops the creation of lockfiles.
; essentially, whenever a file is opened, if this is t, a sym link gets created
; .#filename->user@emacspid.12345
; This setting makes it so those files are not generated.
; It creates danger for simultaneous overwrites, but I'm not particularly concerned
; about that compared with directory timestamps, inode limits, and orphaned links
; left by crashed sessions.
(setq create-lockfiles nil)
;the following line is in place so that when emacs is called
; from the command line with a file
; emacs will open in single-buffer mode
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook 'delete-other-windows)

(setq column-number-mode t)

; turn on outline-minor-mode
(outline-minor-mode 1)

; turn on Emacs server
(unless (server-running-p)
  (server-start))

;;; personal.el ends here
