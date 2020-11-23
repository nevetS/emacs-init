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
(menu-bar-mode -1)

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
  kept-old-versions 6    ; and how many of the old
  )



;The following pushes autosaves from the current directory into an
; autosaves directory
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))

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
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start) ;if we have a server-running-p function AND it returns false
  (if (not (fboundp 'server-running-p)) ;else if there simply is no server-running-p function
      (server-start)
  )
 )

; enable show-paren-mode
(show-paren-mode 1)

; save desktop on exit
;;(desktop-save-mode 1)


; set default font on OSX
(when (eq system-type 'darwin)
  (message "setting default font to Source Code Pro")
  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "Source Code Pro")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly.
  (set-face-attribute 'default nil :height 125)

  ;; you may want to add different for other charset in this way.
  )
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :family "Source Code Pro")
	(set-face-attribute 'default nil :height 100))

(face-spec-set
 'region
 '((t :background "steel blue"))
 'face-defface-spec)
(face-spec-set
 'ivy-current-match
 '((t :background "brown"))
 'face-defface-spec)

(face-spec-set
 'font-lock-string-face
 '((t :foreground "cornsilk2" ))
 'face-defface-spec
 )

(face-spec-set
 'font-lock-comment-face/
 '((t :foreground "peach puff" ))
 'face-defface-spec
 )

(face-spec-set
 'rst-literal
 '((t :foreground "IndianRed2" ))
 'face-defface-spec
 )

(face-spec-set
 'rst-level-2
 '((t
  (:family "Lucida Grande" :height 1.25 :weight bold :background "grey22")))
 'face-defface-spec
 )
(face-spec-set
 'rst-level-2
 '((t
  (:family "Lucida Grande" :height 1.5 :weight bold :background "grey22")))
 'face-defface-spec
 )
(face-spec-set
 'rst-level-1
 '((t
  (:family "Lucida Grande" :height 1.75 :weight bold :background "grey22")))
 'face-defface-spec
 )

(global-set-key (read-kbd-macro "s-<return>") 'toggle-frame-fullscreen)

;; yaml clean whitespace
;; (defun sk_python_clean_whitespace()
;;   (add-to-list 'before-save-hook 'delete-trailing-whitespace)
;;   )
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'yaml-mode-hook 'sk_python_clean_whitespace)

;; enable upcase and downcase region commands
;;  C-x C-u / C-x C-l
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; hide *buffers from ido
(setq ido-ignore-buffers '("\*.*\*"))

;; transposing lines
;; from https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)
(setenv "WORKON_HOME"
	"~.venvs/")
;; single dired buffer using 'a' on path
(put 'dired-find-alternate-file 'disabled nil)
(electric-pair-mode)

;;chmod +x shell scripts
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; personal.el ends here
