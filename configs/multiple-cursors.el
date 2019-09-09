;;; Package --- Summary

;;; This is the initialization file for my multiple cursors configuration

;;; Commentary:

;; Compatibility: Emacs 24.3 or newer.  Older versions require cl-lib

;; https://github.com/magnars/multiple-cursors.el

;; In Emacs 24.4 or newer, it's recommended to use electric-pair-mode instead

;;; Code:
(eval-when-compile (defvar sk:emacs-plugin-path)) ; defined in ~/.init.el
(eval-when-compile (defvar sk:current-plugin)) ; defined in ~/.init.el
(eval-when-compile (defvar selected-keymap)) ; defined in ~/.init.el

(setq sk:current-plugin 'multiple-cursors)
(selected-global-mode 1)
(if (boundp 'sk:emacs-plugin-path)
    (add-to-list 'load-path (concat sk:emacs-plugin-path (symbol-name 'multiple-cursors.el)))
  (message "plugin-path not defined") ;else
    )
;; require the module
;; (require sk:current-plugin)
;; (global-set-key (kbd "C-c m") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-c n") 'mc/mark-sgml-tag-pair)

(use-package multiple-cursors
  ;; stolen from https://github.com/jwiegley/dot-emacs/blob/master/init.el

;;  :after phi-search
;;  :defer 5
;;  :demand
  ;; - Sometimes you end up with cursors outside of your view. You can scroll
  ;;   the screen to center on each cursor with `C-v` and `M-v`.
  ;;
  ;; - If you get out of multiple-cursors-mode and yank - it will yank only
  ;;   from the kill-ring of main cursor. To yank from the kill-rings of every
  ;;   cursor use yank-rectangle, normally found at C-x r y.

  :bind (("C-c *" . hydra-multiple-cursors/body)
	 ("S-<mouse-1>" . mc/add-cursor-on-click))

  :hydra (hydra-multiple-cursors (:hint nil :color pink)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit nil)
  ("a" mc/mark-all-like-this :color blue)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/mark-all-in-region-regexp :color blue)
  ("0" mc/insert-numbers :color blue)
  ("2" mc/insert-fancy-numbers :color blue)
  ("A" mc/insert-letters :color blue)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil :exit nil))
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))

)
  ;; :preface
  ;; (defun reactivate-mark ()
  ;;   (interactive)
  ;;   (activate-mark)))

;;
  ;; :bind (:map selected-keymap
  ;;             ("c"   . mc/edit-lines)
  ;;             ("."   . mc/mark-next-like-this)
  ;;             ("<"   . mc/unmark-next-like-this)
  ;;             ("C->" . mc/skip-to-next-like-this)
  ;;             (","   . mc/mark-previous-like-this)
  ;;             (">"   . mc/unmark-previous-like-this)
  ;;             ("C-<" . mc/skip-to-previous-like-this)
  ;;             ("y"   . mc/mark-next-symbol-like-this)
  ;;             ("Y"   . mc/mark-previous-symbol-like-this)
  ;;             ("w"   . mc/mark-next-word-like-this)
  ;;             ("W"   . mc/mark-previous-word-like-this))


;; old bindings that didn't work
	 ;; ("<C-m> ^"     . mc/edit-beginnings-of-lines)
         ;; ("<C-m> `"     . mc/edit-beginnings-of-lines)
         ;; ("<C-m> $"     . mc/edit-ends-of-lines)
         ;; ("<C-m> '"     . mc/edit-ends-of-lines)
         ;; ("<C-m> R"     . mc/reverse-regions)
         ;; ("<C-m> S"     . mc/sort-regions)
         ;; ("<C-m> W"     . mc/mark-all-words-like-this)
         ;; ("<C-m> Y"     . mc/mark-all-symbols-like-this)
         ;; ("<C-m> a"     . mc/mark-all-like-this-dwim)
         ;; ("<C-m> c"     . mc/mark-all-dwim)
         ;; ("<C-m> l"     . mc/insert-letters)
         ;; ("<C-m> n"     . mc/insert-numbers)
         ;; ("<C-m> r"     . mc/mark-all-in-region)
         ;; ("<C-m> s"     . set-rectangular-region-anchor)
         ;; ("<C-m> %"     . mc/mark-all-in-region-regexp)
         ;; ("<C-m> t"     . mc/mark-sgml-tag-pair)
         ;; ("<C-m> w"     . mc/mark-next-like-this-word)
         ;; ("<C-m> x"     . mc/mark-more-like-this-extended)
         ;; ("<C-m> y"     . mc/mark-next-like-this-symbol)
         ;; ("<C-m> C-x"   . reactivate-mark)
         ;; ("<C-m> C-SPC" . mc/mark-pop)
         ;; ("<C-m> ("     . mc/mark-all-symbols-like-this-in-defun)
         ;; ("<C-m> C-("   . mc/mark-all-words-like-this-in-defun)
         ;; ("<C-m> M-("   . mc/mark-all-like-this-in-defun)
         ;; ("<C-m> ["     . mc/vertical-align-with-space)
         ;; ("<C-m> {"     . mc/vertical-align)

;;; multiple-cursors.el ends here
