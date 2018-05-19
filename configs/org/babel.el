; add python and sh support to org-mode source blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sh . t)))

;; don't require confirmation on C-c C-c for source code block
(setq org-confirm-babel-evaluate nil)

;; turn on syntax highlighting for code blocks
(setq org-src-fontify-natively t)
