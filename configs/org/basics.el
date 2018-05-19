(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-use-speed-commands t)
;; open links by hitting enter
(setq org-return-follows-link t)
(require 'org-protocol)
