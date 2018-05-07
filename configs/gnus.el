(require 'starttls)
(require 'smtpmail)
(require 'nnir)

;; @see http://www.emacswiki.org/emacs/GnusGmail#toc1
(setq gnus-select-method '(nntp "news.gmane.org")) ;; if you read news groups

;; ask encryption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; turn off auto-expiration
(setq gnus-inhibit-user-auto-expire t)

;; @see http://gnus.org/manual/gnus_397.html
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "stanford"
                      (nnimap-address "localhost")
                      (nnimap-server-port 1993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      (nnmail-expiry-wait 90)))

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))

;; Threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; show all email folders
;; from https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

(define-key gnus-group-mode-map
  ;; list all the subscribed groups even they contain zero un-read messages
  (kbd "o") 'my-gnus-group-list-subscribed-groups)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first message.
;; `gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

;; don't include headers in forward
(setq message-forward-ignored-headers "^.*") 

;; Personal Information
(setq user-full-name "Steve Kallestad"
      user-mail-address "stevenk1@stanford.edu")
(load-library "smtpmail")

(setq smtpmail-smtp-server "smtp.stanford.edu"
      smtpmail-default-smtp-server "smtp.stanford.edu")
(setq send-mail-function 'smtpmail-send-it)

(setq  message-send-mail-function 'message-smtpmail-send-it)
(setq smtpmail-local-domain "stanford.edu")
(setq smtpmail-sendto-domain "stanford.edu")
;; (setq smtpmail-local-domain "gmail.com")
;; (setq smtpmail-sendto-domain "gmail.com")

;;       smtpmail-auth-credentials "~/.authinfo.gpg"
(setq smtpmail-smtp-service "465")
;;(setq smtpmail-stream-type 'starttls)
(setq smtpmail-stream-type 'ssl)
;; (setq smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-stream-type 'starttls)
(setq starttls-use-gnutls t)
(setq starttls-gnutls-program "gnutls-cli")
(setq starttls-extra-arguments nil)
;; (defun gnutls-available-p ()
;;   "Function redefined in order not to use built-in GnuTLS support"
;;   nil)
;; (setq starttls-gnutls-program "gnutls-cli")
;; (setq starttls-use-gnutls t)
;; (setq send-mail-function 'mailclient-send-it)
(setq smtpmail-debug-info t)
(setq smtpmail-debug-verb t)
(defun toggle-mail-from-field ()
  (interactive)
  (cond
   ((string= "stevenk1@stanford.edu" user-mail-address)
    (setq user-mail-address "stevenk1@stanford.edu"))
   (t
    (setq user-mail-address "kallestad@gmail.com")))
  (message "Mail FROM: %s" user-mail-address))

;; show threading like in wikipedia
;; from https://groups.google.com/d/msg/gnu.emacs.gnus/pnOnQ1bnFB8/11PI-CD3RRMJ
;; (when window-system
;;   (setq gnus-sum-thread-tree-indent "  ")
;;   (setq gnus-sum-thread-tree-root "● ")
;;   (setq gnus-sum-thread-tree-false-root "◯ ")
;;   (setq gnus-sum-thread-tree-single-indent "◎ ")
;;   (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
;;   (setq gnus-sum-thread-tree-vertical "│")
;;   (setq gnus-sum-thread-tree-single-leaf "╰─► "))

;; (cond (window-system
;;        (setq custom-background-mode 'dark)
;;        (defface my-group-face-1
;;          '((t ( :foreground "white" :bold t))) "First group face")
;;        (defface my-group-face-2
;;          '((t ( :foreground "red" :bold t)))
;;          "Second group face")
;;        (defface my-group-face-3
;;          '((t ( :foreground "ivy" :bold t))) "Third group face")
;;        (defface my-group-face-4
;;          '((t ( :foreground "pink" :bold t))) "Fourth group face")
;;        (defface my-group-face-5
;;          '((t ( :foreground "orange" :bold t))) "Fifth group face")))

;; (setq gnus-group-highlight
;;       '(((> unread 200) . my-group-face-1)
;;         ((and (< level 3) (zerop unread)) . my-group-face-2)
;;         ((< level 3) . my-group-face-3)
;;         ((zerop unread) . my-group-face-4)
;;         (t . my-group-face-5)))

;; display groups (imap folders)
;; (setq gnus-group-line-format "\t%4m%P%M%STotal Messages: %5t\t[%20G\] \tNew Messages: (%y)\n")
(setq gnus-group-line-format "\t%4m%P%M%S \t[%-25G\] \tMessages: (%y/%t)\n")

(defun gnus-user-format-function-@ (header)
"Display @ for message with attachment in summary line.

You need to add `Content-Type' to `nnmail-extra-headers' and
`gnus-extra-headers', see Info node `(gnus)To From Newsgroups'."
(let ((case-fold-search t)
(ctype (or (cdr (assq 'Content-Type (mail-header-extra header)))
"text/plain"))
indicator)
(when (string-match "^multipart/mixed" ctype)
(setq indicator "@"))
(if indicator
indicator
" ")))



(defalias 'gnus-user-format-function-score 'rs-gnus-summary-line-score)

(defun rs-gnus-summary-line-score (head)
  "Return pretty-printed version of article score.

See (info \"(gnus)Group Line Specification\")."
  (let ((c (gnus-summary-article-score (mail-header-number head))))
    ;; (gnus-message 9 "c=%s chars in article %s" c (mail-header-number head))
    (cond ((< c -1000)     "vv")
          ((< c  -100)     " v")
          ((< c   -10)     "--")
          ((< c     0)     " -")
          ((= c     0)     "  ")
          ((< c    10)     " +")
          ((< c   100)     "++")
          ((< c  1000)     " ^")
          (t               "^^"))))
;;; from the wikipedia screenshot taker
;; https://groups.google.com/d/msg/gnu.emacs.gnus/pnOnQ1bnFB8/o8NZkG3j8n0J
;;; threading
(setq gnus-face-9 'font-lock-warning-face)
(setq gnus-face-10 'shadow)
(defun sdl-gnus-summary-line-format-ascii nil
  (interactive)
  (setq gnus-summary-line-format
        (concat
         "%0{%U%R%z%}" "%10{|%}" "%1{%d%}" "%10{|%}"
         "%9{%u&@;%}" "%(%-15,15f %)" "%10{|%}" "%4k" "%10{|%}"
         "%2u&score;" "%10{|%}" "%10{%B%}" "%s\n"))
  (setq
   gnus-sum-thread-tree-single-indent   "o "
   gnus-sum-thread-tree-false-root      "x "
   gnus-sum-thread-tree-root            "* "
   gnus-sum-thread-tree-vertical        "| "
   gnus-sum-thread-tree-leaf-with-other "|-> "
   gnus-sum-thread-tree-single-leaf     "+-> " ;; "\\" is _one_ char
   gnus-sum-thread-tree-indent          "  ")
  (gnus-message 5 "Using ascii tree layout."))

(defun sdl-gnus-summary-line-format-unicode nil
  (interactive)
  (setq gnus-summary-line-format
        (concat
         "%0{%U%R%z%}" "%10{│%}" "%1{%d%}" "%10{│%}"
         "%9{%u&@;%}" "%(%-15,15f %)" "%10{│%}" "%4k" "%10{│%}"
         "%2u&score;" "%10{│%}" "%10{%B%}" "%s\n"))
  (setq
   gnus-sum-thread-tree-single-indent   "◎ "
   gnus-sum-thread-tree-false-root      "  "
   gnus-sum-thread-tree-root            "┌ "
   gnus-sum-thread-tree-vertical        "│"
   gnus-sum-thread-tree-leaf-with-other "├─>"
   gnus-sum-thread-tree-single-leaf     "└─>"
   gnus-sum-thread-tree-indent          "  ")
  (gnus-message 5 "Using ascii tree layout with unicode chars."))

(sdl-gnus-summary-line-format-unicode)
   
