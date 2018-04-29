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

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first message.
;; `gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)
