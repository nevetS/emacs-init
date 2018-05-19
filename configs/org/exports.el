(eval-after-load 'ox-html
  ;; If you prefer to use ~ for <code> tags. Replace "code" with
  ;; "verbatim" here, and replace "~" with "=" below.
  '(push '(code . "<kbd>%s</kbd>") org-html-text-markup-alist))
