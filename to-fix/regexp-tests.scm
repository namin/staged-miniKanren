(load "staged-mk.scm")
(load "staged-regexp.scm")

(run 6 (q)
  (regexp-matcho '(seq foo (rep bar)) q regexp-BLANK))
;; we get some spurious cases
