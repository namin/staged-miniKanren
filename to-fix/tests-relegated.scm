#|
;; Alas, no magic here!

(test
    (run 4 (q)
      (evalo-unstaged q q))
  '???)

(test
    (run-staged 4 (q)
      (evalo-staged q q))
  '???)
|#
