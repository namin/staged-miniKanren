;; using namin/faster-miniKaren branch staged
(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(load "full-interp.scm")

(load "test-check.scm")

(test (run* (q) (evalo
		 `(if #t #t #f)
		 q))
      `(#t))

(test (run* (q) (evalo
		`(choice #t #f)
		q))
      `(#t #f))
