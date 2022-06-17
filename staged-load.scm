(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")
(load "../faster-miniKanren/staged-mk.scm")

(define absento-tags0
  '(absento (closure _.0) (prim _.0) (rec-closure _.0)))

(define not-tags0
  '(=/= ((_.0 closure)) ((_.0 prim)) ((_.0 rec-closure))))

(define not-tags0+error
  '(=/= ((_.0 closure)) ((_.0 error)) ((_.0 prim)) ((_.0 rec-closure))))

(load "staged-apply.scm")
(load "staged-interp.scm")
(load "staged-utils.scm")
(load "staged-run.scm")

(load "unstaged-interp.scm")

(load "test-check.scm")
