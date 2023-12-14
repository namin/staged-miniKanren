#lang racket/base

(require "../../private/internals.rkt"
         "../../test-check.rkt")

(test
    (let ((r
           (fix-scope1-syntax
            #`(fresh ()
                (== 1 #,(data '_.0))
                (fresh ()
                  (== #,(data '_.0) #,(data '_.1)))))))
      (syntax->datum (car r)))
  '(fresh (_.0) (== 1 _.0) (fresh (_.0 _.1) (== _.0 _.1))))

(test
    (let ((r
           (fix-scope1-syntax
            #`(fresh ()
                (fresh ()
                  (== #,(data '_.0) #,(data '_.1)))))))
      (syntax->datum (car r)))
  '(fresh () (fresh (_.0 _.1) (== _.0 _.1))))

(test
    (syntax->datum
     (fix-scope-syntax
      #`(fresh ()
          (fresh ()
            (== #,(data '_.0) #,(data '_.1)))
          (fresh ()
            (== #,(data '_.0) #,(data ' _.2))))))
  ;; this might not be what we want
  '(fresh () (fresh (_.1 _.0) (== _.0 _.1)) (fresh (_.2 _.0) (== _.0 _.2))))

(test
    (syntax->datum
     (fix-scope-syntax
      #`(fresh ()
          (== 1 #,(data '_.0)))))
  '(fresh (_.0)
     (== 1 _.0)))

(test
    (syntax->datum
     (fix-scope-syntax
      #`(fresh ()
         (== 1 #,(data '_.0))
         (fresh ()
           (== #,(data '_.0) #,(data '_.1))))))
  '(fresh (_.0)
     (== 1 _.0)
     (fresh (_.1)
       (== _.0 _.1))))
