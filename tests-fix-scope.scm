(test
    (fix-scope
     '(fresh ()
        (== 1 _.0)))
  '(fresh (_.0)
     (== 1 _.0)))

(test
    (fix-scope
     '(fresh ()
        (== 1 _.0)
        (fresh ()
          (== _.0 _.1))))
  '(fresh (_.0)
     (== 1 _.0)
     (fresh (_.1)
       (== _.0 _.1))))

(test
    (fix-scope1
     '(fresh ()
        (== 1 _.0)
        (fresh ()
          (== _.0 _.1))))
  '((fresh (_.0) (== 1 _.0) (fresh (_.1 _.0) (== _.0 _.1))) ()))

(test
    (let ((r
           (fix-scope1-syntax
            #`(fresh ()
                (== 1 #,(data '_.0))
                (fresh ()
                  (== #,(data '_.0) #,(data '_.1)))))))
      (syntax->datum (first r)))
  '(fresh (_.0) (== 1 _.0) (fresh (_.0 _.1) (== _.0 _.1))))



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
