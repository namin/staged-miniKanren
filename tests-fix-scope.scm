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



