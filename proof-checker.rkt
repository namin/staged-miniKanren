#lang racket

(define (proof? proof)
  (letrec ([member?
            (lambda (x ls)
              (if (null? ls) #f
                  (if (equal? (car ls) x) #t
                      (member? x (cdr ls)))))])
    (letrec ([proof?
              (lambda (proof)
                (match proof
                  [`(,A ,assms assumption ()) (member? A assms)]
                  [`(,B ,assms modus-ponens
                        (((,A => ,B) ,assms ,r1 ,ants1)
                         (,A ,assms ,r2 ,ants2)))
                   (and (proof? (list (list A '=> B) assms r1 ants1))
                        (proof? (list A assms r2 ants2)))]
                  [`((,A => ,B) ,assms conditional
                     ((,B (,A . ,assms) ,rule ,ants)))
                   (proof? (list B (cons A assms) rule ants))]))])
      (proof? proof))))

(proof?
 '(B (A (A => B))
     modus-ponens
     (((A => B) (A (A => B)) assumption ())
      (A (A (A => B)) assumption ()))))

(proof?
 '(B (A (A => B))
     modus-ponens
     (((A => B) (A (A => B)) assumption ())
      (A (A (A => B)) assumption ()))))

(proof?
 '(C (A (A => B) (B => C))
     modus-ponens
     (((B => C) (A (A => B) (B => C)) assumption ())
      (B (A (A => B) (B => C))
         modus-ponens
         (((A => B) (A (A => B) (B => C)) assumption ())
          (A (A (A => B) (B => C)) assumption ()))))))
