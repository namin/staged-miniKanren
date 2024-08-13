#lang info

(define name "staged-minikanren")

(define deps '("base"
               "git://github.com/michaelballantyne/syntax-spec.git#main"))

(define license 'MIT)

(define test-omit-paths '("private/faster-minikanren" "to-fix"))
(define compile-omit-paths '("private/faster-minikanren" "to-fix"))
(define binary-omit-files '("demos"))
