#lang info

(define name "staged-minikanren")

(define deps '("base"
               "syntax-spec-v2"))

(define license 'MIT)

(define test-omit-paths '("private/faster-minikanren" "to-fix" "tests/barliman-comparison"))
(define compile-omit-paths '("private/faster-minikanren" "to-fix" "tests/barliman-comparison"))
(define binary-omit-files '("demos"))
