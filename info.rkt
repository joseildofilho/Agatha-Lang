#lang info
(define collection "agatha")
(define version "1.0")
(define scribblings '(("scribblings/agatha.scrbl")))
(define test-omit-paths '("agatha-test1.rkt" "agatha-test2.rkt" "agatha-test3.rkt" ))
(define deps '("base"
               "beautiful-racket-lib"
               "brag-lib"
               "rackunit-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
