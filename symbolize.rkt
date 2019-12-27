#lang racket

(require racket/contract rackunit)

(provide (contract-out [list-string->list-symbol (list? . -> . list?)]))
(define (list-string->list-symbol list-strings)
  (map (lambda (item)
		 (cond
		   [(list? item) (list-string->list-symbol item)]
		   [else (string->symbol item)]
		   )
		 )
	   list-strings
	   )
  )

(check-equal?
  (list-string->list-symbol (list))
  '())

(check-equal?
  (list-string->list-symbol (list "A"))
  '(A))

(check-equal?
  (list-string->list-symbol (list "A" (list "A")))
  '(A (A)))

(check-equal?
  (list-string->list-symbol (list "A" (list "A" (list "A"))))
  '(A (A (A))))

(check-equal?
  (list-string->list-symbol (list "A" "B" (list "A")))
  '(A B (A)))
