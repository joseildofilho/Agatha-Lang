#lang racket

(require racket/contract)

(define (display-listln list-s)
  (cond
	[(empty? list-s) (void)]
	[else (begin
			(displayln (first list-s))
			(display-listln (rest list-s))
		   )]
	)
  )
(provide (contract-out [display-listln (list? . -> . void?)]))
