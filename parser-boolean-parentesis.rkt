#lang racket

(require rackunit)

(define (list-no exp [acc '()])
  (cond
	[(empty? exp) acc]
	[(equal? (first exp) 'agatha-not) 
	 (list-no (list-tail exp 2)
	 (append acc (list(list(first exp) (second exp)))))]
	[else 
	 (list-no (rest exp) (append acc (list (first exp))))]
	)
  )

(define (infix exp)
  (infix-aux (list-no exp))
  )

(define (infix-aux exp)
  (cond
	[(= (length exp) 3) (list (first exp) (second exp) (last exp))]
	[(= (length exp) 1) (first exp)]
	[(and (= (length exp) 2) (equal? (first exp) 'agatha-not)) exp]
	[else (list (first exp) (second exp) (infix (rest(rest exp))))])
  )
(provide infix)


(check-equal?
  (infix (list 'A 'agatha-and 'B))
  '(A agatha-and B))

(check-equal?
  (infix (list 'A 'agatha-and 'B 'agatha-and 'C))
  '(A agatha-and (B agatha-and C))
  )

(check-equal?
  (infix (list 'A 'agatha-and 'B 'agatha-and 'C 'agatha-or 'D))
  '(A agatha-and (B agatha-and (C agatha-or D)))
  )

(check-equal?
  (infix (list 'A 'agatha-and 'B 'agatha-and 'C 'agatha-or 'D))
  '(A agatha-and (B agatha-and (C agatha-or D)))
  )
(check-equal?
  (infix (list 'A 'agatha-and (list 'A 'agatha-and 'B)))
  '(A agatha-and (A agatha-and B))
  )
(check-equal?
  (infix (list 'A))
  'A
  )
(check-equal?
  (infix (list 'agatha-not 'A))
  '(agatha-not A)
  )
(check-equal?
  (infix (list 'B 'agatha-and 'agatha-not 'A))
  '(B agatha-and (agatha-not A))
  )
(check-equal?
  (infix (list 'agatha-not 'A 'agatha-and 'B))
  '((agatha-not A) agatha-and B)
  )
(check-equal?
  (infix (list 'A 'agatha-and (list 'A 'agatha-and 'B)))
  '(A agatha-and (A agatha-and B))
  )
(check-equal?
  (infix (list 'A 'agatha-and (list 'agatha-and 'A 'B) 'agatha-and 'B))
  '(A agatha-and ((agatha-and A B) agatha-and  B))
  )
(check-equal?
  (infix (list 'A 'agatha-and 'agatha-not (list 'agatha-and 'A 'B) 'agatha-and 'B))
  '(A agatha-and ((agatha-not (agatha-and A B)) agatha-and B))
  )
