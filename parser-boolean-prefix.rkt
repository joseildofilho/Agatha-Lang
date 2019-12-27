#lang racket

(require rackunit)

(define (list-no exp [acc '()])
  (cond
	[(empty? exp) acc]
	[(equal? (first exp) "agatha-not") 
	 (list-no (list-tail exp 2)
	 (append acc (list(list(first exp) (second exp)))))]
	[else 
	 (list-no (rest exp) (append acc (list (first exp))))]
	)
  )

(define (pre-fix exp)
  (pre-fix-aux (list-no exp))
  )

(define (pre-fix-aux exp)
  (cond
	[(= (length exp) 3) (list (second exp) (first exp) (last exp))]
	[(= (length exp) 1) (first exp)]
	[(and (= (length exp) 2) (equal? (first exp) "agatha-not")) exp]
	[else (list (second exp) (first exp) (pre-fix (rest(rest exp))))])
  )
(provide pre-fix)


(check-equal?
  (pre-fix (list "A" "agatha-and" "B"))
  '("agatha-and" "A" "B"))

(check-equal?
  (pre-fix (list "A" "agatha-and" "B"))
  '("agatha-and" "A" "B"))

(check-equal?
  (pre-fix (list "A" "agatha-and" "B" "agatha-and" "C"))
  '("agatha-and" "A" ("agatha-and" "B" "C"))
  )

(check-equal?
  (pre-fix (list "A" "agatha-and" "B" "agatha-and" "C" "agatha-or" "D"))
  '("agatha-and" "A" ("agatha-and" "B" ("agatha-or" "C" "D")))
  )

(check-equal?
  (pre-fix (list "A" "agatha-and" "B" "agatha-and" "C" "agatha-or" "D"))
  '("agatha-and" "A" ("agatha-and" "B" ("agatha-or" "C" "D")))
  )
(check-equal?
  (pre-fix (list "A" "agatha-and" (list "agatha-and" "A" "B")))
  '("agatha-and" "A" ("agatha-and" "A" "B"))
  )
(check-equal?
  (pre-fix (list "A"))
  "A"
  )
(check-equal?
  (pre-fix (list "agatha-not" "A"))
  '("agatha-not" "A")
  )
(check-equal?
  (pre-fix (list "B" "agatha-and" "agatha-not" "A"))
  '("agatha-and" "B" ("agatha-not" "A"))
  )
(check-equal?
  (pre-fix (list "agatha-not" "A" "agatha-and" "B"))
  '("agatha-and" ("agatha-not" "A") "B")
  )
(check-equal?
  (pre-fix (list "A" "agatha-and" (list "agatha-and" "A" "B")))
  '("agatha-and" "A" ("agatha-and" "A" "B"))
  )
(check-equal?
  (pre-fix (list "A" "agatha-and" (list "agatha-and" "A" "B") "agatha-and" "B"))
  '("agatha-and" "A" ("agatha-and" ("agatha-and" "A" "B") "B"))
  )
(check-equal?
  (pre-fix (list "A" "agatha-and" "agatha-not" (list "agatha-and" "A" "B") "agatha-and" "B"))
  '("agatha-and" "A" ("agatha-and" ("agatha-not" ("agatha-and" "A" "B")) "B"))
  )
