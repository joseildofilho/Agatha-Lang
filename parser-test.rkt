#lang racket
(require agatha/parser
		 agatha/tokenizer
		 brag/support
		 rackunit)

(check-equal?
  (parse-to-datum
	(apply-tokenizer-maker make-tokenizer "// line comment \n"))
  '(agatha-program)
  )
(check-equal?
  (parse-to-datum (apply-tokenizer-maker make-tokenizer "f = A;"))
  '(agatha-program (agatha-list (agatha-exp "f" "=" (agatha-b-exp "A")";"))))
(check-equal?
  (parse-to-datum
	(apply-tokenizer-maker make-tokenizer "f = A; g = B;"))
  '(agatha-program 
	 (agatha-list 
	   (agatha-exp "f" "=" (agatha-b-exp "A") ";") 
	   (agatha-exp "g" "=" (agatha-b-exp "B") ";"))))
(check-equal?
  (parse-to-datum (apply-tokenizer-maker make-tokenizer "f = ¬A;"))
  '(agatha-program 
	 (agatha-list (agatha-exp "f" "=" (agatha-b-exp "not" "A")";"))))
(check-equal?
  (parse-to-datum (apply-tokenizer-maker make-tokenizer "f = ¬A and ¬B or C -> D;"))
  '(agatha-program 
	 (agatha-list (agatha-exp "f" "=" (agatha-b-exp "not" "A" "and" "not" "B" "or" "C" "->" "D")";"))))
(check-equal?
  (parse-to-datum (apply-tokenizer-maker make-tokenizer "#f;"))
  '(agatha-program 
	 (agatha-list (agatha-command "#" "f" ";"))))
(check-equal?
  (parse-to-datum (apply-tokenizer-maker make-tokenizer "f = (A and B);"))
  '(agatha-program 
	 (agatha-list
	   (agatha-exp 
		 "f" "=" 
		 (agatha-b-exp "("(agatha-b-exp "A" "and" "B")")")";"
	   )
	 )
  )
)
(check-equal?
  (parse-to-datum (apply-tokenizer-maker make-tokenizer "f = A and (A or B);"))
  '(agatha-program 
	 (agatha-list
	   (agatha-exp 
		 "f" "=" 
		 (agatha-b-exp "A" "and" "("
					   (agatha-b-exp "A" "or" "B")
					   ")")";"))))
