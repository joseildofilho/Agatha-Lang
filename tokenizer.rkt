#lang racket

(require racket/contract brag/support)

(module+ test (require rackunit))

(define (agatha-token? x)
  (or (eof-object? x) (string? x ) (token-struct? x)))

(module+ test
		 (check-true (agatha-token? eof))
		 (check-true (agatha-token? "Olá"))
		 (check-true (agatha-token? (token 'TOKENTOKEN "Olá")))
		 (check-false (agatha-token? 42))
		 )
(define-lex-abbrev operators (:or "=" "^" "+" "¬" "->" "#" "!"))
(define operators-names (hash
						  "=" "="
						  "^" "and"
						  "+" "or"
						  "¬" "not"
						  "->" "->"
						  "#" "#"))

(define (make-tokenizer port)
  (define (next-token)
	(define agatha-lexer
	  (lexer
		[(:or (from/to "//" "\n") whitespace) (next-token)]
		[(repetition 0 128 lower-case) (token 'VARIABLE lexeme)]
		[(repetition 0 128 upper-case) (token 'VARIABLE lexeme)]
		[operators (token 'OP (hash-ref operators-names lexeme))]
		[";" (token 'ENDDEF lexeme)]
		[(char-set "()") (token lexeme lexeme)]
		[any-char lexeme]
		)
	  )
	(agatha-lexer port)
	)
  next-token
  )

(provide (contract-out 
		   [make-tokenizer
			 (input-port? . -> . (-> agatha-token?))
			 ]))
