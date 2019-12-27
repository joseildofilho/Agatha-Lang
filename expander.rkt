#lang br/quicklang

(require agatha/parser-boolean-parentesis)
(require agatha/boolean-evaluator agatha/symbolize)
(require agatha/utils)

(define-macro (agatha-expander PARSE-TREE)
			  #'(#%module-begin
				 PARSE-TREE))

(define-macro (agatha-program AGATHA-LIST)
			  #'(displayln AGATHA-LIST))
(provide agatha-program)

(define-macro (agatha-list AGATHA-EXP ...)
			  #'(for/fold
				  ([current-state (hash)])
				   ([line (in-list (list AGATHA-EXP ...))])
				   (line current-state)))
(provide agatha-list)

(struct variable (expr [table #:mutable]) #:transparent)
(define-macro (agatha-exp AGATHA-B-EXP ... ";")
			  #'(lambda (memory)
				  (define expr (list AGATHA-B-EXP ...))
				  (define var-def (variable (car (list-tail expr 2)) #f))
				  (hash-set memory (first expr)	var-def)
				  )
			  )
(provide agatha-exp)

(define-macro-cases agatha-b-exp
	[(agatha-b-exp A ... "(" B ... ")" C ... ) 
	 #'(infix (list A ... B ... C ...))]
	[(agatha-b-exp A ...) #'(infix (list A ...))]
)
(provide agatha-b-exp)

(define-macro-cases agatha-command
	[(agatha-command "#" COMMAND ";") 
	 #'(lambda (memory) 
		 (define var (hash-ref memory COMMAND))
		 (define expr (variable-expr var))
		 (define expr-sym (list-string->list-symbol expr))
		 (define truth-table-gen (make-truth-table expr-sym))
		 (set-variable-table! var truth-table-gen)
		 (displayln (truth-table-headers truth-table-gen))
		 (display-listln (truth-table-rows truth-table-gen))
		 memory
		 )])
(provide agatha-command)

(define (evaluate-boolean-exp var memory)
  (displayln var)
  (displayln "Zask")
  (displayln (first (hash-ref memory var)))
  (datum->syntax #f (car(hash-ref memory var)))
  )


(provide (rename-out [agatha-expander #%module-begin]))
