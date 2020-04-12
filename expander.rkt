#lang br/quicklang

(require agatha/parser-boolean-parentesis)
(require agatha/boolean-evaluator agatha/symbolize)
(require agatha/utils)

(define-macro (agatha-expander PARSE-TREE)
			  #'(#%module-begin
				 PARSE-TREE))

(define-macro (agatha-program AGATHA-LIST)
			  #'(void AGATHA-LIST))
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
                              (define variables
                                (if (not (list? (car (list-tail expr 2))))
                                    (list (car (list-tail expr 2)))
                                    (car (list-tail expr 2)))
                                )
                              (define variable-name (first expr))
                              

                              (define (rpc-string-list expr)
                                (map (lambda (v)
                                       (if (list? v)
                                           (rpc-string-list v)
                                           (if (and (string? v) (hash-has-key? memory v))
                                               (variable-expr (hash-ref memory v))
                                               v)))
                                     expr))
                              (print "Aqui pow Dinovo")
                              (define var (variable (rpc-string-list variables) #f))
                              
                              (define memory_back (hash-set memory variable-name var))
                              memory_back
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
             (define expr-var (list-string->list-symbol (variable-expr var)))
             (define truth-table-gen (make-truth-table expr-var))
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
