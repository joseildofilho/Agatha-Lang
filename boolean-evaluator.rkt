#lang racket

(require racket/contract)

;;; Truth-table generator.
;;; Takes a fully-parenthesized logical expression in infix notation with operators
;;;     not, or, and, ->, <->
;;; and returns the LaTeX code for a centered truth table for the expression.
;;; Assumes that the operators above correspond to the following LaTeX macros:
;;;    \lognot, \logor, \logand, \logthen, \logiff
;;; Does NOT check that the expression is well formed.

;;; Code for the truth table computation is separate from code that generates LaTeX
;;; Code for the truth table computation starts here

; Returns the Racket function corresponding to the given operator symbol.
; Operators or and and must be wrappend inside lambda declarations because
; in Racket they are macros, not functions 
(define (function operator)
  (cond ((equal? operator 'not) not)
        ((equal? operator 'or) (lambda (p q) (or p q)))
        ((equal? operator 'and) (lambda (p q) (and p q)))
        ((equal? operator '->) (lambda (p q) (cond (p q) (else #t))))
        ((equal? operator '<->) (lambda (p q) (or (and p q) (and (not p) (not q)))))
        (else (error 'function "found unknown operator '~a" operator))))

; Makes a list of n lists, each with 2^n (Racket) truth values,
; ordered in the standard way
(define (make-proposition-table n)
  (cond ((= n 1) (list '(#t #f)))
        (else (let ((replicate (lambda (value) (make-list (expt 2 (- n 1)) value))))                     
                (cons (append (replicate #t) (replicate #f))
                      (map (lambda (col) (append col col))
                           (make-proposition-table (- n 1))))))))

; Holds all the information needed for a column of the truth table
(struct column (header values order))

; Given a list of truth-table columns, adds the appropriate number
; of open and closed parentheses to the column headers
(define (parenthesize column-list)
  (define (wrap left tk right)
    (list (column (format "~a~a~a" left (column-header tk) right)
                 (column-values tk)
                 (column-order tk))))
  (append (wrap "(" (first column-list) "")
          (cond ((> (length column-list) 2)
                 (rest (take column-list (- (length column-list) 1))))
                (else '()))
          (wrap "" (last column-list) ")")))

; Makes a list of the propositional symbols contained in the expression
(define (make-proposition-list expression)
  (cond ((symbol? expression) (list expression))
        (else (sort
               (remove-duplicates
                (flatten
                 (cond ((= 2 (length expression))
                        (make-proposition-list (second expression)))
                       ((= 3 (length expression))
                        (append (make-proposition-list (first expression))
                                (make-proposition-list (third expression))))
                       (else (error 'make-proposition-list
                                    "only accepts unary or binary operators")))))
               symbol<?))))

; Holds all the information needed for a truth table in row-major order
(struct truth-table (headers rows orders n-props))
(provide (struct-out truth-table))

; Takes an m-list of n-lists and transposes it to an n-list of m-lists.
; Does NOT check that the lengths of the lists in the input list are equal.
; WARNING: This function may generate headaches if read by the unprepared.
; It contains a map and a lambda inside a map inside a lambda inside a foldl
(define (transpose list-of-lists)
  (foldl (lambda arg (map (lambda x (apply append x))
                          (last arg)
                          (map list (first arg))))
         (make-list (length (first list-of-lists)) '())
         list-of-lists))

; Finds the first position of item x (as compared with equal?) in the given list lst
(define (first-position-of x lst)
  (cond ((empty? lst) (error 'first-position-of "~a not found" x))
        ((equal? x (first lst)) 0)
        (else (+ 1 (first-position-of x (rest lst))))))

; Makes the truth table for the given expression
(provide (contract-out [make-truth-table (list? . -> . truth-table?)]))
(define (make-truth-table expression)
  (define proposition-symbols (make-proposition-list expression))
  (define n-props (length proposition-symbols))
  (define proposition-values (make-proposition-table n-props))
  ; The function truth-info collects all the information in column-major order
  (define (truth-info expression (order 1))
    (cond ((symbol? expression)       ; Must be a propositional symbol if we get here
           (let ((values (list-ref proposition-values
                                   (first-position-of expression proposition-symbols))))
             (list (list (column (format "~a" expression)
                                (make-list (length (first proposition-values)) 0)
                                0))
                   values
                   order)))
          ((= (length expression) 2)  ; Expression with a unary operator
           (let* ((right (truth-info (last expression) order))
                  (new-values (map (function (first expression)) (second right))))
             (list (parenthesize (append (list (column (format "~a" (first expression))
                                                      new-values
                                                      (third right)))
                                         (first right)))
                   new-values
                   (+ (third right) 1))))
          ((= (length expression) 3)  ; Expression with a binary operator
           (let* ((left (truth-info (first expression) order))
                  (right (truth-info (last expression) (third left)))
                  (new-values (map (function (second expression)) (second left) (second right))))
             (list (parenthesize (append (first left)
                                         (list (column (format "~a" (second expression))
                                                      new-values
                                                      (third right)))
                                         (first right)))
                   new-values
                   (+ (third right) 1))))
          (else (error 'truth-info "only accepts unary or binary operators"))))
  ; Only need the first item of the resulting information list
  ; (the rest is used internally by truth-info)
  (let ((formula-columns (first (truth-info expression))))
    ; Package the table information into a truth-table structure in row-major order
    (truth-table (append (map (lambda (p) (format "~a" p)) proposition-symbols)
                         (map column-header formula-columns))
                 (transpose (append proposition-values (map column-values formula-columns)))
                 (append (make-list n-props 0)
                         (map column-order formula-columns))
                 n-props)))

;;; Code for the conversion to LaTeX starts here

; Replaces operator symbols with the corresponding LaTeX macros and wraps into math mode
(define (replace str in out)
  (let ((pos (first (regexp-match-positions in str))))
    (string-append "$"
                   (substring str 0 (car pos))
                   out
                   (substring str (cdr pos))
                   "$")))

; Converts an operator, Boolean, or number to an appropriate LaTeX string.
; Zeros are placeholders, and are converted to the empty string
(define (latex str)
  (cond ((boolean? str) (cond (str "\\logT") (else "\\logF")))
        ((number? str) (cond ((= str 0) "") (else (format "$~a$" str))))
        ((regexp-match? "not" str) (replace str "not" "\\lognot"))
        ((regexp-match? "or" str) (replace str "or" "\\logor"))
        ((regexp-match? "and" str) (replace str "and" "\\logand"))
        ((regexp-match? "<->" str) (replace str "<->" "\\logiff"))
        ((regexp-match? "->" str) (replace str "->" "\\logthen"))
        ((string? str) (string-append "$" str "$"))
        (else str)))

; Converts a truth-table structure to a LaTeX table centered on the page
(define (make-latex table)
  (let* ((n-props (truth-table-n-props table))
         (n-cols (length (truth-table-headers table)))
         (n-forms (- n-cols n-props))
         (headers (truth-table-headers table))
         (rows (truth-table-rows table))
         (orders (truth-table-orders table)))
    (printf "\\begin{center}\n\t\\begin{tabular}{*{~a}{c}|*{~a}{c}}\n" n-props n-forms)
    (printf "\t\t")
    (map (lambda (header) (printf "~a & " (latex header))) (take headers (- n-cols 1)))
    (printf "~a \\\\\\hline\n" (latex (last headers)))
    (let ((print-row
           (lambda (row)
             (begin
               (printf "\t\t")
               (map (lambda (item) (printf "~a & " (latex item))) (take row (- n-cols 1)))
               (printf "~a \\\\" (latex (last row)))))))
      (map (lambda (row) (begin (print-row row) (printf "\n"))) (take rows (- (length rows) 1)))
      (print-row (last rows))
      (printf "\\hline\n")
      (print-row orders)
      (printf "\n\t\\end{tabular}\n\\end{center}\n\n"))))

;;; Combine the two parts
(define (make-latex-truth-table expression)
  (make-latex (make-truth-table expression)))

;;; End of truth-table generator code

;;; The first two are for the assignment itself, not for its solution
;(define e1 '((P and Q) -> P))
;(define e2 '(((P and (not Q)) or ((not P) and R)) -> (Q or R)))

;(define table1 (make-latex-truth-table e1))
;(define table2 (make-latex-truth-table e2))

;;; Problem 1
;(define 1.a '(((P -> Q) -> Q) -> ((Q -> P) -> P)))
;(define 1.b '(((P and Q) -> R) -> ((P -> R) or (Q -> R))))

;(define table-1.a (make-latex-truth-table 1.a))
;(define table-1.b (make-latex-truth-table 1.b))

;;; Problem 2
;(define 2.a '(\\phi -> (\\phi or \\psi)))
;(define 2.b '(\\psi -> (\\phi -> \\psi)))
;(define 2.c '((not (not \\phi)) <-> \\phi))

;(define table-2.a (make-latex-truth-table 2.a))
;(define table-2.b (make-latex-truth-table 2.b))
;(define table-2.c (make-latex-truth-table 2.c))
