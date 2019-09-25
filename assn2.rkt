#lang plai
; uncomment the following line to hide all passing tests
 (print-only-errors)

;; ==========================================================
;;                     EBNF & DEFINE-TYPES
;; ==========================================================

; Assignment 2 : With names and renames. 
;
;   <ReWAE> ::= <number>
;          | { + <ReWAE> <ReWAE> }
;          | { - <ReWAE> <ReWAE> }
;          | { * <ReWAE> <ReWAE> }
;          | { begin <ReWAE> <ReWAES> }
;          | { if0 <ReWAE> then <ReWAE> else <ReWAE> }
;          | { with { <id> <ReWAE> } <ReWAE> }
;          | { lazy-with { <id> <ReWAE> } <ReWAE> }
;          | { rename { <id> as <id> } in <ReWAE> }
;          | { with* { <WithMappings> } <ReWAE> }
;          | <id>
;
;    <WithMappings> ::=
;                     | { <id> <ReWAE> } <WithMappings>
;
;    <ReWAES> ::=
;               |  <ReWAE> <ReWAES>


; This will be the abstract syntax of our language.
; REMINDER: YOU CANNOT MAKE ANY CHANGES TO THE DEFINE-TYPE
; We will take marks for changes in this definition.

(define-type ReWAE     
  [num (n number?)]
  [binop (op procedure?) (lhs ReWAE?) (rhs ReWAE?)]
  [if0 (scrutinee ReWAE?) (then-expr ReWAE?) (else-expr ReWAE?)] 
  [with (id symbol?) (named-expr ReWAE?) (body ReWAE?)]
  [lazy-with (id symbol?) (named-expr ReWAE?) (body ReWAE?)]
  [rename-as (source-id symbol?) (dest-id symbol?) (body ReWAE?)]
  [begin-exp (exprs (non-empty-listof ReWAE?))]
  [id (name symbol?)]
  )



;; ==========================================================
;;                           PARSE
;; ==========================================================
; TODO You must complete this list of reserved symbols.
(define *reserved-symbols* '(+ - with * begin if0 lazy-with rename then else as in num))

;; We invite you to reuse the valid-identifier predicate that we have
;; seen in lectures before:

;; valid-identifier? : any -> boolean
;; Determines whether the parameter is valid as an identifier name, i.e.,
;; a symbol that is not reserved.
(define (valid-identifier? sym)
  (and (symbol? sym)
       (not (member sym *reserved-symbols*))))

;; Reserved symbols.
(test (valid-identifier? 'lazy-with) false)
(test (valid-identifier? 'else) false)
(test (valid-identifier? 'x) true)


;; TODO: Implement this function.

;; parse : any -> ReWAE
;; Consumes an s-expression (in ReWAE's concrete syntax)
;; and generates the corresponding ReWAE program.

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? valid-identifier? sexp) (id sexp)] ;; couldn't everything be a valid identifier then? 
    [(list '+ lexp rexp) (binop + (parse lexp) (parse rexp))]
    [(list '- lexp rexp) (binop - (parse lexp) (parse rexp))]
    [(list '* lexp rexp) (binop * (parse lexp) (parse rexp))]
    [(list 'if0 cond 'then t 'else e) (if0 (parse cond)
                                           (parse t)
                                           (parse e))]
    [(list 'rename (list source 'as dest) 'in body) (rename-as source
                                                   dest
                                                   (parse body))]
    [(list 'begin exp exprs ...) (begin-exp (cons (parse exp) (map parse exprs)))]
    [(list 'with (list (? valid-identifier? id) expr) body) (with id (parse expr) (parse body))]
    [(list 'lazy-with (list (? valid-identifier? id) expr) body) (lazy-with id (parse expr) (parse body))]
    [(list 'with* a body) (parse (first (with-star-helper a body)))]        

;     (cond [(empty? a) (parse body)] ;; {with* {} body}
;                                [(not (list? (first a))) (error "")];; {with* {x 1} body}
;                                [else
;                                 (with (first (first a)) (parse (rest (first a))) (parse (rest a)))])]


                                 ;(parse (with (first (first a)) (parse (rest (first a))) (parse body)))])]

                                
                                 ;(with '(first (first a)) (parse (rest (first a)))
                                            ;(parse (with (rest a) body)))])]
;    [(list 'with* a body) (with (first (first a)) (map (lambda (i)
;                                                         (with ((first i) (parse (second i))) (rest a)))
;                                                       (rest a)) body)] ;; write tests 
    [else (error "")]))

;; with-star-helper: (list symbol ReWAE) ReWAE -> ReWAE

(define (with-star-helper a body)
  (cond [(empty? a) (list body)]
        [else
         (list (cons 'with
               (cons (first a)
                     (with-star-helper (rest a) body))))]))
; test number:
(test (parse '1) (num 1))

; test identifier
(test (parse 'y) (id 'y))


; test addition/subtraction/multiplication
(test (parse '{+ 1 2}) (binop + (num 1) (num 2)))
(test (parse '{- 6 2}) (binop - (num 6) (num 2)))
(test (parse '{* 6 2}) (binop * (num 6) (num 2)))

; test if0 number
(test (parse '{if0 0 then 1 else 0}) (if0 (num 0)
                                           (num 1)
                                           (num 0)))
(test (parse '{if0 1 then 1 else 0}) (if0 (num 1)
                                           (num 1)
                                           (num 0)))
(test (parse '{if0 {+ 5 6} then {* 3 4} else {- 7 3}}) (if0 (binop + (num 5) (num 6))
                                                            (binop * (num 3) (num 4))
                                                            (binop - (num 7) (num 3)))) ;; more if0 tests? with more complex ReWAES?

;; rename tests
(test (parse '{rename {k as b} in {with {k 2} k}}) (rename-as 'k 'b (with 'k (num 2) (id 'k)))) 

;; begin tests
(test (parse '{begin {with {x 5} {with {y 1} y}} 5}) (begin-exp (list (with 'x (num 5) (with 'y (num 1) (id 'y)))
                                                                      (num 5))))

(test (parse '{begin {+ 5 6} {* 2 3}}) (begin-exp (list (binop + (num 5) (num 6))
                                                                  (binop * (num 2) (num 3)))))


                                                  

;; with tests
 

;; lazy-with tests

#;
[(list 'with* a body) (with (first (first a)) (map (lambda (i)
                                                         (with ((first i) (parse (second i))) (rest a)))
                                                       (rest a)) body)]

;;with* tests

(test (parse '{with* {{x 1}} 1}) (with 'x (num 1) (num 1)))


(test (parse '{with* {{x 1} {x 2}} x})
      (with 'x (num 1)
            (with 'x (num 2) (id 'x))))


(test (parse '{with* {{x 1} {y 2}} {+ x y}})
      (with 'x (num 1) (with 'y (num 2) (binop + (id 'x) (id 'y)))))
;
;
;(test (parse '{with* {{x {+ 1 2}} {y {+ x 1}}} {+ y 0}}) (with 'x (binop + (num 1) (num 2))
;                                                               (with 'y (binop + (id 'x) (num 1))
;                                                                     (binop + (id 'y) (num 0)))))
;



; We STRONGLY recommend you to add more tests to this function!
(test (parse 'x) (id 'x))
(test/exn (parse '+) "")
(test/exn (parse '{-}) "")
(test/exn (parse '{* 1}) "")
(test/exn (parse '{+ 1 2 3}) "")

; ill-structured lazy-with
(test/exn (parse '{lazy-with}) "")
(test/exn (parse '{lazy-with x}) "")
(test/exn (parse '{lazy-with x 2 3}) "")
(test/exn (parse '{lazy-with {x 1}}) "")
(test/exn (parse '{lazy-with {x 1} 2 3}) "")
(test/exn (parse '{lazy-with {x 1 2} 3}) "")
(test/exn (parse '{lazy-with {+ 1} 2}) "")

; + (and -/with) with non-AEs as arguments
(test/exn (parse '{lazy-with {x "foo"} x}) "")
(test/exn (parse '{lazy-with {x 1} "foo"}) "")

(test (parse '{with {x 1} x})
      (with 'x (num 1) (id 'x)))
(test (parse '{with {x 1} {with {x 2} x}})
      (with 'x (num 1)
            (with 'x (num 2) (id 'x))))
(test (parse '{lazy-with {x 1} x})
      (lazy-with 'x (num 1) (id 'x)))
(test (parse '{lazy-with {x 1} {lazy-with {x 2} x}})
      (lazy-with 'x (num 1)
            (lazy-with 'x (num 2) (id 'x))))

(test (parse '{rename {x as y} in
                      x})
      (rename-as 'x 'y (id 'x)))

(test (parse '{with {x 10}
                    {rename {x as y} in
                            x}})
      (with 'x (num 10)
            (rename-as 'x 'y (id 'x))))

(test (parse '{* {- 1 2} {+ 3 4}})
      (binop *
             (binop - (num 1) (num 2))
             (binop + (num 3) (num 4))))

; These tests check for errors generated explicitly by you via (error ...)
; TODO: write more tests for checking that parse generates errors when needed
(test/exn (parse "I am a string, not a symbol") "")
(test/exn (parse '{with {} 1}) "")
(test/exn (parse '{with {{1 x}} x}) "")
(test/exn (parse '{with* {x 1} x}) "")
(test/exn (parse 'with) "")
(test/exn (parse '{a b c}) "")
(test/exn (parse '+) "")
(test/exn (parse '{+ "not" "number"}) "")
(test/exn (parse '{begin "not" "list"}) "")
(test/exn (parse '{begin {{+ 2 3} {+ "not" "number"}}}) "")



;; ==========================================================
;;                           INTERP
;; ==========================================================

#;
(define-type ReWAE     
  [num (n number?)]
  [binop (op procedure?) (lhs ReWAE?) (rhs ReWAE?)]
  [if0 (scrutinee ReWAE?) (then-expr ReWAE?) (else-expr ReWAE?)] 
  [with (id symbol?) (named-expr ReWAE?) (body ReWAE?)]
  [lazy-with (id symbol?) (named-expr ReWAE?) (body ReWAE?)]
  [rename-as (source-id symbol?) (dest-id symbol?) (body ReWAE?)]
  [begin-exp (exprs (non-empty-listof ReWAE?))]
  [id (name symbol?)]
  )

;; subst : ReWAE symbol ReWAE -> ReWAE
;; substitute out target-id for val everywhere it
;; is referenced "free" in target-exp.
(define (subst target-exp target-id val)
  (type-case ReWAE target-exp
    [num (n) target-exp]
    [binop (op l r) (binop op (subst l target-id val)
                        (subst r target-id val))]
    [if0 (scrutinee t e) (if0 (subst scrutinee target-id val)
                         (subst t target-id val)
                         (subst e target-id val))]
     [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id target-id)
              (with bound-id
                    (subst named-expr target-id val)
                    bound-body)
              (with bound-id
                    (subst named-expr target-id val) 
                    (subst bound-body target-id val)))]
    [lazy-with (bound-id named-expr bound-body)
               (if (symbol=? bound-id target-id)
              (lazy-with bound-id
                    (subst named-expr target-id val)
                    bound-body)
              (lazy-with bound-id
                    (subst named-expr target-id val) 
                    (subst bound-body target-id val)))]  
    [rename-as (source-id dest-id body) (if (symbol=? target-id source-id)
                                            (subst body dest-id val) 
                                            target-exp)]


    
    [begin-exp (exprs) (with target-id val (subst exprs target-id val))]                           ;; TODO
    [id (v) (if (symbol=? v target-id) val target-exp)]))
    


(test (subst (num 10) 'x (num 1)) (num 10))
(test (subst (id 'x) 'x (num 1)) (num 1))
(test (subst (id 'x) 'x (num 2)) (num 2))
(test (subst (id 'y) 'x (num 1)) (id 'y))
(test (subst (num 10) 'x (num 1)) (num 10))
(test (subst (with 'x (num 2) (num 3)) 'x (num 1))
      (with 'x (num 2) (num 3)))
(test (subst (with 'x (id 'x) (id 'x)) 'x (num 1))   
      (with 'x (num 1) (id 'x)))

;(test (subst (rename-as 'y 'z (binop + (num 0) (id 'z))) 'y (num 5)) (with 'z (id 'y) (binop + (id 'z) (num 0))))

;; TODO The test provided is just an example.
;;      You must provide more tests for subst!


;; TODO you must also write the interp function!

#;
(define-type ReWAE     
  [num (n number?)]
  [binop (op procedure?) (lhs ReWAE?) (rhs ReWAE?)]
  [if0 (scrutinee ReWAE?) (then-expr ReWAE?) (else-expr ReWAE?)] 
  [with (id symbol?) (named-expr ReWAE?) (body ReWAE?)]
  [lazy-with (id symbol?) (named-expr ReWAE?) (body ReWAE?)]
  [rename-as (source-id symbol?) (dest-id symbol?) (body ReWAE?)]
  [begin-exp (exprs (non-empty-listof ReWAE?))]
  [id (name symbol?)]
  )

;; interp : ReWAE -> number
;; consumes a ReWAE and computes the corresponding number
(define (interp an-ae)
  (type-case ReWAE an-ae
    [num (n) n]
    [binop (op l r) (op (interp l) (interp r))]
    [if0 (scrutinee t e) (if (= 0 (interp scrutinee))
                             (interp t)
                             (interp e))]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body
                         bound-id
                         (num (interp named-expr))))]
    [lazy-with (bound-id named-expr bound-body)
               (interp (subst bound-body
                              bound-id
                       ;; change this so not interpreting before substituting - how?
                              named-expr))] ;before: (num (interp named-expr))
    [rename-as (source dest body) (error "")] ; (subst target-exp target-id val) ;; rename x as y in (+ x 3)
    [begin-exp (exprs) (cond [(empty? exprs) (error "")]
                             [(= 1 (length (exprs))) (interp (first (exprs)))]
                             [else
                              (foldl (lambda (elem v)
                                       (+ v (if (equal? (first (reverse (exprs))) elem)
                                                (interp elem) ;; if at last of list, interp elem and keep result
                                                (if (not (equal? (interp elem) 0))
                                                    0
                                                    (interp elem)))))
                                                
                                     0
                                     exprs)])]
    [id (v) (error "")] 
    ))


(test (interp (lazy-with 'x (binop + (num 4) (id 'y))
                         (num 5)))
      5)

; TODO: write more tests for interp as needed
(test (interp (with 'x (num 1) (id 'x))) 1)
(test (interp (binop - (num 8) (num 3))) 5)
(test (interp (binop -
                     (binop * (num 5) (num 6))
                     (binop + (num -10) (num 2))))
      38)
(test (interp (with 'x (num 1) (num 2))) 2)
(test (interp (with 'x (binop + (num 1) (num 2)) (id 'x))) 3)
(test (interp (with 'x (num 1) (id 'x))) 1)
(test (interp (with 'x (num 1)
                    (with 'x (num 2) (id 'x)))) 2)
(test (interp (with 'x (num 1)
                    (with 'x (id 'x) (id 'x))))
      1)
(test/exn (interp (id 'y)) "")
