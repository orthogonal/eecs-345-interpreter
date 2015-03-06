(load "simpleParser.scm")
(load "state_ops.scm")


; The basic outer Mstate.
; Iterates through ((expr) (expr) (expr)) and executes each expression
; in turn, passing its state value along.
; Returns whatever is returned from the final expression evaluated, which
; ought to be a return function.
; If it gets an expression, i.e. (= x 5), rather than a list of expressions,
; it updates the state as appropriate rather than diving any deeper.
; If it sees a command it doesnt know how to handle it returns it
(define Mstate-cps
  (lambda (expr s return)
    (cond
      ((null? expr) (return s))
      ((list? (car expr))                       (Mstate-cps (car expr) s 
                                                    (lambda (s1) (Mstate-cps (cdr expr) s1
                                                    (lambda (s2) (return s2))))))
      ((equal? (car expr) 'var)                 (Mstate_var-cps expr s (lambda (s1) (return s1))))
      ((equal? (car expr) '=)                   (Mstate_eq-cps expr s (lambda (s1) (return s1))))
      ((equal? (car expr) 'return)              (Mstate_return-cps expr s (lambda (s1) (return s1))))
      ((equal? (car expr) 'if)                  (Mstate_if-cps expr s (lambda (s1) (return s1))))
      ((equal? (car expr) 'while)               (Mstate_while-cps (cadr expr) (caddr expr) s (lambda (s1) (return s1))))
      ((equal? (car expr) 'begin)               (Mstate_begin-cps (cdr expr) s (lambda (s1) (return s1))))
      (else (return s))
      )))

; Two possibilities.
; (var x) has length 2, so cddr will be null and just add x to inittable.
; (var x value) has length 3, so cddr will not be null.
; then you add x to inittable and add its calculated value as a binding.
; The calculated value is (Mvalue (caddr expr) s) or Mvalue(value)
; Init is false if no value, true if there is a value.
(define Mstate_var-cps
    (lambda (expr s return)
        (cond
            ((eq? (get_init-cps (cadr expr) s (getStateList s) (lambda (v) v)) #t) (error "Redefining variable"))
            ((null? (cddr expr)) (return (set_init (cadr expr) 'false s)))
            (else (return (set_binding (cadr expr) (Mvalue (caddr expr) s)
                (set_init (cadr expr) #t s))))
        )
    )
)

; Takes (= x 5) and
;    if x hasn't been declared, it's not in the init table, so throw an error.
;    set the binding of x to 5.
; If it's an expression like (= x (+ 3 y)) set the binding to the
;   Mvalue evaluation of the right operand expression.
(define Mstate_eq-cps
    (lambda (expr s return)
        (if (eq? (get_init-cps (cadr expr) s (getStateList s) (lambda (v) v)) 'error)
            (error "Variable assignment before declaration")
            (return (set_binding (cadr expr) (right_op_val expr s)
                (set_init (cadr expr) #t s)))
        )
))

; Takes (return (+ 3 x)) and return 3+x, NOT a state!
; Since this doesn't return a state, it's assuming that it is the last thing
;    called and that at this point the user wants a value, not a state.
(define Mstate_return-cps
    (lambda (expr s return)
      (return (add 'return (Mvalue (cadr expr) s) s))
    )
)

; Takes (if (cond) (then-expr) (else-expr))
; First evaluates the boolean condition.  If it's true, return Mstate(then-expr)
; If it's false, return just the state unchanged if no else statement, and
;  Mstate(else-expr) if there is an else statement.
(define Mstate_if-cps
  (lambda (expr s return)
    (cond
      ((Mboolean (cadr expr) s) (Mstate-cps (caddr expr) s (lambda (s1) (return s1))))
      ((null? (cdddr expr)) (return s))
      (else (Mstate-cps (cadddr expr) s (lambda (s1) (return s1))))
      )
    )
  )

;Takes a begin statement
;This is where layers of code are computed
;We just feed a new state in front of the other things to "store" them

(define Mstate_begin-cps
  (lambda (expr s return)
    (Mstate-cps expr (append new_state s) (lambda (s1) (return s1)))
  ))

;Evaluates a body based on a condtion that is true and watches for break
;Takes (< i j) (= i (+ i 1)) s and return
;This part takes care of break and continue statements
;TODO: integrate continue statement

(define Mstate_while-cps
  (lambda(condition body s return)
    (call/cc (lambda (break)
	     (letrec ((loop
			(lambda (condition body s1)
			  (cond
			    ((Mboolean condition s1)(Mstate_while-cps condition body (Mstate-cps body s1
							      (lambda(v s2)
								(cond
								  ((eq? v 'break)(break s2))
								  ((eq? v 'continue)(break (loop condition body s2)))
								  )
								)
							      )
                                                                    return)
			     )
			     (else s1)
			     )
			  )
			))
	       (loop condition body s)
	       )))
    )
  )

; Can be a number, an atom, or a list
; If it's a number, just return the number.
; If it's an atom, return the value of that atom in the list, with error check. 
; It it's a list, check if it's +, -, *, /, %.
;   If it is any of those mathematical operators, return the value as appropriate.
;   For -, if it's just (- 5) then return 0 - 5.
;   For /, if it's 7 / 5, return (7 - (7 % 5)) / 5, or 5 / 5, or 1, for int division.
; If it's none of those, check if it's a boolean operator, and if so return the
;  boolean evaluation of the expression.
; If none of the above, throw an error, expression is invalid.
(define Mvalue
    (lambda (expr s)
        (cond
            ((number? expr) expr)
            ((not (list? expr)) (get_binding_safe-cps expr s (getStateList s) (lambda (v) v)))
            ((equal? (car expr) '+) (+ (left_op_val expr s) (right_op_val expr s)))
            ((equal? (car expr) '-)
                (if (null? (cddr expr)) 
                    (- 0 (left_op_val expr s))
                    (- (left_op_val expr s) (right_op_val expr s))))
            ((equal? (car expr) '*) (* (left_op_val expr s) (right_op_val expr s)))
            ((equal? (car expr) '/) (/ (- (left_op_val expr s) (modulo (left_op_val expr s) (right_op_val expr s))) (right_op_val expr s))) ; Integer division:  (x - (x % y)) / y
            ((equal? (car expr) '%) (modulo (left_op_val expr s) (right_op_val expr s)))
            ((logical_operator? (car expr)) (Mboolean expr s))
            (error "Invalid expression for Mvalue")
        )
))

; Returns the value of the boolean expression expr.
; If expr is #t or #f, just return those.
; If it's something else that's not a list, it must be a variable, so return the
;    value of that variable (presumed to be correct type)
; Otherwise, if it's a boolean expression do the corresponding scheme expression
;    with the Mbooleans of the operands (or operand if it's not)
; and if it's an arithmetic comparison, do the corresponding scheme expression
;    with the Mvalues of the operands.
; NOTE that this uses #t and #f, do we need to use true and false?
(define Mboolean
    (lambda (expr s)
        (cond
            ((boolean? expr) expr)
            ((equal? expr 'true) #t)
            ((equal? expr 'false) #f)
            ((not (list? expr)) (get_binding_safe-cps expr s (getStateList s) (lambda (v) v)))
            ((equal? (car expr) '||) (or (Mboolean (cadr expr) s) (Mboolean (caddr expr) s)))
            ((equal? (car expr) '&&) (and (Mboolean (cadr expr) s) (Mboolean (caddr expr) s)))
            ((equal? (car expr) '!) (not (Mboolean (cadr expr) s)))
            ((equal? (car expr) '>) (> (left_op_val expr s) (right_op_val expr s)))
            ((equal? (car expr) '<) (< (left_op_val expr s) (right_op_val expr s)))
            ((equal? (car expr) '>=) (>= (left_op_val expr s) (right_op_val expr s)))
            ((equal? (car expr) '>=) (>= (left_op_val expr s) (right_op_val expr s)))
            ((equal? (car expr) '==) (eq? (left_op_val expr s) (right_op_val expr s)))
            ((equal? (car expr) '!=) (not (eq? (left_op_val expr s) (right_op_val expr s))))
            (error "Invalid expression for Mboolean")
        )
))

(define left_op_val
    (lambda (expr s)
        (Mvalue (cadr expr) s)
))

(define right_op_val
    (lambda (expr s)
        (Mvalue (caddr expr) s)
))

(define logical_operator?
    (lambda (op)
        (cond
            ((equal? op '||) #t)
            ((equal? op '&&) #t)
            ((equal? op '>) #t)
            ((equal? op '<) #t)
            ((equal? op '>=) #t)
            ((equal? op '<=) #t)
            ((equal? op '==) #t)
            ((equal? op '!=) #t)
            ((equal? op '!) #t)
            (else 'false)
        )
))

(define interpret
    (lambda (filename)
        (letrec ((s (Mstate-cps (parser filename) new_state (lambda (v) v))))
            (cond
                ((eq? (table_search-cps 'return s (lambda (v) v)) #t) 'true)
                ((eq? (table_search-cps 'return s (lambda (v) v)) #f) 'false)
                (else (table_search-cps 'return s (lambda (v) v)))
            )
)))


;  Test Code
(require rackunit)

(check-eq? (interpret "tests/1") 150)
(check-eq? (interpret "tests/2") -4)
(check-eq? (interpret "tests/3") 10)
(check-eq? (interpret "tests/4") 16)
(check-eq? (interpret "tests/5") 220)
(check-eq? (interpret "tests/6") 5)
(check-eq? (interpret "tests/7") 6)
(check-eq? (interpret "tests/8") 10)
(check-eq? (interpret "tests/9") 5)
(check-eq? (interpret "tests/10") -39)
(check-exn exn:fail? (lambda () (interpret "tests/11")))
(check-exn exn:fail? (lambda () (interpret "tests/12")))
(check-exn exn:fail? (lambda () (interpret "tests/13")))
(check-exn exn:fail? (lambda () (interpret "tests/14")))
(check-eq? (interpret "tests/15") 'true)
(check-eq? (interpret "tests/16") 100)
(check-eq? (interpret "tests/17") 'false)
(check-eq? (interpret "tests/18") 'true)
(check-eq? (interpret "tests/p1test1") 200)

(check-eq? (interpret "tests2/1") 100)
(check-eq? (interpret "tests2/2") 20)
(check-eq? (interpret "tests2/3") 6)
(check-eq? (interpret "tests2/4") -1)
(check-eq? (interpret "tests2/6") 2)
(check-eq? (interpret "tests2/7") 164)
(check-exn exn:fail? (lambda () (interpret "tests2/8")))
(check-exn exn:fail? (lambda () (interpret "tests2/9")))
(check-exn exn:fail? (lambda () (interpret "tests2/10")))
(check-eq? (interpret "tests2/11") 12)
(check-eq? (interpret "tests2/12") 32)
