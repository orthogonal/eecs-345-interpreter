(load "simpleParser.scm")
;(load "state_ops.scm")


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
      ((null? expr) s)
      ((list? (car expr))(Mstate-cps (cdr expr) (Mstate-cps (car expr) s return) return))
      ((equal? (car expr) 'var)                 (Mstate_var-cps expr s return))
      ((equal? (car expr) '=)                   (Mstate_eq-cps expr s return))
      ((equal? (car expr) 'return)              (Mstate_return-cps expr s return))
      ((equal? (car expr) 'if)                  (Mstate_if-cps expr s return))
      ((equal? (car expr) 'while)               (Mstate_while-cps (cadr expr) (caddr expr) s return))
      ((equal? (car expr) 'begin)               (Mstate_begin-cps (cdr expr) s return))
      (else (return (car expr) s))
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
            ((eq? (get_init (cadr expr) s (getStateList s)) #t) (error "Redefining variable"))
            ((null? (cddr expr)) (set_init (cadr expr) 'false s))
            (else (set_binding (cadr expr) (Mvalue (caddr expr) s)
                (set_init (cadr expr) #t s)))
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
        (if (eq? (get_init (cadr expr) s (getStateList s)) 'error)
            (error "Variable assignment before declaration")
            (set_binding (cadr expr) (right_op_val expr s)
                (set_init (cadr expr) #t s))
        )
))

; Takes (return (+ 3 x)) and return 3+x, NOT a state!
; Since this doesn't return a state, it's assuming that it is the last thing
;    called and that at this point the user wants a value, not a state.
(define Mstate_return-cps
    (lambda (expr s return)
      (add 'return (Mvalue (cadr expr) s) s)
    )
)

; Takes (if (cond) (then-expr) (else-expr))
; First evaluates the boolean condition.  If it's true, return Mstate(then-expr)
; If it's false, return just the state unchanged if no else statement, and
;  Mstate(else-expr) if there is an else statement.
(define Mstate_if-cps
  (lambda (expr s return)
    (cond
      ((Mboolean (cadr expr) s)(Mstate-cps (caddr expr) s return))
      ((null? (cdddr expr)) s)
      (else (Mstate-cps (cadddr expr) s return))
      )
    )
  )

;Takes a begin statement
;This is where layers of code are computed
;We just feed a new state in front of the other things to "store" them
;Then we need to merge the tables back together

(define Mstate_begin-cps
  (lambda (expr s return)
    (unionStates (Mstate-cps expr (append new_state s) return) s)
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
								  ((eq? v 'continue)s2)
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
            ((not (list? expr)) (get_binding_safe expr s (getStateList s)))
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
            ((not (list? expr)) (get_binding_safe expr s (getStateList s)))
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
        (cond
          ((eq?  (table_search 'return (Mstate-cps (parser filename) new_state (lambda(v)v))) #t) 'true)
          ((eq?  (table_search 'return (Mstate-cps (parser filename) new_state (lambda(v)v))) #f) 'false)
          (else (table_search 'return (Mstate-cps (parser filename) new_state (lambda(v)v))))
          )
    )
)


; Test code
;(Mstate '() new_state)
;(Mstate '( (var x) (var y 10) (var z (+ y y)) (var err (+ y x)) ) new_state)
;(Mstate '( (var x) (var y 10) (= x (+ y 10)) ) new_state)
;(Mstate '( (var x 5) (var y) (= y (+ x 3)) (return y)) new_state)
;(Mstate '( (var y (&& #t #f)) (return y) ) new_state)

;(Mstate (parser "p1test1") new_state)