(load "simpleParser.scm")
(load "state_ops.scm")


; The basic outer Mstate.
; Iterates through ((expr) (expr) (expr)) and executes each expression
; in turn, passing its state value along.
; Returns whatever is returned from the final expression evaluated, which
; ought to be a return function.
; If it gets an expression, i.e. (= x 5), rather than a list of expressions,
; it updates the state as appropriate rather than diving any deeper.
(define Mstate
    (lambda (expr s)
        (if (null? expr)
            s
            (if (list? (car expr))
                (Mstate (cdr expr) (Mstate (car expr) s))
                (cond
                    ((equal? (car expr) 'var)   (Mstate_var expr s))
                    ((equal? (car expr) '=)     (Mstate_eq expr s))
                    ((equal? (car expr) 'return) (Mstate_return expr s))
                    ((equal? (car expr) 'if)    (Mstate_if expr s))
                )
            )
        )
))

; Two possibilities.
; (var x) has length 2, so cddr will be null and just add x to inittable.
; (var x value) has length 3, so cddr will not be null.
; then you add x to inittable and add its calculated value as a binding.
; The calculated value is (Mvalue (caddr expr) s) or Mvalue(value)
; Init is false if no value, true if there is a value.
(define Mstate_var
    (lambda (expr s)
        (cond
            ((eq? (get_init (cadr expr) s) #t) (error "Redefining variable"))
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
(define Mstate_eq
    (lambda (expr s)
        (if (eq? (get_init (cadr expr) s) 'error)
            (error "Variable assignment before declaration")
            (set_binding (cadr expr) (right_op_val expr s)
                (set_init (cadr expr) #t s))
        )
))

; Takes (return (+ 3 x)) and return 3+x, NOT a state!
; Since this doesn't return a state, it's assuming that it is the last thing
;    called and that at this point the user wants a value, not a state.
; TODO: move the true/false conversion logic to new function
(define Mstate_return
    (lambda (expr s)
        (cond
            ((eq? (Mvalue (cadr expr) s) #t) 'true)
            ((eq? (Mvalue (cadr expr) s) #f) 'false)
            (else (Mvalue (cadr expr) s))
        )
    )
)

; Takes (if (cond) (then-expr) (else-expr))
; First evaluates the boolean condition.  If it's true, return Mstate(then-expr)
; If it's false, return just the state unchanged if no else statement, and
;  Mstate(else-expr) if there is an else statement.
(define Mstate_if
    (lambda (expr s)
        (if (Mboolean (cadr expr) s)
            (Mstate (caddr expr) s)
            (if (null? (cdddr expr))
                s
                (Mstate (cadddr expr) s)
            )
        )
))

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
            ((not (list? expr)) (get_binding_safe expr s))
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
            ((not (list? expr)) (get_binding_safe expr s))
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
        (Mstate (parser filename) new_state)
    )
)


; Test code
;(Mstate '() new_state)
;(Mstate '( (var x) (var y 10) (var z (+ y y)) (var err (+ y x)) ) new_state)
;(Mstate '( (var x) (var y 10) (= x (+ y 10)) ) new_state)
;(Mstate '( (var x 5) (var y) (= y (+ x 3)) (return y)) new_state)
;(Mstate '( (var y (&& #t #f)) (return y) ) new_state)

;(Mstate (parser "p1test1") new_state)
