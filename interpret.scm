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
        (if (null? (cddr expr))
            (set_init (cadr expr) 'false s)
            (set_binding (cadr expr) (Mvalue (caddr expr) s)
                (set_init (cadr expr) 'true s))
        )
))

; Takes (= x 5) and
;    if x hasn't been declared, it's not in the init table, so throw an error.
;    set the binding of x to 5.
; If it's an expression like (= x (+ 3 y)) set the binding to the
;   Mvalue evaluation of the right operand expression.
(define Mstate_eq
    (lambda (expr s)
        (if (eq? (get_init (cadr expr) s) 'error)
            (error "Variable assignment before declaration")
            (set_binding (cadr expr) (right_op expr s)
                (set_init (cadr expr) 'true s))
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
            ((equal? (car expr) '+) (+ (left_op expr s) (right_op expr s)))
            ((equal? (car expr) '-)
                (if (null? (caddr expr))
                    (- 0 (left_op expr s))
                    (- (left_op expr s) (right_op expr s))))
            ((equal? (car expr) '*) (* (left_op expr s) (right_op expr s)))
            ((equal? (car expr) '/) (/ (- (left_op expr s) (modulo (left_op expr s) (right_op expr s))) (right_op expr s))) ; Integer division:  (x - (x % y)) / y
            ((equal? (car expr) '%) (modulo (left_op expr s) (right_op expr s)))
            ((logical_operator? (car expr)) (Mboolean expr s))
            (error "Invalid expression for Mvalue")
        )
))

(define left_op
    (lambda (expr s)
        (Mvalue (cadr expr) s)
))

(define right_op
    (lambda (expr s)
        (Mvalue (caddr expr) s)
))

(define logical_operator?
    (lambda (op)
        (cond
            ((equal? op '||) 'true)
            ((equal? op '&&) 'true)
            ((equal? op '>) 'true)
            ((equal? op '<) 'true)
            ((equal? op '>=) 'true)
            ((equal? op '<=) 'true)
            ((equal? op '==) 'true)
            ((equal? op '!=) 'true)
            ((equal? op '!) 'true)
            (else 'false)
        )
))


; Test code
;(Mstate '() new_state)
;(Mstate '( (var x) (var y 10) (var z (+ y y)) (var err (+ y x)) ) new_state)
(Mstate '( (var x) (var y 10) (= x (+ y 10)) ) new_state)
