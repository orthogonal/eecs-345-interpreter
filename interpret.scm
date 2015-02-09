(load "simpleParser.scm")



; The basic outer Mstate.
; Iterates through ((expr) (expr) (expr)) and executes each expression
; in turn, passing its state value along.
; Returns whatever is returned from the final expression evaluated, which
; ought to be a return function.
; If it gets an expression, i.e. (= x 5), rather than a list of expressions,
; it updates the state as appropriate rather than diving any deeper.
(define Mstate
    (lambda (expr s)
        (if
            (list? (car expr))
                (Mstate (cdr expr) (Mstate (car expr)))
                (cond
                    ((equal? (car expr) 'var)   (Mstate_var (expr s)))
                    ((equal? (car expr) '=)     (Mstate_eq (expr s)))
                    ((equal? (car expr) 'return) (Mstate_return (expr s)))
                    ((equal? (car expr) 'if)    (Mstate_if (expr s)))
                )
        )
))

; Two possibilities.
; (var x) has length 2, so cddr will be null and just add x to inittable.
; (var x value) has length 3, so cddr will not be null.
; then you add x to inittable and add its value as a binding.
(define Mstate_var
    (lambda (expr s)
        (if
            (null? cddr expr)
