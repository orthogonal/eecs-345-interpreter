(load "simpleParser.scm")
(load "state_ops.scm")
(parser "p1test1")

(define Mstate
    (lambda (expression s)
        (cond
            ((eq? '= (operator expression)) (

(define Mvalue
    (lambda (expression s)
        (cond
            ((number? expression) expression)
            ((eq? 
