(load "simpleParser.scm")
(parser "p1test1")


; Step through a table represented as ((var val) (var val) (var val))
; If the name of the var matches the var argument, return val
; If it's not in the table return 'error
(define table_search
    (lambda (var table)
        (cond
            ((null? table) 'error)
            ((eq? var (car (car table))) (cdr (car table)))
            (else (table_search var (cdr table))))))

; Return the type of var, where typetable is the first table in s
(define type
    (lambda (var s))
        (table_search var (car s)))

; Return the init of var, where inittable is the second table in s
(define init
    (lambda (var s)
        (table_search var (cadr s))))

; Create s, where s is (typetable inittable)
; and each table is ((var val) (var val) (var val))
(define new_state
    (lambda ()
        (return '(()()))
))

(define add
    (lambda (var type table)
; Add a type to s
(define add_type
    (lambda (var type s)
        (cons '(var type) (car s))))

; Add an init to s
(define add_init
    (lambda (var init s)
        (cons '(var init) (cadr s))))

(define union
    (lambda (var 
