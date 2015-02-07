;(load "simpleParser.scm")
;(parser "p1test1")


; Step through a table represented as ((var val) (var val) (var val))
; If the name of the var matches the var argument, return val
; If it's not in the table return 'error
(define table_search
    (lambda (var table)
        (cond
            ((null? table) 'error)
            ((eq? var (car (car table))) (cadr (car table)))
            (else (table_search var (cdr table)))
)))

; Return the type of var, where typetable is the first table in s
(define type
    (lambda (var s)
        (table_search var (typetable s))
))

; Return the init of var, where inittable is the second table in s
(define init
    (lambda (var s)
        (table_search var (inittable s))
))


; Add a (key value) to the table which is ((key value) (key value) ... )
; So i.e. ((a, 5) (b, 6)) -> ((c, 7) (a, 5) (b, 6)) with args c, 7.
(define add
    (lambda (key value table)
        (cons (cons key (cons value '())) table)
))

; Take a table which is ((key1 value) (key2 value) ...)
; and return ((key2 value) ...) if it was (delete key1 table)
(define delete
    (lambda (key table)
        (cond
            ((null? table) '())
            ((eq? key (car (car table))) (cdr table))
            (else (cons (car table) (delete key (cdr table))))
)))

; Add a type to s, which is (typetable inittable)
; Do it by adding the new typetable to the cdr, which is (inittable)
(define add_type
    (lambda (var type s)
        (cons (add var type (typetable s)) (cons (inittable s) '()))
))

; Add an init to s, which is (typetable inittable)
; Do it by adding typetable to ( new inittable )
(define add_init
    (lambda (var init s)
        (cons (typetable s) (cons (add var init (inittable s)) '()))
))

; Delete a var from the typetable of s, so it now returns 'error for that var.
(define delete_type
    (lambda (var s)
        (cons (delete var (typetable s)) (cons (inittable s) '()))
))

; Delete a var from the inittable of s, so it now returns 'error for that var.
(define delete_init
    (lambda (var s)
        (cons (typetable s) (cons (delete var (inittable s)) '()))
))

; Union of two tables
; Add (key val) from table1 to the front of a version of table2 that does not
; have key in it.  This version of table2 also has already recursively added
; the rest of the keys in table1 to itself.
(define union
    (lambda (table1 table2)
        (cond
            ((null? table1) table2)
            (else (cons
                    (car table1)
                    (delete (car (car table1)) (union (cdr table1) table2))
        )))
))


; ==== DEFINITIONS ====

; Create s, where s is (typetable inittable)
; and each table is ((var val) (var val) (var val))
(define new_state '( () () ) )

; Typetable is the first list in s
(define typetable
    (lambda (s)
        (car s)
))

; Inittable is the second list in s
(define inittable
    (lambda (s)
        (cadr s)
))

; ==== TEST CODE ====
(delete 'a '((e 4) (b 5) (y 6) (a 7)))
(union '((x 5) (y 6) (a 7)) '((e 4) (b 5) (y 6) (a 7)))


