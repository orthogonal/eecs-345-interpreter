; ========== BASIC LIST OPERATIONS ==========
; Applicable for any key-value pair list.

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

; Union of two tables
; Add (key val) from table1 to the front of a version of table2 that does not
; have key in it.  This version of table2 also has already recursively added
; the rest of the keys in table1 to itself.
; i.e. ((a 5) (b 3)) ((d 6) (a 3)) -> ((a 5) (b 3) (d 6))
(define union
    (lambda (table1 table2)
        (cond
            ((null? table1) table2)
            (else (cons
                    (car table1)
                    (delete (car (car table1)) (union (cdr table1) table2))
        )))
))

; Update the first item on a list
(define update_first
    (lambda (new_value list)
        (cons new_value (cdr list))
))

; Update the second item on a list
(define update_second
    (lambda (new_value list)
        (cons (car list) (update_first new_value (cdr list)))
))

; Update the third item on a list
(define update_third
    (lambda (new_value list)
        (cons (car list) (update_second new_value (cdr list)))
))


; ========== STATE MANIPULATION ==========
; Changing particular things within a state

(define add_binding
    (lambda (key value s)
        (update_bindings
            (add key value (delete key (bindings s))) s)
))

(define add_init
    (lambda (key value s)
        (update_inittable
            (add key value (delete key (inittable s))) s)
))

(define get_binding
    (lambda (key s)
        (table_search
            key (bindings s))
))

(define get_init
    (lambda (key s)
        (table_search
            key (inittable s))
))


; ========== DEFINITIONS ==========
; Change these to restructure state
(define bindings
    (lambda (s)
        (car s)))

(define update_bindings
    (lambda (new_bindings s)
        (update_first new_bindings s)))

(define inittable
    (lambda (s)
        (cadr s)))

(define update_inittable
    (lambda (new_inittable s)
        (update_second new_inittable s)))

(define new_state '( () () )   )



; ==== TEST CODE ====
(delete 'a '((e 4) (b 5) (y 6) (a 7)))
(union '((x 5) (y 6) (a 7)) '((e 4) (b 5) (y 6) (a 7)))
(add_init 'x 'true (add_binding 'y 2 (add_binding 'w 4 new_state)))
(get_binding 'x (add_binding 'x 5 (add_binding 'y 4 (add_binding 'x 2 new_state))))
