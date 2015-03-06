
; ========== BASIC LIST OPERATIONS ==========
; Applicable for any key-value pair list.

; Step through a table represented as ((var val) (var val) (var val))
; If the name of the var matches the var argument, return val
; If it's not in the table return 'error
(define table_search-cps
    (lambda (var table return)
        (cond
            ((null? table) (return 'error))
            ((eq? var (car (car table))) (return (cadr (car table))))
            (else (table_search-cps var (cdr table) (lambda (v) (return v))))
)))

; Add a (key value) to the table which is ((key value) (key value) ... )
; So i.e. ((a, 5) (b, 6)) -> ((c, 7) (a, 5) (b, 6)) with args c, 7.
(define add
    (lambda (key value table)
        (cons (cons key (cons value '())) table)
))

; Take a table which is ((key1 value) (key2 value) ...)
; and return ((key2 value) ...) if it was (delete-cps key1 table)
(define delete-cps
    (lambda (key table return)
        (cond
            ((null? table) (return '()))
            ((eq? key (car (car table))) (return (cdr table)))
            (else (delete-cps key (cdr table) (lambda (t) (return (cons (car table) t)))))
)))

; Union of two tables
; Add (key val) from table1 to the front of a version of table2 that does not
; have key in it.  This version of table2 also has already recursively added
; the rest of the keys in table1 to itself.
; i.e. ((a 5) (b 3)) ((d 6) (a 3)) -> ((a 5) (b 3) (d 6))
(define union-cps
    (lambda (table1 table2 return)
        (cond
            ((null? table1) (return table2))
            (else (union-cps (cdr table1) table2 (lambda (t2) (cons (car table1)
                (delete-cps (car (car table1)) t2 (lambda (v) (return v)))))))
        )))

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

(define set_binding
    (lambda (key value s)
        (update_bindings
            (add key (box value) (delete-cps key (bindings s))) s (lambda (v) v))
))

(define set_init
    (lambda (key value s)
        (update_inittable
            (add key value (delete-cps key (inittable s))) s (lambda (v) v))
))

(define get_binding
    (lambda (key s)
        (table_search-cps
            key (bindings s) (lambda (v) v))
))

(define get_init-cps
    (lambda (key s stateList return)
        (letrec ((val (table_search-cps key (inittable s) (lambda (v) v))))
            (cond
                ((and (equal? val 'error) (not (null? stateList)))
                    (get_init-cps key stateList (getStateList stateList) (lambda (v) (return v))))
                (else (table_search-cps key (inittable s) (lambda (v) (return v))))
))))

; Throw an error if the binding is not there.
(define get_binding_safe-cps
    (lambda (key s stateList return)
        (letrec ((val (get_binding key s)))
            (cond
                ((and (equal? val 'error) (not (null? stateList)))
                    ((get_binding_safe-cps key stateList (getStateList stateList) (lambda (v) (return v))))
                ((equal? val 'error) (error "Referencing variable before assignment"))
                (else (return (unbox val)))
)))))


; ========== DEFINITIONS ==========
; Change these to restructure state
(define bindings
    (lambda (s)
        (car s)))

(define getStateList
    (lambda (s)
        (cddr s)))

(define update_bindings
    (lambda (new_bindings s)
        (update_first new_bindings s)))

(define inittable
    (lambda (s)
        (cadr s)))

(define update_inittable
    (lambda (new_inittable s)
        (update_second new_inittable s)))

(define new_state '( () () ))



; ==== TEST CODE ====
;(delete-cps 'a '((e 4) (b 5) (y 6) (a 7)) (lambda (v) v))
;(union '((x 5) (y 6) (a 7)) '((e 4) (b 5) (y 6) (a 7)))
;(set_init 'x #t (set_binding 'y 2 (set_binding 'w 4 new_state)))
;(get_binding 'x (set_binding 'x 5 (set_binding 'y 4 (set_binding 'x 2 new_state))))
