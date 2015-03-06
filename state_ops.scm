
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

(define set_binding
    (lambda (key value s)
      (cond
        ((eq? 'error (get_binding key s)) (update_first (update_bindings (add key value (delete key (bindings (car s)))) (car s)) s))
        ((not (eq? 'error (get_binding_layer key (car s)))) (update_first (update_bindings (add key value (delete key (bindings (car s)))) (car s)) s))
        (else (cons (car s) (set_binding key value (cdr s))))
      )
))

(define set_init
    (lambda (key value s)
        ;(update_first (update_inittable
        ;    (add key value (delete key (inittable (top_layer s)))) (top_layer s)) s)
        (cond
            ((eq? 'error (get_init key s)) (update_first (update_inittable (add key value (delete key (inittable (car s)))) (car s)) s))
            ((not (eq? 'error (get_init_layer key (car s)))) (update_first (update_inittable (add key value (delete key (inittable (car s)))) (car s)) s))
            (else (cons (car s) (set_init key value (cdr s))))
        )
))

(define get_binding
    (lambda (key s)
        (table_search
            key (all_bindings s))
))

(define get_binding_layer
    (lambda (key layer)
        (table_search
            key (bindings layer))
))


 (define get_init
   (lambda (key s)
     (table_search
       key (all_inittable s))))

(define get_init_layer
    (lambda (key layer)
        (table_search
            key (inittable layer))
))

; Throw an error if the binding is not there.
(define get_binding_safe
    (lambda (key s)
        (if (equal? (get_binding key s) 'error)
            (error "Referencing variable before assignment")
            (get_binding key s)
)))


; ========== DEFINITIONS ==========
; Change these to restructure state
(define bindings 
  (lambda (layer)
    (car layer)))

(define all_bindings
    (lambda (s)
      (cond
        ((null? s) '())
        (else (append (bindings (top_layer s)) (all_bindings (cdr s))))
      )
    )
)

(define update_bindings
    (lambda (new_bindings layer)
        (update_first new_bindings layer)))

(define inittable
  (lambda (layer)
    (cadr layer)))

(define all_inittable
    (lambda (s)
      (cond
        ((null? s) '())
        (else (append (inittable (top_layer s)) (all_inittable (cdr s))))
      )
    )
)

(define update_inittable
    (lambda (new_inittable layer)
      (update_second new_inittable layer)))
        

(define new_layer '( () () ))

; State stored as a list of layers. Each layer contains two tables: bindings and inittable
; Each table is stored as a list of pairs (var val)
(define new_state (list new_layer))

(define top_layer
  (lambda (s)
    (car s)
  )
)

(define add_layer
  (lambda (s)
    (cons new_layer s)
  )
)

(define remove_layer
  (lambda (s) 
    (cdr s)
  )
)


; ==== TEST CODE ====
;(delete 'a '((e 4) (b 5) (y 6) (a 7)))
;(union '((x 5) (y 6) (a 7)) '((e 4) (b 5) (y 6) (a 7)))
;(get_binding 'x (set_binding 'x 5 (set_binding 'y 4 (set_binding 'x 2 new_state))))
;(M_state '(= x z ) (remove_layer (set_binding 'z 2 (set_binding 'y 3 (add_layer (set_init 'x #t (set_binding 'y 2 (set_binding 'w 4 new_state))))))))
