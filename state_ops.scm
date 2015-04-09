; ========== BASIC LIST OPERATIONS ==========
; Applicable for any key-value pair list.

; Step through a table represented as ((var val) (var val) (var val))
; If the name of the var matches the var argument, return val
; If it's not in the table return 'error
(define layer_search
    (lambda (var layer)
        (cond
            ((null? layer) 'error)
            ((eq? var (car (car layer))) (cadr (car layer)))
            (else (layer_search var (cdr layer)))
)))

; Searches for the first occurence of a variable in the state and returns its value
(define state_search
  (lambda (var state)
    (cond
      ((null? state) 'error)
      ((eq? (layer_search var (top_layer state)) 'error) (state_search var (remove_layer state)))
      (else (layer_search var (top_layer state)))
    )
  )
)

; Add a (key value) to the table which is ((key value) (key value) ... )
; So i.e. ((a, 5) (b, 6)) -> ((c, 7) (a, 5) (b, 6)) with args c, 7.
(define add_to_layer
  (lambda (key value layer)
    (cons (list key value) layer)
  )
)

; Adds a variable's value to the top_layer of a state
(define add_to_state
  (lambda (key value state)
    (cons (add_to_layer key value (top_layer state)) (remove_layer state))
  )
)

; Take a table which is ((key1 value) (key2 value) ...)
; and return ((key2 value) ...) if it was (delete key1 table)
(define delete_from_layer
    (lambda (key layer)
        (cond
            ((null? layer) '())
            ((eq? key (car (car layer))) (cdr layer))
            (else (cons (car layer) (delete_from_layer key (cdr layer))))
)))

; Sets a variable's value in a state
(define set_binding
  (lambda (key value s)
    (cond
      ((eq? 'error (get_binding key s)) (add_to_state key value s))
      ((not (eq? 'error (get_binding_layer key (top_layer s)))) (add_to_state key value (cons (delete_from_layer key (top_layer s)) (remove_layer s))))
      (else (cons (top_layer s) (set_binding key value (remove_layer s))))
    )
  )
)

; Gets a variables value inside a given state
(define get_binding
  (lambda (key s)
    (state_search key s)
  )
)

; Gets a variables value inside a given layer
(define get_binding_layer
  (lambda (key layer)
    (layer_search key layer)
  )
)

; Throw an error if the binding is not there.
(define get_binding_safe
    (lambda (key s)
        (if (equal? (get_binding key s) 'error)
            (error "Referencing variable before assignment")
            (get_binding key s)
)))

; Checks if a given key has been initialized
(define defined?
  (lambda (key s)
    (not (eq? (state_search key s) 'error))
  )
)

; Returns the remainder of a layer, after a given key
(define layer_remainder
  (lambda (key layer)
    (cond 
      ((null? layer) new_layer)
      ((eq? (car (car layer)) key) layer)
      (else (layer_remainder key (cdr layer)))
    )
  )
)

; Returns the remainder of a state, after a given key
(define state_remainder
  (lambda (key s)
    (cond
      ((null? s) new_state)
      ((eq? (layer_search key (top_layer s)) 'error) (state_remainder key (remove_layer s)))
      (else (cons (state_remainder_layer key (top_layer s)) (remove_layer s)))
    )
  )
)

; State stored as a list of layers. Each layer contains two tables: bindings and inittable
; Each table is stored as a list of pairs (var val)
(define new_layer '())
(define new_state (list new_layer))

; Some state/layer abstractions
(define top_layer car)
(define add_layer
  (lambda (s)
    (cons new_layer s)
  )
)
(define remove_layer cdr)

; ==== TEST CODE ====
;(delete-cps 'a '((e 4) (b 5) (y 6) (a 7)) (lambda (v) v))
;(union '((x 5) (y 6) (a 7)) '((e 4) (b 5) (y 6) (a 7)))
;new_state
;(set_binding 'z  4 (add_layer (set_binding 'x 5 (set_binding 'y 4 (set_binding 'x 2 new_state)))))
;(M_state '(= x z ) (remove_layer (set_binding 'z 2 (set_binding 'y 3 (add_layer (set_init 'x #t (set_binding 'y 2 (set_binding 'w 4 new_state))))))))
