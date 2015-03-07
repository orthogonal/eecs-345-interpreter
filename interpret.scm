(load "simpleParser.scm")
;(load "state_ops.scm")


; The basic outer Mstate.
; Iterates through ((expr) (expr) (expr)) and executes each expression
; in turn, passing its state value along.
; Returns whatever is returned from the final expression evaluated, which
; ought to be a return function.
; If it gets an expression, i.e. (= x 5), rather than a list of expressions,
; it updates the state as appropriate rather than diving any deeper.
; If it sees a command it doesnt know how to handle it returns it
(define Mstate-cps
  (lambda (expr s return break)
    (cond
      ((null? expr) (return s))
      ((list? (car expr))                       (Mstate-cps (car expr) s 
                                                    (lambda (s1) (Mstate-cps (cdr expr) s1
                                                    (lambda (s2) (return s2)) (lambda (s2) (break s2)))) break))
      ((equal? (car expr) 'var)                 (Mstate_var-cps expr s return break))
      ((equal? (car expr) '=)                   (Mstate_eq-cps expr s return break))
      ((equal? (car expr) 'return)              (Mstate_return-cps expr s return break))
      ((equal? (car expr) 'if)                  (Mstate_if-cps expr s return break))
      ((equal? (car expr) 'while)               (Mstate_while-cps (cadr expr) (caddr expr) s return))
      ((equal? (car expr) 'begin)               (Mstate_begin-cps (cdr expr) s return break))
      ((equal? (car expr) 'break)               (break (remove_layer s)))
      ((equal? (car expr) 'continue)            s)

      
      )))

; Two possibilities.
; (var x) has length 2, so cddr will be null and just add x to inittable.
; (var x value) has length 3, so cddr will not be null.
; then you add x to inittable and add its calculated value as a binding.
; The calculated value is (Mvalue (caddr expr) s) or Mvalue(value)
; Init is false if no value, true if there is a value.
(define Mstate_var-cps
    (lambda (expr s return break)
        (cond
            ((eq? (get_init (cadr expr) s) #t) (error "Redefining variable"))
            ((null? (cddr expr)) (return (set_init (cadr expr) 'false s)))
            (else (Mvalue-cps (caddr expr) s (lambda (v) (return (set_binding (cadr expr) v (set_init (cadr expr) #t s))))))
        )
    )
)

; Takes (= x 5) and
;    if x hasn't been declared, it's not in the init table, so throw an error.
;    set the binding of x to 5.
; If it's an expression like (= x (+ 3 y)) set the binding to the
;   Mvalue evaluation of the right operand expression.
(define Mstate_eq-cps
    (lambda (expr s return break)
        (if (eq? (get_init (cadr expr) s) 'error)
            (error "Variable assignment before declaration")
            (return (set_binding (cadr expr) (right_op_val expr s)
                (set_init (cadr expr) #t s)))
        )
))

; Takes (return (+ 3 x)) and return 3+x, NOT a state!
; Since this doesn't return a state, it's assuming that it is the last thing
;    called and that at this point the user wants a value, not a state.
(define Mstate_return-cps
    (lambda (expr s return break)
        (set_binding'return (Mvalue-cps (cadr expr) s (lambda (v) v)) s)
    )
)

; Takes (if (cond) (then-expr) (else-expr))
; First evaluates the boolean condition.  If it's true, return Mstate(then-expr)
; If it's false, return just the state unchanged if no else statement, and
;  Mstate(else-expr) if there is an else statement.
(define Mstate_if-cps
  (lambda (expr s return break)
    (cond
      ((Mboolean-cps (cadr expr) s (lambda (v) v)) (Mstate-cps (caddr expr) s return break))
      ((null? (cdddr expr)) (return s))
      (else (Mstate-cps (cadddr expr) s return break))
      )
    )
  )

;Takes a begin statement
;This is where layers of code are computed
;We just feed a new state in front of the other things to "store" them

(define Mstate_begin-cps
  (lambda (expr s return break)
      (Mstate-cps expr (add_layer s) (lambda (s1) (return (remove_layer s1))) (lambda (s1) (break s1)))
  ))

;Evaluates a body based on a condition that is true and watches for break
;Takes (< i j) (= i (+ i 1)) s and return
;This part takes care of break and continue statements
(define Mstate_while-cps
    (lambda (condition body s return)
      (call/cc (lambda (break)
        (letrec (
            (loop (lambda (condition body s return)
                (cond
                    ((Mboolean-cps condition s (lambda (v) v))
                        (loop condition body (Mstate-cps body s return (lambda(s_out) (break s_out))) return))
                    (else (return s)))
            ))) (loop condition body s return)
)))))

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
(define Mvalue-cps
    (lambda (expr s return)
        (cond
            ((number? expr) (return expr))
            ((not (list? expr)) (return (get_binding_safe expr s)))
            ((equal? (car expr) '+) (return (+ (left_op_val expr s) (right_op_val expr s))))
            ((equal? (car expr) '-)
                (if (null? (cddr expr)) 
                    (return (- 0 (left_op_val expr s)))
                    (return (- (left_op_val expr s) (right_op_val expr s)))))
            ((equal? (car expr) '*) (return (* (left_op_val expr s) (right_op_val expr s))))
            ((equal? (car expr) '/) (return (/ (- (left_op_val expr s) (modulo (left_op_val expr s) (right_op_val expr s))) (right_op_val expr s)))) ; Integer division:  (x - (x % y)) / y
            ((equal? (car expr) '%) (return (modulo (left_op_val expr s) (right_op_val expr s))))
            ((logical_operator? (car expr)) (Mboolean-cps expr s (lambda (v) (return v))))
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
(define Mboolean-cps
    (lambda (expr s return)
        (cond
            ((boolean? expr) (return expr))
            ((equal? expr 'true) (return #t))
            ((equal? expr 'false) (return #f))
            ((not (list? expr)) (return (get_binding_safe expr s)))
            ((equal? (car expr) '||) (Mboolean-cps (caddr expr) s (lambda (v1) (Mboolean-cps (cadr expr) s (lambda (v2) (return (or v1 v2)))))))
            ((equal? (car expr) '&&) (Mboolean-cps (caddr expr) s (lambda (v1) (Mboolean-cps (cadr expr) s (lambda (v2) (return (and v1 v2)))))))
            ((equal? (car expr) '!) (Mboolean-cps (cadr expr) s (lambda (v) (return (not v)))))
            ((equal? (car expr) '>) (return (> (left_op_val expr s) (right_op_val expr s))))
            ((equal? (car expr) '<) (return (< (left_op_val expr s) (right_op_val expr s))))
            ((equal? (car expr) '>=) (return (>= (left_op_val expr s) (right_op_val expr s))))
            ((equal? (car expr) '>=) (return (>= (left_op_val expr s) (right_op_val expr s))))
            ((equal? (car expr) '==) (return (eq? (left_op_val expr s) (right_op_val expr s))))
            ((equal? (car expr) '!=) (return (not (eq? (left_op_val expr s) (right_op_val expr s)))))
            (error "Invalid expression for Mboolean")
        )
))

(define left_op_val
    (lambda (expr s)
        (Mvalue-cps (cadr expr) s (lambda (v) v))
))

(define right_op_val
    (lambda (expr s)
        (Mvalue-cps (caddr expr) s (lambda (v) v))
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
            (cond
                ((eq? (get_binding 'return (Mstate-cps (parser filename) new_state (lambda (v) v) (lambda (v1) v1))) #t) 'true)
                ((eq? (get_binding 'return (Mstate-cps (parser filename) new_state (lambda (v) v) (lambda (v1) v1))) #f) 'false)
                ((eq? (get_binding 'return (Mstate-cps (parser filename) new_state (lambda (v) v) (lambda (v1) v1))) 'error) (error "Undefined Variable"));this is returned when we dont see a return in the state
                (else (get_binding 'return (Mstate-cps (parser filename) new_state (lambda (v) v) (lambda (v1) v1))))
            )
            
))


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
; and return ((key2 value) ...) if it was (delete-cps key1 table)
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
      (cond
        ((eq? 'error (get_binding key s)) (update_first (update_bindings (add key value (delete key (bindings (car s)))) (car s)) s))
        ((not (eq? 'error (get_binding_layer key (car s)))) (update_first (update_bindings (add key value (delete key (bindings (car s)))) (car s)) s))
        (else (cons (car s) (set_binding key value (cdr s))))
      )
))

(define set_init
    (lambda (key value s)
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
;(delete-cps 'a '((e 4) (b 5) (y 6) (a 7)) (lambda (v) v))
;(union '((x 5) (y 6) (a 7)) '((e 4) (b 5) (y 6) (a 7)))
;(get_binding 'x (set_binding 'x 5 (set_binding 'y 4 (set_binding 'x 2 new_state))))
;(M_state '(= x z ) (remove_layer (set_binding 'z 2 (set_binding 'y 3 (add_layer (set_init 'x #t (set_binding 'y 2 (set_binding 'w 4 new_state))))))))
