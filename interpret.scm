(load "functionParser.scm")
;(load "simpleParser.scm")
(load "state_ops.scm")

; Kicks off the interpreter
(define interpret
  (lambda (filename)
    (prettify_result
      (interpret_main filename)
    )
  )
)

; Converts #t to true and #f to false before returning
(define prettify_result
  (lambda (result)
    (cond
        ((eq? result #t) 'true)
        ((eq? result #f) 'false)
        (else result)
    )
  )
)

; Creates the "outer state" by making a first pass on the parse tree, then adds a new layer to it
(define initial_environment 
  (lambda (parse_tree)
    (add_layer (interpret_outer_parse_tree parse_tree new_state new_return_continuation))
  )
)

; Runs the main function
(define interpret_main
  (lambda (filename)
    (Mvalue_function_call-cps '(funcall main) (initial_environment (parser filename)) new_return_continuation)
  )
)

; First pass of parse tree to build "outer state"
(define interpret_outer_parse_tree
  (lambda (parse_tree state return)
    (cond
      ((null? parse_tree) state)
      (else (interpret_outer_parse_tree (parse_tree_remainder parse_tree) (Mstate_outer (parse_tree_statement parse_tree) state return) return))
    )
  )
)

; Gets environment function from function closure and calls it with a state to return function environment
(define get_function_environment
  (lambda (expr s)
    ((caddr (get_closure expr s)) s)
   )
 )

; Mstate function for building the outer state. Only variable definitions, variable assignment, and function definitions are allowed outside of a function
(define Mstate_outer
  (lambda (expr s return)
    (cond
      ((equal? (keyword expr) 'var)       (Mstate_var-cps expr s return))
      ((equal? (keyword expr) '=)         (Mstate_eq-cps expr s return))
      ((equal? (keyword expr) 'function)  (Mstate_function_def-cps expr s return))
      (else (return state))
    )
  )
)

(define Mvalue_function_call-cps
  (lambda (expr s return)
    ;DONE get function environment
    ;DONE evaluate each actual parameter and bind it to formal parameter (binding goes in function environment)
    ;DONE interpret the body of the function with the function environment
    ;TODO use boxes to allow for global variable side effects
    
    (get_binding 'return 
      (interpret_parse_tree
       (get_function_body (get_closure expr s)) (bind_parameters (get_actual_params expr) (get_formal_params (get_closure expr s)) s return) new_return_continuation continue_error break_error))
  )
)

(define get_formal_params car)
(define get_function_body cadr)
(define get_actual_params cddr)

; Evaluates the actual params and binds their values to the formal params
; Isn't actually cps
(define bind_parameters
  (lambda (actual_params formal_params s return)
    (cond
      ((null? actual_params) (return s))
      (else (bind_parameters (cdr actual_params) (cdr formal_params) (set_binding (car formal_params) (Mvalue-cps (car actual_params) s return) s) return))
    )
  )
)

; Looks up and returns function closure from state
(define get_closure
  (lambda (expr s)
    (get_binding (functionname expr) s)
  )
)

; Adds an entry to the state of (function_name function_closure)
(define Mstate_function_def-cps
  (lambda (expr s return)
    (return (set_binding (functionname expr) (make_closure expr s) s))
  )
)

; Some abstractions for parsing out the pieces of a function definition expression
(define functionname cadr)
(define arglist caddr)
(define functionbody cadddr)

; Function closure is (formal_parameters, function_body, function_that_creates_function_environment)
(define make_closure
  (lambda (expr s)
    (list (arglist expr) (functionbody expr) (functionenvironment (functionname expr)))
  )
)

; Returns a function that generates a function environment given the current state
(define functionenvironment
 (lambda (name)
  (lambda (state)
    (state_remainder name state)
  )
 )
)

; Abstractions for interpret
(define new_return_continuation (lambda (v) v))
(define break_error (lambda (v) (error 'no-loop)))
(define continue_error (lambda (v) (error 'no-loop)))

; Takes a parse tree generated from Connamacher's parser, and interprets statements one at a time
(define interpret_parse_tree
  (lambda (parse_tree state return break continue)
    (cond
      ((null? parse_tree) state)
      (else (interpret_parse_tree (parse_tree_remainder parse_tree) (Mstate-cps (parse_tree_statement parse_tree) state return break continue) return break continue))
    )
  )
)

; Abstractions for interpret_parse_tree
(define parse_tree_remainder cdr)
(define parse_tree_statement car)

; The basic outer Mstate.
; Takes an expression, i.e. (= x 5), and calls the appropriate function
;   based on the keyword of the expression
(define Mstate-cps
  (lambda (expr s return break continue)
    (cond
      ((equal? (keyword expr) 'var)                 (Mstate_var-cps expr s return))
      ((equal? (keyword expr) '=)                   (Mstate_eq-cps expr s return))
      ((equal? (keyword expr) 'return)              (Mstate_return-cps expr s return))
      ((equal? (keyword expr) 'if)                  (Mstate_if-cps expr s return break continue))
      ((equal? (keyword expr) 'while)               (Mstate_while-cps expr s return))
      ((equal? (keyword expr) 'begin)               (Mstate_begin-cps expr s return break continue))
      ((equal? (keyword expr) 'funcall)             (Mstate_function_call-cps expr s return))
      ((equal? (keyword expr) 'function)            (Mstate_function_def-cps expr s return))
      ((equal? (keyword expr) 'break)               (break s))
      ((equal? (keyword expr) 'continue)            (continue s))
    )
  )
)

; Abstractions for Mstate
(define keyword car)

; Two possibilities, variable declaration with and without initial value.
; (var x) has length 2, so cddr will be null and just add x to inittable.
; (var x value) has length 3, so cddr will not be null.
; then you add x to inittable and add its calculated value as a binding.
; The calculated value is (Mvalue (caddr expr) s) or Mvalue(value)
; Init is false if no value, true if there is a value.
(define Mstate_var-cps
    (lambda (expr s return)
        (cond
            ((defined? (varname expr) s) (error "Redefining variable"))
            ;((null? (cddr expr)) (return (set_init (varname expr) 'false s)))
            ((null? (cddr expr)) (return s))
            (else (Mvalue-cps (initialvalue expr) s (lambda (v) (return (set_binding (varname expr) v s)))))
        )
    )
)

; Abstractions for Mstate_var
(define varname cadr)
(define initialvalue caddr)

; Takes (= x 5) and
; If it's an expression like (= x (+ 3 y)) set the binding to the
;   Mvalue evaluation of the right operand expression.
(define Mstate_eq-cps
  (lambda (expr s return)
;    (if (eq? (get_init (varname expr) s) 'error)
;      (error "Variable assignment before declaration")
      (return (set_binding (varname expr) (right_op_val expr s) s))
;    )
  )
)

; Adds the return value into the state under name 'return
(define Mstate_return-cps
  (lambda (expr s return)
    (set_binding 'return (Mvalue-cps (returnexpr expr) s (lambda (v) v)) s)
  )
)

; Abstractions for Mstate return
(define returnexpr cadr)

; Takes (if (cond) (then-expr) (else-expr))
; First evaluates the boolean condition.  If it's true, return Mstate(then-expr)
; If it's false, return just the state unchanged if no else statement, and
;  Mstate(else-expr) if there is an else statement.
(define Mstate_if-cps
  (lambda (expr s return break continue)
    (cond
      ((Mboolean-cps (ifcond expr) s (lambda (v) v)) (Mstate-cps (iftruebody expr) s return break continue))
      ((null? (ifnoelse expr)) s)
      (else (Mstate-cps (iffalsebody expr) s return break continue))
    )
  )
)

; Abstractions for if
(define ifcond cadr)
(define iftruebody caddr)
(define ifnoelse cdddr)
(define iffalsebody cadddr)

; Takes a begin statement
; This is where layers of code are computed
(define Mstate_begin-cps
  (lambda (expr s return break continue)
    (remove_layer (interpret_parse_tree (begin_body expr) (add_layer s) return (break_layer break) (continue_layer continue)))
  )
)

; Abstractions for layers
(define begin_body cdr)

(define break_layer
  (lambda (break)
    (lambda (s)
      (break (remove_layer s))
    )
  )
)

(define continue_layer
  (lambda (continue)
    (lambda (s)
      (continue (remove_layer s))
    )
  )
)

;Evaluates a body based on a condition that is true and watches for break
;Takes (< i j) (= i (+ i 1)) s and return
;This part takes care of break and continue statements
(define Mstate_while-cps
    (lambda (expr s return)
      (call/cc (lambda (break)
        (letrec (
            (loop (lambda (expr s)
                (cond
                    ((Mboolean-cps (whilecond expr) s (lambda (v) v))
                        (loop expr (Mstate-cps (whilebody expr) s return break (continue loop expr return break))))
                    (else (break s)))
            ))) (loop expr s)
      )
    ))
  )
)

; Abstractions for while
(define whilecond cadr)
(define whilebody caddr)
(define continue
  (lambda (loop expr return break)
    (lambda (state)
      (break (loop expr state))
    )
  )
)

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
            ((equal? (operator expr) '+) (return (+ (left_op_val expr s) (right_op_val expr s))))
            ((equal? (operator expr) '-)
                (if (null? (cddr expr)) 
                    (return (- 0 (left_op_val expr s)))
                    (return (- (left_op_val expr s) (right_op_val expr s)))))
            ((equal? (operator expr) '*) (return (* (left_op_val expr s) (right_op_val expr s))))
            ((equal? (operator expr) '/) (return (/ (- (left_op_val expr s) (modulo (left_op_val expr s) (right_op_val expr s))) (right_op_val expr s)))) ; Integer division:  (x - (x % y)) / y
            ((equal? (operator expr) '%) (return (modulo (left_op_val expr s) (right_op_val expr s))))
            ((equal? (operator expr) 'funcall) (Mvalue_function_call-cps expr s (lambda (v) (return v))))
            ((logical_operator? (operator expr)) (Mboolean-cps expr s (lambda (v) (return v))))
            (error "Invalid expression for Mvalue")
        )
))

(define operator car)

; Returns the value of the boolean expression expr.
; If expr is #t or #f, just return those.
; If it's something else that's not a list, it must be a variable, so return the
;    value of that variable (presumed to be correct type)
; Otherwise, if it's a boolean expression do the corresponding scheme expression
;    with the Mbooleans of the operands (or operand if it's not)
; and if it's an arithmetic comparison, do the corresponding scheme expression
;    with the Mvalues of the operands.
(define Mboolean-cps
    (lambda (expr s return)
        (cond
            ((boolean? expr) (return expr))
            ((equal? expr 'true) (return #t))
            ((equal? expr 'false) (return #f))
            ((not (list? expr)) (return (get_binding_safe expr s)))
            ((equal? (car expr) '||) (Mboolean-cps (caddr expr) s (lambda (v1) (Mboolean-cps (cadr expr) s (lambda (v2) (return (or v1 v2)))))))
            ((equal? (car expr) '&&) (Mboolean-cps (caddr expr) s (lambda (v1) (Mboolean-cps (cadr expr) s (lambda (v2) (return (and v1 v2)))))))
            ((equal? (car expr) '!=) (return (not (equal? (left_op_val expr s) (right_op_val expr s)))))
            ((equal? (car expr) '!) (Mboolean-cps (cadr expr) s (lambda (v) (return (not v)))))
            ((equal? (car expr) '>) (return (> (left_op_val expr s) (right_op_val expr s))))
            ((equal? (car expr) '<) (return (< (left_op_val expr s) (right_op_val expr s))))
            ((equal? (car expr) '>=) (return (>= (left_op_val expr s) (right_op_val expr s))))
            ((equal? (car expr) '<=) (return (<= (left_op_val expr s) (right_op_val expr s))))
            ((equal? (car expr) '==) (return (equal? (left_op_val expr s) (right_op_val expr s))))
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
            (else #f)
        )
))

(interpret "tests3/5")
