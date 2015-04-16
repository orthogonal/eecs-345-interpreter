;For current testing
;(load "functionParser.scm")
(load "classParser.scm")
;(load "basicParser.scm")

; Kicks off the interpreter for classes
(define interpretClass
  (lambda (filename class_main)
     (prettify_result
      (interpret_main_in_class (parser filename)))))

; Kicks off the interpreter for functions
(define interpretFunction
  (lambda (filename)
    (prettify_result
     (interpret_main (parser filename)))))

; Kicks off the interpreter for code
(define interpretCode
  (lambda (filename)
    (prettify_result
     (get_binding 'return
                  (interpret_parse_tree_return
                   (parser filename)
                   new_state
                   new_return_continuation continue_error break_error throw_error)))))

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

; Runs the main function in the given class
(define interpret_main_in_class
  (lambda (parsed class_name)
    (Mvalue_function_call-cps '(funcall main) (initial_environment parsed) new_return_continuation class_name)))

; Runs the main function
(define interpret_main
  (lambda (parsed)
    (Mvalue_function_call-cps '(funcall main) (initial_environment parsed) new_return_continuation)
    )
  )

; Creates the "outer state" by making a first pass on the parse tree, then adds a new layer to it
(define initial_environment 
  (lambda (parse_tree)
    (interpret_outer_parse_tree-cps parse_tree new_state new_return_continuation)
    )
  )

; First pass of parse tree to build "outer state"
(define interpret_outer_parse_tree-cps
  (lambda (parse_tree state return)
    (display "\n")
    (display "\n")
    (display parse_tree)
    (display "\n")
    (display state)
    (cond
      ((null? parse_tree) (return state))
      (else (interpret_outer_parse_tree-cps 
             (parse_tree_remainder parse_tree) 
             (Mstate_outer-cps (parse_tree_statement parse_tree) state new_return_continuation) 
             return))
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
(define Mstate_outer-cps
  (lambda (expr s return)
    (cond
      ((equal? (keyword expr) 'var)       (Mstate_var-cps expr s return))
      ((equal? (keyword expr) '=)         (Mstate_eq-cps expr s return))
      ((equal? (keyword expr) 'function)  (Mstate_function_def-cps expr s return))
      ((equal? (keyword expr) 'class)     (Mstate_class_def-cps expr s return))
      (else (return state))
      )
    )
  )

; Evaluates a function call and returns its value
; This is done by treating a function as a subprogram and returning the 'return binding in the resulting state
(define Mvalue_function_call-cps
  (lambda (expr s return class_name)
    (if (eq? (get_closure (functionname expr) class_name) 'error)
      (error "calling undefined function")
      (return (get_binding 'return
          (interpret_parse_tree_return
            (get_function_body (get_closure (functionname expr) s class_name))
             (bind_parameters-cps (get_actual_params expr) (get_formal_params (get_closure (functionname expr) s class_name)) s
               (add_layer (get_function_environment expr s)) new_return_continuation)
             new_return_continuation break_error continue_error throw_error)))
    )
  )
)

(define get_formal_params car)
(define get_function_body cadr)
(define get_actual_params cddr)

; Evaluates the actual params and binds their values to the formal params
(define bind_parameters-cps
  (lambda (actual_params formal_params s functionenv return)
    (cond
      ((null? actual_params) (return functionenv))
      (else (Mvalue-cps (car actual_params) s
                        (lambda (v) (bind_parameters-cps (cdr actual_params) (cdr formal_params) s
                                                         (add_to_state (car formal_params) v functionenv)
                                                         (lambda (v2) (return v2)
                                                           ))
                          )
                        ))
      )
    )
  )

; Looks up and returns function closure from state
;(define get_closure
;  (lambda (expr s)
;    (get_binding (functionname expr) s)
;    )
;  )

; Adds an entry to the state of (function_name function_closure)
(define Mstate_function_def-cps
  (lambda (expr s return)
    (return (set_binding (functionname expr) (make_closure expr s) s))
    )
  )

; Adds an entry to the state of (classname class_def)
(define Mstate_class_def-cps
  (lambda (expr s return)
    (return (set_binding (classname expr) (class_def expr)))))

(define classname cadr)

; Calls a function with the proper function environment
; Side effects are handled naturally by using boxed values
; The initial state is returned, because the function will have updated any global vars via side effects
(define Mstate_function_call-cps
  (lambda (expr s return throw)
    (begin
      (interpret_parse_tree_return
       (get_function_body (get_closure expr s))
       (bind_parameters-cps (get_actual_params expr) (get_formal_params (get_closure expr s)) s
                            (add_layer (get_function_environment expr s)) new_return_continuation)
       new_return_continuation break_error continue_error throw_error)
      
      (return s)
      )
    ))

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
(define throw_error (lambda (v) (error 'no-try)))

; Starts the function call with returnImmediate which immediately exits function
(define interpret_parse_tree_return
  (lambda (parse_tree state return break continue throw)
    (call/cc
     (lambda (returnImmediate)
       (interpret_parse_tree parse_tree state return returnImmediate break continue throw)
       )
     )
    )
  )

; Takes a parse tree generated from Connamacher's parser, and interprets statements one at a time
(define interpret_parse_tree
  (lambda (parse_tree state return function_return break continue throw)
    (cond
      ((null? parse_tree) state)
      (else (interpret_parse_tree 
             (parse_tree_remainder parse_tree) 
             (Mstate-cps (parse_tree_statement parse_tree) 
                         state return function_return break continue throw) 
             return function_return break continue throw))
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
  (lambda (expr s return function_return break continue throw)
    (cond
      ((equal? (keyword expr) 'var)                 (Mstate_var-cps expr s return))
      ((equal? (keyword expr) '=)                   (Mstate_eq-cps expr s return))
      ((equal? (keyword expr) 'return)              (Mstate_return-cps expr s function_return throw))
      ((equal? (keyword expr) 'if)                  (Mstate_if-cps expr s return function_return break continue throw))
      ((equal? (keyword expr) 'while)               (Mstate_while-cps expr s return function_return throw))
      ((equal? (keyword expr) 'begin)               (Mstate_begin-cps expr s return function_return break continue throw))
      ((equal? (keyword expr) 'funcall)             (Mstate_function_call-cps expr s return throw))
      ((equal? (keyword expr) 'function)            (Mstate_function_def-cps expr s return))
      ((equal? (keyword expr) 'try)                 (Mstate_try-cps expr s return))
      ((equal? (keyword expr) 'throw)               (Mstate_throw-cps expr throw))
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
      ((defined_in_layer? (varname expr) (top_layer s)) (error "Redefining variable"))
      ((null? (cddr expr)) (return (set_binding (varname expr) 'null s)))
      (else (Mvalue-cps (initialvalue expr) s (lambda (v) (return 
                                                           (cons (car (set_binding (varname expr) v (top_layer_state s))) (remove_layer s))))))
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
    (cond
      ((defined_in_layer? (varname expr) (top_layer s)) (return (set_binding (varname expr) (right_op_val expr s) s)))
      ((defined? (varname expr) s) (return (update_binding (varname expr) (right_op_val expr s) s)))
      (else (error "Variable undefined or out of scope"))
      )
    )
  )

; Adds the return value into the state under name 'return
(define Mstate_return-cps
  (lambda (expr s function_return throw)
    (cond
      ((procedure? throw)(function_return (set_binding 'return (Mvalue-cps (returnexpr expr) s (lambda (v) v)) s)))
      (else (return_finally expr (Mstate-cps (finally_block throw) s new_return_continuation function_return break_error continue_error throw_error) function_return))
      )
    )
  )

; Abstractions for Mstate return
(define returnexpr cadr)
(define return_finally
  (lambda (expr s function_return)
    (function_return (set_binding 'return (Mvalue-cps (returnexpr expr) s (lambda (v) v)) s))
    )
  )

; Takes (if (cond) (then-expr) (else-expr))
; First evaluates the boolean condition.  If it's true, return Mstate(then-expr)
; If it's false, return just the state unchanged if no else statement, and
;  Mstate(else-expr) if there is an else statement.
(define Mstate_if-cps
  (lambda (expr s return function_return break continue throw)
    (cond
      ((Mboolean-cps (ifcond expr) s (lambda (v) v)) (Mstate-cps (iftruebody expr) s return function_return break continue throw))
      ((null? (ifnoelse expr)) s)
      (else (Mstate-cps (iffalsebody expr) s return function_return break continue throw))
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
; IMPORTANT: the letrec here is necessary due to begin statements (eg functions) having global side effects
; If we reevaluated the state everywhere that new_state is used, a global var could be incremented over and over again
(define Mstate_begin-cps
  (lambda (expr s return function_return break continue throw)
    (letrec ((new_state (interpret_parse_tree (begin_body expr) (add_layer s) return function_return (break_layer break) (continue_layer continue) throw)))
      (cond 
        ((defined? 'return new_state) (set_binding 'return (get_binding 'return (return new_state)) (remove_layer new_state)))
        (else (return (remove_layer new_state)))
        ))
    ))

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

;Evaluates the body of a try block and executes a try statement if an error is thrown
(define Mstate_try-cps
  (lambda (expr s return function_return)
    (if (hascatchorfinally expr)
        (call/cc (lambda (throw)
                   (Mstate-cps (trybody expr) s return function_return break (continue loop expr return break) expr)
                   )
                 ))
    (else error "No catch for a try")
  )
)

;abstractions for try
(define trybody cadr)
(define catchbody caddr)
(define finallybody cadddr)
  
(define hascatchorfinally
  (lambda (expr)
    (cond
      ((or (null? (catchbody expr)) (finallybody expr)) #t)
      (else #f)
      )
    )
  )
  
(define hascatchandfinally
  (lambda (expr)
    (cond
      ((and (not(null? (catchbody expr))) (not(null?(finallybody expr)))) #t)
      (else #f)
      )
    )
  )
  
(define hascatch
  (lambda (throw)
    (not (null? (catchbody throw)))
    )
  )
  
(define hasfinally
  (lambda (throw)
    (not (null? (catchbody throw)))
    )
  )
  
(define finally_block
  (lambda (throw)
    (finallybody throw)
    )
  )
  
(define catch_block
  (lambda (throw)
    (catchbody throw)
    )
  )

; throws the exception to the try block or finally
(define Mstate_throw-cps
  (lambda (expr throw)
    (cond
      ((procedure? throw) (throw(exeption expr)))
      ((hascatchandfinally throw) (executecatchfinally throw))
      ((hascatch throw) (executecatch throw))
      (else (executefinally expr))
      )
    )
  )
  
;abstractions for throw TODO: Fill in execute statements
(define exeption cadr)
  
(define executecatch
  (lambda (throw)
    error 'unimplemented
    )
  )
 
(define executecatchfinally
  (lambda (throw)
    error 'unimplemented
    )
  )
  
(define executefinally
  (lambda (throw)
    error 'unimplemented
    )
  )
    

;Evaluates a body based on a condition that is true and watches for break
;Takes (< i j) (= i (+ i 1)) s and return
;This part takes care of break and continue statements
(define Mstate_while-cps
  (lambda (expr s return function_return throw)
    (call/cc (lambda (break)
               (letrec (
                        (loop (lambda (expr s)
                                (cond
                                  ((Mboolean-cps (whilecond expr) s (lambda (v) v))
                                   (loop expr (Mstate-cps (whilebody expr) s return function_return break (continue loop expr return break) throw)))
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
      ((equal? expr 'true) (return #t))
      ((equal? expr 'false) (return #f))
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
      ((equal? (car expr) 'funcall) (Mvalue_function_call-cps expr s (lambda (v)
                                                                       (cond
                                                                         ((equal? v 'true) (return #t))
                                                                         ((equal? v 'false) (return #f))
                                                                         (return v)))))
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




;  =============
;  state_ops.scm
;  =============




; Searches for the first occurence of a variable in the layer and returns its value
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
    (cons (add_to_layer key (box value) (top_layer state)) (remove_layer state))
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
; Adds a (variable boxed_value) pair to the outermost layer of the state if the variable isn't there
; Updates the boxed_value if the variable is there
(define set_binding
  (lambda (key value s)
    (cond
      ((defined_in_layer? key (top_layer s)) (cons (update_binding_layer key value (top_layer s)) (remove_layer s)))
      (else (add_to_state key value s))
    )
  )
      )

; Updates the first binding of this variable in the layer using set_box!
; Returns the layer with the updated variable
(define update_binding_layer
  (lambda (key value layer)
    (cond
      ((eq? key (car (car layer))) (begin (set-box! (cadr (car layer)) value) layer))
      (else (cons (car layer) (update_binding_layer key value (cdr layer))))
    )
  )
      )

; Updates the first binding of this variable in the state using set_box!
; Returns the state with the updated variable
(define update_binding
  (lambda (key value state)
    (cond
      ((defined_in_layer? key (top_layer state)) (cons (update_binding_layer key value (top_layer state)) (remove_layer state)))
      (else (cons (top_layer state) (update_binding key value (remove_layer state))))
      )
    )
  )           

; Gets a variables value inside a given state
(define get_binding
  (lambda (key s)
    (cond
      ((equal? (state_search key s) 'error) 'error)
      (else (unbox (state_search key s)))
    )
  )
      )

; Gets a variables value inside a given layer
(define get_binding_layer
  (lambda (key layer)
    (cond
      ((equal? (layer_search key layer) 'error) 'error)
      (else (unbox (layer_search key layer)))
      )
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

; Checks if a given key has been initialized
(define defined_in_layer?
  (lambda (key layer)
    (not (eq? (layer_search key layer) 'error))
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
      (else (cons (layer_remainder key (top_layer s)) (remove_layer s)))
    )
  )
      )

; Gets the state that is just the global layer and nothing else!
(define global_state
  (lambda (s)
    (cond
      ((null? (cdr s)) s)
      (else (global_state (cdr s)))
      )))

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
(define top_layer_state
  (lambda (s)
    (list (car s))))
(define remove_layer cdr)


(define new_class '('null '() '() '()))
(define parent car)
(define field_environment cadr)
(define method_environment caddr)
(define instance_field_names cadddr)

(define get_def_name cadr)
(define get_def_extends caddr)
(define get_def_body cadddr)
(define get_def_parent_name
    (lambda (d)
        (cadr (get_def_extends d))))

(define class_def
    (lambda (d s)
        (cond
            ((null? (get_def_extends d)) (class_body_def (get_def_body d) new_class s))   ; ('null env ifn)
            (else (class_body_def (get_def_body d) (cons (get_def_parent_name d) (cdr new_class)) s))  ; (B env ifn)
        )
    )
)

(define class_body_def ; change the second and third things in the class tuple to be the field/method envs.
    (lambda (body class s)
        ((cons (parent class)
         (cons (get_field_environment body '())
         (cons (get_method_environment body '() s)
         (cdddr class)))))))

(define get_field_environment
    (lambda (body env)
        (cond
            ((null? body) env)
            ((equal? 'static-var (car (car body)))
                (cond
                    ((null? (cddr (car body))) (get_field_environment (cdr body) (add_to_layer (cadr (car body)) 'null env)))
                    (else (Mvalue-cps (caddr (car body)) (list env) (lambda (v) (get_field_environment (cdr body) (add_to_layer (cadr (car body)) v env)))))
                )
            )
            (else (get_field_environment (cdr body) env))
        )))


(define get_method_environment
    (lambda (body env s)
        (cond
            ((null? body) env)
            ((equal? 'static-function (car (car body)))
                (cond
                    ((null? (cddr (car body))) (get_field_environment (cdr body) (add_to_layer (cadr (car body)) 'null env)))
                    (else (get_field_environment (cdr body) (add_to_layer (cadr (car body)) (make_closure (caddr (car body)) s))))
                )
            )
            (else (get_field_environment (cdr body) env))
        )))


; Gets i.e. A.x, you would call with (key=x, class_name=A, state=s)
; If the class name exists, search its field_environment list for the (tbc)
(define get_field_binding
    (lambda (key class_name s)
        (cond
            ((equal? 'null class_name) (get_binding key s))
            ((equal? 'error (get_binding class_name s)) 'error)
            (else (get_field_binding_in_class key (get_binding class_name s) s))
)))

(define get_field_binding_in_class
    (lambda (key class s)
        (cond
            ((equal? 'error (state_search key (field_environment class))) (get_field_binding key (parent class) s))
            (else (state_search key (field_environment class)))
        )
    )
)

(define get_closure
    (lambda (key class_name s)
        (cond
            ((equal? 'null class_name) (get_binding key s))
            ((equal? 'error (get_binding class_name s)) 'error)
            (else (get_closure_in_class key (get_binding class_name s) s))
)))

(define get_closure_in_class
    (lambda (key class s)
        (cond
            ((equal? 'error (state_search key (method_environment class))) (get_closure key (parent class) s))
            (else (state_search key (method_environment class)))
        )
    )
)

;(initial_environment (parser "tests4/1" "A"))

