;For current testing
;(load "functionParser.scm")
(load "classParser.scm")
;(load "basicParser.scm")

; Kicks off the interpreter for classes
(define interpretClass
  (lambda (filename class_name)
     (prettify_result
      (interpret_main_in_class (parser filename) class_name))))

; Kicks off the interpreter for functions
(define interpretFunction
  (lambda (filename)
    (prettify_result
     (interpret_main (parser filename))
     )
    )
  )

; Kicks off the interpreter for code
(define interpretCode
  (lambda (filename)
    (prettify_result
     (get_binding 'return
                  (interpret_parse_tree_return
                   (parser filename)
                   new_state
                   new_return_continuation continue_error break_error throw_error))
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

; Runs the main function in the given class
(define interpret_main_in_class
  (lambda (parsed class_name)
    (Mvalue_function_call-cps '(funcall main) (initial_environment parsed class_name) new_return_continuation throw_error class_name)))

; Runs the main function
(define interpret_main
  (lambda (parsed)
    (Mvalue_function_call-cps '(funcall main) (initial_environment parsed) throw_error new_return_continuation throw_error class_name)
    )
  )

; Creates the "outer state" by making a first pass on the parse tree, then adds a new layer to it
(define initial_environment 
  (lambda (parse_tree class_name)
    (interpret_outer_parse_tree-cps parse_tree new_state new_return_continuation class_name)
    )
  )

; First pass of parse tree to build "outer state"
(define interpret_outer_parse_tree-cps
  (lambda (parse_tree state return class_name)
    (cond
      ((null? parse_tree) (return state))
      (else (interpret_outer_parse_tree-cps 
             (parse_tree_remainder parse_tree) 
             (Mstate_outer-cps (parse_tree_statement parse_tree) state new_return_continuation class_name) 
             return
             class_name))
      )
    )
  )

; Gets environment function from function closure and calls it with a state to return function environment
(define get_function_environment
  (lambda (expr class_name s)
    ((caddr (get_closure (functionname expr) class_name s)) s)
    )
  )

; Mstate function for building the outer state. Only variable definitions, variable assignment, and function definitions are allowed outside of a function
(define Mstate_outer-cps
  (lambda (expr s return class_name)
    (cond
      ((equal? (keyword expr) 'var)       (Mstate_var-cps expr s return throw_error class_name))
      ((equal? (keyword expr) '=)         (Mstate_eq-cps expr s throw_error return))
      ((equal? (keyword expr) 'function)  (Mstate_function_def-cps expr s return class_name))
      ((equal? (keyword expr) 'class)     (Mstate_class_def-cps expr s return))
      (else (return state))
      )
    )
  )

; Evaluates a function call and returns its value
; This is done by treating a function as a subprogram and returning the 'return binding in the resulting state
(define Mvalue_function_call-cps
  (lambda (expr s return throw class_name)
    ;(display "\n\n")
    ;(display "mvalue function call: ")
    ;(display class_name)
    ;(display "\n")
    ;(display expr)
    ;(display "\n")
    ;(display s)
    ;(display "\n")
    ;(display "calling function as: ")
    ;(display (get_function_class expr s class_name))
    (if (eq? (get_closure (functionname expr)  (get_function_class expr s class_name) s) 'error)
      (error "calling undefined function")
      (return (get_binding 'return
          (interpret_parse_tree_return
            (get_function_body (get_closure (functionname expr) (get_function_class expr s class_name) s))
             (bind_parameters-cps (get_actual_params expr) (get_formal_params (get_closure (functionname expr) (get_function_class expr s class_name) s)) s
               (add_layer (get_function_environment expr  (get_function_class expr s class_name) s)) new_return_continuation throw class_name)
             new_return_continuation break_error continue_error throw (get_function_class expr s class_name))))
    )
  )
)

(define get_formal_params car)
(define get_function_body cadr)
(define get_actual_params cddr)

; Evaluates the actual params and binds their values to the formal params
(define bind_parameters-cps
  (lambda (actual_params formal_params s functionenv return throw class_name)
    ;(display "\n\n")
    ;(display "bind params: ")
    ;(display class_name)
    ;(display "\n")
    ;(display actual_params)
    ;(display "\n")
    ;(display formal_params)
    ;(display "\n")
    ;(display s)
    (cond
      ((null? actual_params) (return functionenv))
      (else (Mvalue-cps (car actual_params) s
                        (lambda (v) (bind_parameters-cps (cdr actual_params) (cdr formal_params) s
                                                         (add_to_state (car formal_params) v functionenv)
                                                         (lambda (v2) (return v2)
                                                           ) throw class_name)
                          ) throw class_name)
       )
      )
    )
  )

; Adds an entry to the state of (function_name function_closure)
(define Mstate_function_def-cps
  (lambda (expr s return class_name)
    (return (set_closure_binding (functionname expr) (make_closure expr s class_name) class_name s))
    )
  )

; Adds an entry to the state of (classname class_def)
(define Mstate_class_def-cps
  (lambda (expr s return)
    (return (class_def expr s))
  )); No need to even pretend to be calling set_*_binding here, no inner classes.

(define classname cadr)

; Calls a function with the proper function environment
; Side effects are handled naturally by using boxed values
; The initial state is returned, because the function will have updated any global vars via side effects
(define Mstate_function_call-cps
  (lambda (expr s return throw class_name)
    ;(display "\n\n")
    ;(display "mstate function call: ")
    ;(display class_name)
    ;(display "\n")
    ;(display expr)
    ;(display "\n")
    ;(display s)
    ;(display "\n")
    ;(display "function class: ")
    ;(display (get_function_class expr s class_name))
    ;(display "\n")
    ;(display (functionname expr))
    ;(display "\n")
    ;(display (get_closure (functionname expr) (get_function_class expr s class_name) s))
    ;(display "\ncalling function as: ")
    ;(display (get_function_class expr s class_name))
  
    (begin
      (interpret_parse_tree_return
       (get_function_body (get_closure (functionname expr) (get_function_class expr s class_name) s))
       (bind_parameters-cps (get_actual_params expr) (get_formal_params (get_closure (functionname expr) (get_function_class expr s class_name) s)) s
                            (add_layer (get_function_environment expr (get_function_class expr s class_name) s)) new_return_continuation throw class_name)
       new_return_continuation break_error continue_error throw (get_function_class expr s class_name))
      
      (return s)
      )
    ))

(define get_function_class
  (lambda (expr s class_name)
    (cond
      ((not (list? (cadr expr)))
       (cond 
         ((defined? (cadr expr) (method_environment (get_binding class_name s))) class_name)
         ((defined? (cadr expr) s) class_name)
         ((eq? 'null class_name) (error "undefined function"))
         (else (get_function_class expr s (parent (get_binding class_name s))))
        ))
      ((equal? (cadr (cadr expr)) 'super) (parent (get_binding class_name s)))
      ((defined? (caddr (cadr expr)) (method_environment (get_binding (cadr (cadr expr)) s))) (cadr (cadr expr)))
      (else (get_function_class expr s (parent (get_binding (cadr (cadr expr)) s)))))))

(define get_field_class
  (lambda (expr s class_name)
    ;(display "\n\n")
    ;(display "get field class: ")
    ;(display class_name)
    ;(display "\n")
    ;(display expr)
    ;(display "\n")
    ;(display s)
    ;(display "\n")
    ;(display (eq? (cadr expr) 'super))
    ;(display (parent (get_binding class_name s)))
    (cond
      ((eq? (cadr expr) 'super) (parent (get_binding class_name s)))
      ((defined? (caddr expr) (field_environment (get_binding (cadr expr) s))) (cadr expr))
      (else (get_field_class expr s (parent (get_binding (cadr expr) s)))))))

; Some abstractions for parsing out the pieces of a function definition expression
(define functionname 
  (lambda (expr)
  (cond
    ((list? (cadr expr)) (caddr (cadr expr))) ;(funcall (dot super getSize) ..)
    (else (cadr expr))))) ;(funcall getSize ...)
(define arglist caddr)
(define functionbody cadddr)

; Function closure is (formal_parameters, function_body, function_that_creates_function_environment)
(define make_closure
  (lambda (expr s class_name)
    (list (arglist expr) (functionbody expr) (functionenvironment class_name))
    )
  )

; Returns a function that generates a function environment given the current state
(define functionenvironment
  (lambda (name)
    (lambda (state)
      ;(state_remainder name state)
      state
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
  (lambda (parse_tree state return break continue throw class_name)
    (call/cc
     (lambda (returnImmediate)
       (interpret_parse_tree parse_tree state return returnImmediate break continue throw class_name)
       )
     )
    )
  )

; Takes a parse tree generated from Connamacher's parser, and interprets statements one at a time
(define interpret_parse_tree
  (lambda (parse_tree state return function_return break continue throw class_name)
    (cond
      ((null? parse_tree) state)
      (else (interpret_parse_tree 
             (parse_tree_remainder parse_tree) 
             (Mstate-cps (parse_tree_statement parse_tree) 
                         state return function_return break continue throw class_name) 
             return function_return break continue throw class_name))
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
  (lambda (expr s return function_return break continue throw class_name)
    (cond
      ((equal? (keyword expr) 'var)                 (Mstate_var-cps expr s return throw class_name))
      ((equal? (keyword expr) '=)                   (Mstate_eq-cps expr s return throw class_name))
      ((equal? (keyword expr) 'return)              (Mstate_return-cps expr s function_return throw class_name))
      ((equal? (keyword expr) 'if)                  (Mstate_if-cps expr s return function_return break continue throw class_name))
      ((equal? (keyword expr) 'while)               (Mstate_while-cps expr s return function_return throw class_name))
      ((equal? (keyword expr) 'begin)               (Mstate_begin-cps expr s return function_return break continue throw class_name))
      ((equal? (keyword expr) 'funcall)             (Mstate_function_call-cps expr s return throw class_name))
      ((equal? (keyword expr) 'try)                 (Mstate_try-cps expr s return function_return break continue throw class_name))
      ((equal? (keyword expr) 'throw)               (throw (list s expr)))
      ((equal? (keyword expr) 'function)            (Mstate_function_def-cps expr s return class_name))
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
  (lambda (expr s return throw class_name)
    (cond
      ((defined_in_layer? (varname expr) (top_layer s)) (error "Redefining variable"))
      ((null? (cddr expr)) (return (set_binding (varname expr) 'null s)))
      (else (Mvalue-cps (initialvalue expr) s (lambda (v) (return 
                                                           (cons (car (set_binding (varname expr) v (top_layer_state s))) (remove_layer s)))) throw class_name))
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
  (lambda (expr s return throw class_name)
    ;(display "\n\n")
    ;(display "mstate eq: ")
    ;(display class_name)
    ;(display "\n")
    ;(display expr)
    ;(display "\n")
    ;(display s)
    ;(cond
    ;  ((defined_in_layer? (varname expr) (top_layer s)) (return (set_binding (varname expr) (right_op_val expr s throw class_name) s)))
    ;  ((defined? (varname expr) s) (return (update_binding (varname expr) (right_op_val expr s throw class_name) s)))
    ;  (else (error "Variable undefined or out of scope"))
    ;  )
    (return (set_field_binding (varname expr) (right_op_val expr s throw class_name) class_name s))
    )
  )

; Adds the return value into the state under name 'return
(define Mstate_return-cps
  (lambda (expr s function_return throw class_name)
    (function_return (set_binding 'return (Mvalue-cps (returnexpr expr) s (lambda (v) v) throw class_name) s))
    )
  )

;abstractions for return
(define returnexpr cadr)

; Takes (if (cond) (then-expr) (else-expr))
; First evaluates the boolean condition.  If it's true, return Mstate(then-expr)
; If it's false, return just the state unchanged if no else statement, and
;  Mstate(else-expr) if there is an else statement.
(define Mstate_if-cps
  (lambda (expr s return function_return break continue throw class_name)
    (cond
      ((Mboolean-cps (ifcond expr) s (lambda (v) v) throw class_name) (Mstate-cps (iftruebody expr) s return function_return break continue throw class_name))
      ((null? (ifnoelse expr)) s)
      (else (Mstate-cps (iffalsebody expr) s return function_return break continue throw class_name))
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
  (lambda (expr s return function_return break continue throw class_name)
    (letrec ((new_state (interpret_parse_tree (begin_body expr) (add_layer s) return function_return (break_layer break) (continue_layer continue) throw class_name)))
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
;The throw is the expr comming in which will be used for catch
;This takes cares of the fi
(define Mstate_try-cps
  (lambda (expr s return function_return break continue throw class_name)
    (if(hasfinally expr)
       (executefinally (finally_block expr) 
                       (try_catch expr s return
                                  (lambda (v) 
                                    (return (executefinally (finally_block expr) v return function_return break continue throw class_name)))
                                  break continue throw class_name)
                       return function_return break continue throw class_name)
       (try_catch expr s return function_return break continue throw class_name)
       )

    )
)

;takes care of catch
(define try_catch
  (lambda (expr s return function_return break continue lastThrow class_name)
    (call/cc (lambda (throw)
               (if(hascatch expr)
                  (interpret_parse_tree 
                   (trybody expr) s return 
                   function_return
                   break continue (lambda (v) (throw (executecatch (catch_block expr)
                                                                   (set_binding (catchvariable (catch expr)) (get_binding (catchvalue v) (catchstate v)) s)
                                                                   return function_return break continue lastThrow class_name))) class_name)
                  (interpret_parse_tree 
                   (trybody expr) s return 
                   function_return
                   break continue throw class_name)
                  )
               )

             )
    )
  )

;abstractions for try
(define catchvariable caadar)
(define catchstate car)
(define catchvalue cadadr)
(define trybody cadr)
(define catch cddr)
(define bodyofcatch caddar)
(define finally cdddr)
(define bodyoffinally cadar)
(define exeptionofcatch cadr)
  
(define hascatch
  (lambda (expr)
    (if (not(null? (car (catch expr))))
    (equal? (caar (catch expr)) 'catch)
    #f)
    )
  )
  
(define hasfinally
  (lambda (expr)
    (if (not(null? (car (finally expr))))
    (equal? (caar (finally expr)) 'finally)
    #f)
  )
  )
  
(define catch_block
  (lambda (s)
    (if (hascatch s)
    (bodyofcatch(catch s))
    '())
    )
  )

(define finally_block
  (lambda (s)
    (if (hasfinally s)
    (bodyoffinally(finally s))
    '())
    )
  )
  
;abstractions for throw
(define exeption cadr)
  
(define executecatch
  (lambda (expr s return function_return break continue throw class_name)
    (interpret_parse_tree expr s return function_return break continue throw class_name))
  )
  
(define executefinally
  (lambda (expr s return function_return break continue throw class_name)
    (interpret_parse_tree expr s return function_return break continue throw class_name)
  )
  )
    

;Evaluates a body based on a condition that is true and watches for break
;Takes (< i j) (= i (+ i 1)) s and return
;This part takes care of break and continue statements
(define Mstate_while-cps
  (lambda (expr s return function_return throw class_name)
    (call/cc (lambda (break)
               (letrec (
                        (loop (lambda (expr s)
                                (cond
                                  ((Mboolean-cps (whilecond expr) s (lambda (v) v) throw class_name)
                                   (loop expr (Mstate-cps (whilebody expr) s return function_return break (continue loop expr return break) throw class_name)))
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
  (lambda (expr s return throw class_name)
    ;(display "\n\n")
    ;(display "mvalue call: ")
    ;(display class_name)
    ;(display "\n")
    ;(display expr)
    ;(display "\n")
    ;(display s)
    (cond
      ((number? expr) (return expr))
      ((equal? expr 'true) (return #t))
      ((equal? expr 'false) (return #f))
      ((and (not (list? expr)) (eq? (get_binding expr s) 'error)) (return (get_field_binding expr class_name s)))
      ((not (list? expr)) (return (get_binding expr s)))
      ;((not (list? expr)) (return (get_field_binding expr class_name s)))
      ((equal? (operator expr) '+) (return (+ (left_op_val expr s throw class_name) (right_op_val expr s throw class_name))))
      ((equal? (operator expr) '-)
       (if (null? (cddr expr)) 
           (return (- 0 (left_op_val expr s throw class_name)))
           (return (- (left_op_val expr s throw class_name) (right_op_val expr s throw class_name)))))
      ((equal? (operator expr) '*) (return (* (left_op_val expr s throw class_name) (right_op_val expr s throw class_name))))
      ((equal? (operator expr) '/) (return (/ (- (left_op_val expr s throw class_name) (modulo (left_op_val expr s throw class_name) (right_op_val expr s throw class_name))) (right_op_val expr s throw class_name)))) ; Integer division:  (x - (x % y)) / y
      ((equal? (operator expr) '%) (return (modulo (left_op_val expr s throw class_name) (right_op_val expr s throw class_name))))
      ((equal? (operator expr) 'funcall) (Mvalue_function_call-cps expr s (lambda (v) (return v)) throw class_name))
      ((logical_operator? (operator expr)) (Mboolean-cps expr s (lambda (v) (return v)) throw class_name))
      ((equal? (operator expr) 'dot) (return (get_field_binding (caddr expr) (get_field_class expr s class_name) s)))
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
  (lambda (expr s return throw class_name)
    (cond
      ((boolean? expr) (return expr))
      ((equal? expr 'true) (return #t))
      ((equal? expr 'false) (return #f))
      ((and (not (list? expr)) (eq? (get_binding expr s) 'error)) (return (get_field_binding expr class_name s)))
      ((not (list? expr)) (return (get_binding expr s)))
      ((equal? (car expr) '||) (Mboolean-cps (caddr expr) s (lambda (v1) (Mboolean-cps (cadr expr) s (lambda (v2) (return (or v1 v2))) throw class_name)) throw class_name))
      ((equal? (car expr) '&&) (Mboolean-cps (caddr expr) s (lambda (v1) (Mboolean-cps (cadr expr) s (lambda (v2) (return (and v1 v2))) throw class_name)) throw class_name))
      ((equal? (car expr) '!=) (return (not (equal? (left_op_val expr s throw class_name) (right_op_val expr s throw class_name)))))
      ((equal? (car expr) '!) (Mboolean-cps (cadr expr) s (lambda (v) (return (not v))) throw class_name))
      ((equal? (car expr) '>) (return (> (left_op_val expr s throw class_name) (right_op_val expr s throw class_name))))
      ((equal? (car expr) '<) (return (< (left_op_val expr s throw class_name) (right_op_val expr s throw class_name))))
      ((equal? (car expr) '>=) (return (>= (left_op_val expr s throw class_name) (right_op_val expr s throw class_name))))
      ((equal? (car expr) '<=) (return (<= (left_op_val expr s throw class_name) (right_op_val expr s throw class_name))))
      ((equal? (car expr) '==) (return (equal? (left_op_val expr s throw class_name) (right_op_val expr s throw class_name))))
      ((equal? (car expr) 'funcall) (Mvalue_function_call-cps expr s (lambda (v)
                                                                       (cond
                                                                         ((equal? v 'true) (return #t))
                                                                         ((equal? v 'false) (return #f))
                                                                         (return v))) throw class_name))
      )
    ))

(define left_op_val
  (lambda (expr s throw class_name)
    ;(display "\n\n")
    ;(display "left op val: ")
    ;(display class_name)
    ;(display "\n")
    ;(display expr)
    ;(display "\n")
    ;(display s)
    ;(display "\n")
    ;(display (Mvalue-cps (cadr expr) s (lambda (v) v) throw class_name))
    (Mvalue-cps (cadr expr) s (lambda (v) v) throw class_name)
    ))

(define right_op_val
  (lambda (expr s throw class_name)
    (Mvalue-cps (caddr expr) s (lambda (v) v) throw class_name)
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
        (error "Referencing variable before assignment:" key)
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


(define new_class '(null (()) (()) ()))
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

; Returns the state having added the class (with its static fields and methods)
(define class_def
    (lambda (d s)
        (cond
            ;Adds (classname new_class) to the state and calls class_body_def
            ((null? (get_def_extends d)) (class_body_def (get_def_body d) (set_binding (get_def_name d) new_class s) (get_def_name d)))   ; ('null env ifn)
            ;Adds (classname new_class_with_parent_name) to the state and calls class_body_def
            (else (class_body_def (get_def_body d) (set_binding (get_def_name d) (cons (get_def_parent_name d) (cdr new_class)) s) (get_def_name d)))  ; (B env ifn)
        )
    )
)

; Returns the state having added the static fields and methods to the class
(define class_body_def ; change the second and third things in the class tuple to be the field/method envs.
    (lambda (body s class_name)
      (cond
        ((null? body) s)
        ((equal? 'static-var (car (car body))) (class_body_def (cdr body) (add_to_field_environment (car body) s class_name) class_name)) ;add field to class's static fields
        ((equal? 'static-function (car (car body))) (class_body_def (cdr body) (add_to_method_environment (car body) s class_name) class_name));add method to class's static methods
        (else (class_body_def (cdr body) s class_name)))))
        

(define add_to_field_environment
    (lambda (expr s class_name)
        (letrec ((class (get_binding class_name s)))
        (cond
          ((null? (cddr expr)) (set_binding class_name  (list (parent class) (set_binding (cadr expr) 'null (field_environment class)) (method_environment class) (instance_field_names class)) s))
          (else (set_binding class_name (list (parent class) (set_binding (cadr expr) (Mvalue-cps (caddr expr) s new_return_continuation throw_error class_name) (field_environment class)) (method_environment class) (instance_field_names class)) s))
          ))))


(define add_to_method_environment
    (lambda (expr s class_name)
        (letrec ((class (get_binding class_name s)))
        ;(display "\n\n")
        ;(display "class name in add_method: ")
        ;(display class_name)
        ;(display "\n")
        ;(display expr)
        ;(display "\n")
        ;(display class)
        ;(display "\n")
        ;(display (cadr expr))
        ;(display "\n")
        ;(display (make_closure expr s class_name))
        ;(display "\n")
        ;(display (set_binding (cadr expr) (make_closure expr s class_name) (method_environment class)))
        ;(display "\n")
        ;(display (list (parent class) (field_environment class) (set_binding (cadr expr) (make_closure expr s class_name) (method_environment class)) (instance_field_names class)))
        (cond
          ((null? (cddr expr)) (set_binding class_name (list (parent class) (field_environment class) (set_binding (cadr expr) 'null (method_environment class)) (instance_field_names class))) s)
          (else (set_binding class_name (list (parent class) (field_environment class) (set_binding (cadr expr) (make_closure expr s class_name) (method_environment class)) (instance_field_names class)) s))
          ))))


; Gets i.e. A.x, you would call with (key=x, class_name=A, state=s)
; If the class name exists, search its field_environment list for the (tbc)
(define get_field_binding
    (lambda (key class_name s)
      ;(display "\n\n")
      ;(display "get field binding: ")
      ;(display class_name)
      ;(display "\n")
      ;(display key)
      ;(display "\n")
      ;(display s)
        (cond
            ((and (list? key) (eq? 'dot (car key))) (cond   ; (dot A x) or (dot super x)
                ((eq? 'super (cadr key)) (get_field_binding (caddr key) (parent (get_binding class_name s)) s))
                (else (get_field_binding (caddr key) (cadr key) s))
            ))
            (else 
                (cond
                    ((equal? 'null class_name) (get_binding key s))
                    ((not (equal? 'error (get_binding_layer key (car s)))) (get_binding_layer key (car s)))
                    ((equal? 'error (get_binding class_name s)) 'error)
                    (else (get_field_binding_in_class key (get_binding class_name s) s))
                )
            )
        )
    )
)

(define get_field_binding_in_class
    (lambda (key class s)
        (cond
            ((equal? 'error (get_binding key (field_environment class))) (get_field_binding key (parent class) s))
            (else (get_binding key (field_environment class)))
        )
    )
)

; Something to be called by set_binding if it gets a dot thing, i.e. A.x = 5 would be set_field_binding(x, 5, A, s)
; We would additionally want to make it so that if we were in class A and x=5, the same thing happened... TODO
; If the class_name comes up 'null, then change in the outer environment of s.  i.e. if x=5 and x is global, not a field of A.
; I don't do this, but you could call this for everything.  If you aren't looking to set a value within the context of a class, make class_name 'null,
; and it will fall-through to the regular set_binding operation.
(define set_field_binding
  (lambda (key val class_name s)
    (letrec ((class (get_binding class_name s)))
    (cond
      ((and (list? key) (eq? 'dot (car key))) (cond
        ((eq? 'super (cadr key)) (set_field_binding (caddr key) val (parent (get_binding class_name s)) s)) ; (dot parent x) call with parent class
        (else (set_field_binding (caddr key) val (cadr key) s))
      ))
      (else
        (cond
          ((defined_in_layer? key (top_layer s)) (set_binding key val s))
          ((defined? key s) (update_binding key val s))
          ((equal? 'error (get_binding class_name s)) 'error)
          ((defined? key (field_environment class)) (set_binding class_name (list (parent class) (update_binding key val (field_environment class)) (method_environment class) (instance_field_names class)) s))
          ((equal? 'null class_name) (set_binding key val s))
          (else (set_field_binding_in_class key val (get_binding class_name s) s))
        )
      )
    ))
  )
)

(define set_field_binding_in_class
  (lambda (key val class s)
    (cond
      ((equal? 'error (get_binding key (field_environment class))) (set_field_binding key val (parent class) s)) ; call recursively on parent.
      (else (set_binding key val (field_environment class)))  ; This should update the class definition, i.e. for a static variable.  A.x=5
    )
  )
)

(define get_closure
    (lambda (key class_name s)
        (cond
            ((and (list? key) (eq? 'dot (car key))) (cond   ; (dot A x) or (dot super x)
                ((eq? 'super (cadr key)) (get_closure (caddr key) (parent (get_binding class_name s)) s))
                (else (get_closure (caddr key) (cadr key) s))
            ))
            (else 
                (cond
                    ((eq? 'null class_name) (get_binding key s))
                    ((eq? 'error (get_binding class_name s)) 'error)
                    (else (get_closure_in_class key (get_binding class_name s) s))
                )
            )
        )
))

(define get_closure_in_class
    (lambda (key class s)
        (cond
            ((equal? 'error (get_binding key (method_environment class))) (get_closure key (parent class) s))
            (else (get_binding key (method_environment class)))
        )
    )
)

; Same code as for fields, but will update the closure environment of a class.  Drops through to set_binding if class_name is 'null as usual.
(define set_closure_binding
  (lambda (key val class_name s)
    (cond
      ((and (list? key) (eq? 'dot (car key))) (cond
        ((eq? 'super (cadr key)) (set_closure_binding (caddr key) val (parent (get_binding class_name s)) s))
        (else (set_closure_binding (caddr key) val (cadr key) s))
      ))
      (else
        (cond
          ((equal? 'null class_name) (set_binding key val s))
          ((equal? 'error (get_binding class_name s)) 'error)
          (else (set_closure_binding_in_class key val (get_binding class_name s) s))
        )
      )
    )
  )
)

(define set_closure_binding_in_class
  (lambda (key val class s)
    (cond
      ((equal? 'error (get_binding key (method_environment class))) (set_closure_binding key val (parent class) s)) ; call recursively on parent.
      (else (set_binding key val (method_environment class)))  ; This should update the class definition, i.e. for a static variable.  A.x=5
    )
  )
)

;(initial_environment (parser "tests4/2") 'A)
;(state_remainder 'A (initial_environment (parser "tests4/2") 'A))
;(interpretClass "tests4/2" 'A)
;(parser "tests4/7")
;(initial_environment (parser "tests4/7") 'A)
;(interpretClass "tests4/7" 'A)

