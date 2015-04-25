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
    (Mvalue_function_call-cps '(funcall main) (initial_environment parsed class_name) new_return_continuation throw_error class_name 'null)))

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
  (lambda (expr class_name instance s)
    ((caddr (get_closure (functionname expr) class_name instance s)) s)
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
  (lambda (expr s return throw class_name instance)
    ;(display "\n\n")
    ;(display "mvalue function call: ")
    ;(display class_name)
    ;(display ", ")
    ;(display instance)
    ;(display "\n")
    ;(display expr)
    ;(display "\n")
    ;(display (cadr expr))
    ;(display "\n")
    ;(display s)
    ;(display "\n")
    (letrec ((function_class (get_function_class expr s (initial_function_class expr s class_name instance) instance)) (function_instance (get_function_instance expr s class_name instance)))
    (letrec ((function_closure (get_closure (cadr expr) function_class function_instance s)))
    (if (eq? function_closure 'error)
      (error "calling undefined function")
      (return (get_binding 'return
          (interpret_parse_tree_return
            (get_function_body function_closure)
             (bind_parameters-cps (get_actual_params expr) (get_formal_params function_closure) s
               (add_layer (get_function_environment expr function_class function_instance s)) new_return_continuation throw class_name instance)
             new_return_continuation break_error continue_error throw function_class function_instance)))
    )))))

(define get_formal_params car)
(define get_function_body cadr)
(define get_actual_params cddr)

; Evaluates the actual params and binds their values to the formal params
(define bind_parameters-cps
  (lambda (actual_params formal_params s functionenv return throw class_name instance)
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
                                                           ) throw class_name instance)
                          ) throw class_name instance)
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
  (lambda (expr s return throw class_name instance)
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
    (letrec ((function_class (get_function_class expr s class_name instance)) (function_instance (get_function_instance expr s class_name instance)))
    (begin
      (interpret_parse_tree_return
       (get_function_body (get_closure (functionname expr) function_class function_instance s))
       (bind_parameters-cps (get_actual_params expr) (get_formal_params (get_closure (functionname expr) function_class function_instance s)) s
                            (add_layer (get_function_environment expr function_class function_instance s)) new_return_continuation throw class_name instance)
       new_return_continuation break_error continue_error throw function_class function_instance)
      
      (return s)
      ))
    ))

(define initial_function_class
  (lambda (expr s class_name instance)
    ;(display "\n\ninitial function class: ")
    ;(display class_name)
    ;(displayy ", ")
    ;(display instance)
    ;(display "\n")
    ;(display s)
    (if (and (not (list? (cadr expr))) (instance_any_has_method (cadr expr) class_name instance s))
       (car instance)
       class_name
     ))) 

(define get_function_class
  (lambda (expr s class_name instance)
    ;(display "\n\n")
    ;(display "get function class: ")
    ;(display class_name)
    ;(display ", ")
    ;(display instance)
    ;(displayy "\n")
    ;(display expr)
    ;(display "\n")
    ;(display s)
    ;(displayy "\n")
    (cond
      ((not (list? (cadr expr)))
       (cond 
         ((defined? (cadr expr) (static_method_environment (get_binding class_name s))) class_name)
         ((defined? (cadr expr) s) class_name)
         ((eq? 'null class_name) (error "undefined function"))
         ((instance_has_method (cadr expr) class_name instance s) class_name)
         (else (get_function_class expr s (parent (get_binding class_name s)) instance))
        ))
      ((equal? (cadr (cadr expr)) 'super) (parent (get_binding class_name s)))
      ((equal? (cadr (cadr expr)) 'this) (get_function_class (list (car expr) (caddr (cadr expr))) s (car instance) instance))
      ((is_instance? (cadr (cadr expr)) class_name instance s) (car (get_field_binding (cadr (cadr expr)) class_name instance s)))
      ((defined? (caddr (cadr expr)) (static_method_environment (get_binding (cadr (cadr expr)) s))) (cadr (cadr expr)))
      (else (get_function_class expr s (parent (get_binding (cadr (cadr expr)) s)) instance)))))

(define get_function_instance
  (lambda (expr s class_name instance)
    ;(display "\n\n")
    ;(display "get function instance ")
    ;(display class_name)
    ;(display ", ")
    ;(display instance)
    ;(display "\n")
    ;(displayyyyyyyyyyyyyy expr)
    ;(display "\n")
    ;(display s)
    ;(display "\n")
    (cond
      ((not (list? (cadr expr))) instance)
      ((eq? (cadr (cadr expr)) 'super) instance)
      ((eq? (cadr (cadr expr)) 'this) instance)
      ((is_class? (cadr (cadr expr)) s) instance) 
      (else (get_field_binding (cadr (cadr expr)) class_name instance s)))
    ))

(define get_field_class
  (lambda (expr s class_name instance)
    ;(display "\n\n")
    ;(displayy "get field class: ")
    ;(displayy class_name)
    ;(displayy "\n")
    ;(display expr)
    ;(display "\n")
    ;(displayy s)
    ;(display "\n")
    (cond
      ((eq? (cadr expr) 'super) (parent (get_binding class_name s)))
      ;((eq? (cadr expr) 'super) class_name)
      ((eq? (cadr expr) 'this) class_name)
      ((is_instance? (cadr expr) class_name instance s) class_name) 
      ((defined? (caddr expr) (static_field_environment (get_binding (cadr expr) s))) (cadr expr))
      (else (get_field_class (list (car expr) (parent (get_binding (cadr expr) s)) (caddr expr)) s class_name instance)))))

(define get_field_instance
  (lambda (expr s class_name instance)
    ;(display "\n\n")
    ;(display "get field instance: ")
    ;(display class_name)
    ;(displayy "\n")
    ;(display expr)
    ;(display "\n")
    ;(display s)
    ;(displayy "\n")
    (cond
      ((eq? (cadr expr) 'super) instance)
      ((eq? (cadr expr) 'this) instance)
      ((is_class? (cadr expr) s) instance) 
      (else (get_field_binding (cadr expr) class_name instance s)))))

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
  (lambda (parse_tree state return break continue throw class_name instance)
    (call/cc
     (lambda (returnImmediate)
       (interpret_parse_tree parse_tree state return returnImmediate break continue throw class_name instance)
       )
     )
    )
  )

; Takes a parse tree generated from Connamacher's parser, and interprets statements one at a time
(define interpret_parse_tree
  (lambda (parse_tree state return function_return break continue throw class_name instance)
    (cond
      ((null? parse_tree) state)
      (else (interpret_parse_tree 
             (parse_tree_remainder parse_tree) 
             (Mstate-cps (parse_tree_statement parse_tree) 
                         state return function_return break continue throw class_name instance) 
             return function_return break continue throw class_name instance))
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
  (lambda (expr s return function_return break continue throw class_name instance)
    (cond
      ((equal? (keyword expr) 'var)                 (Mstate_var-cps expr s return throw class_name instance))
      ((equal? (keyword expr) '=)                   (Mstate_eq-cps expr s return throw class_name instance))
      ((equal? (keyword expr) 'return)              (Mstate_return-cps expr s function_return throw class_name instance))
      ((equal? (keyword expr) 'if)                  (Mstate_if-cps expr s return function_return break continue throw class_name instance))
      ((equal? (keyword expr) 'while)               (Mstate_while-cps expr s return function_return throw class_name instance))
      ((equal? (keyword expr) 'begin)               (Mstate_begin-cps expr s return function_return break continue throw class_name instance))
      ((equal? (keyword expr) 'funcall)             (Mstate_function_call-cps expr s return throw class_name instance))
      ((equal? (keyword expr) 'try)                 (Mstate_try-cps expr s return function_return break continue throw class_name instance))
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
  (lambda (expr s return throw class_name instance)
    (cond
      ((defined_in_layer? (varname expr) (top_layer s)) (error "Redefining variable"))
      ((null? (cddr expr)) (return (set_binding (varname expr) 'null s)))
      (else (Mvalue-cps (initialvalue expr) s (lambda (v) (return 
                                                           (cons (car (set_binding (varname expr) v (top_layer_state s))) (remove_layer s)))) throw class_name instance))
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
  (lambda (expr s return throw class_name instance)
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
    (return (set_field_binding (varname expr) (right_op_val expr s throw class_name instance) class_name instance s))
    )
  )

; Adds the return value into the state under name 'return
(define Mstate_return-cps
  (lambda (expr s function_return throw class_name instance)
    (function_return (set_binding 'return (Mvalue-cps (returnexpr expr) s (lambda (v) v) throw class_name instance) s))
    )
  )

;abstractions for return
(define returnexpr cadr)

; Takes (if (cond) (then-expr) (else-expr))
; First evaluates the boolean condition.  If it's true, return Mstate(then-expr)
; If it's false, return just the state unchanged if no else statement, and
;  Mstate(else-expr) if there is an else statement.
(define Mstate_if-cps
  (lambda (expr s return function_return break continue throw class_name instance)
    (cond
      ((Mboolean-cps (ifcond expr) s (lambda (v) v) throw class_name instance) (Mstate-cps (iftruebody expr) s return function_return break continue throw class_name instance))
      ((null? (ifnoelse expr)) s)
      (else (Mstate-cps (iffalsebody expr) s return function_return break continue throw class_name instance))
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
  (lambda (expr s return function_return break continue throw class_name instance)
    (letrec ((new_state (interpret_parse_tree (begin_body expr) (add_layer s) return function_return (break_layer break) (continue_layer continue) throw class_name instance)))
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
  (lambda (expr s return function_return break continue throw class_name instance)
    (if(hasfinally expr)
       (executefinally (finally_block expr) 
                       (try_catch expr s return
                                  (lambda (v) 
                                    (function_return (executefinally (finally_block expr) v return function_return break continue throw class_name instance)))
                                  (lambda (v) 
                                    (break (executefinally (finally_block expr) v return function_return break continue throw class_name instance)))
                                  (lambda (v) 
                                    (continue (executefinally (finally_block expr) v return function_return break continue throw class_name instance)))
                                  (lambda (v) 
                                    (throw (list (executefinally (finally_block expr) (catchstate v) return function_return break continue throw class_name instance) (exeptionofcatch v)))) 
                                  class_name instance)
                       return function_return break continue throw class_name instance)
       (try_catch expr s return function_return break continue throw class_name instance)
       )

    )
)

;takes care of catch
(define try_catch
  (lambda (expr s return function_return break continue lastThrow class_name instance)
    (call/cc (lambda (throw)
               (if(hascatch expr)
                  (interpret_parse_tree 
                   (trybody expr) s return 
                   function_return
                   break continue (lambda (v) (throw (executecatch (catch_block expr)
                                                                   (set_binding (catchvariable (catch expr)) (Mvalue-cps (catchvalue v) (catchstate v) return lastThrow class_name instance) s)
                                                                   return function_return break continue lastThrow class_name instance))) class_name instance)
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
  (lambda (expr s return function_return break continue throw class_name instance)
    (interpret_parse_tree expr s return function_return break continue throw class_name instance))
  )
  
(define executefinally
  (lambda (expr s return function_return break continue throw class_name instance)
    (interpret_parse_tree expr s return function_return break continue throw class_name instance)
  )
  )
    

;Evaluates a body based on a condition that is true and watches for break
;Takes (< i j) (= i (+ i 1)) s and return
;This part takes care of break and continue statements
(define Mstate_while-cps
  (lambda (expr s return function_return throw class_name instance)
    (call/cc (lambda (break)
               (letrec (
                        (loop (lambda (expr s)
                                (cond
                                  ((Mboolean-cps (whilecond expr) s (lambda (v) v) throw class_name instance)
                                   (loop expr (Mstate-cps (whilebody expr) s return function_return break (continue loop expr return break) throw class_name instance)))
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
  (lambda (expr s return throw class_name instance)
    ;(display "\n\n")
    ;(display "mvalue : ")
    ;(display class_name)
    ;(display ", ")
    ;(display instance)
    ;(display "\n")
    ;(display expr)
    ;(displayy "\n")
    ;(displayy s)
    (cond
      ((number? expr) (return expr))
      ((equal? expr 'true) (return #t))
      ((equal? expr 'false) (return #f))
      ((and (not (list? expr)) (eq? (get_binding expr s) 'error)) (return (get_field_binding expr class_name instance s)))
      ((not (list? expr)) (return (get_binding expr s)))
      ;((not (list? expr)) (return (get_field_binding expr class_name s)))
      ((equal? (operator expr) '+) (return (+ (left_op_val expr s throw class_name instance) (right_op_val expr s throw class_name instance))))
      ((equal? (operator expr) '-)
       (if (null? (cddr expr)) 
           (return (- 0 (left_op_val expr s throw class_name instance)))
           (return (- (left_op_val expr s throw class_name instance) (right_op_val expr s throw class_name instance)))))
      ((equal? (operator expr) '*) (return (* (left_op_val expr s throw class_name instance) (right_op_val expr s throw class_name instance))))
      ((equal? (operator expr) '/) (return (/ (- (left_op_val expr s throw class_name instance) (modulo (left_op_val expr s throw class_name instance) (right_op_val expr s throw class_name instance))) (right_op_val expr s throw class_name instance)))) ; Integer division:  (x - (x % y)) / y
      ((equal? (operator expr) '%) (return (modulo (left_op_val expr s throw class_name instance) (right_op_val expr s throw class_name instance))))
      ((equal? (operator expr) 'funcall) (Mvalue_function_call-cps expr s (lambda (v) (return v)) throw class_name instance))
      ((logical_operator? (operator expr)) (Mboolean-cps expr s (lambda (v) (return v)) throw class_name instance))
      ((equal? (operator expr) 'new) (return (create_instance expr s)))
      ((equal? (operator expr) 'dot) (return (get_field_binding expr (get_field_class expr s class_name instance) (get_field_instance expr s class_name instance) s)))
      (error "Invalid expression for Mvalue")
      )
    ))

(define operator car)

; Returns a tuple of (class_name instance_values)
; Handles (new A)
(define create_instance
  (lambda (expr s)
    (letrec ((class_name (cadr expr)))
    (list class_name (all_initial_instance_values s class_name))
    )))

; Gets a list of instance field values (for this class and all parent classes) in reverse order of how they're defined
(define all_initial_instance_values 
  (lambda (s class_name)
    (cond
      ((eq? class_name 'null) '())
      (else (append (all_initial_instance_values s (parent (get_binding class_name s))) (reverse (initial_instance_values s (top_layer (instance_field_environment (get_binding class_name s)))))))
      )))

; You shouldn't ever need to call this function; it's just a helper to the one above.
; Gets a list of a single classes field values (does not include inherited values). These are not reversed.
; The box/unbox is so that we don't change the default values on when we update a specific value (basically a copy operation)
(define initial_instance_values
  (lambda (s instance_environment)
    (cond
      ((null? instance_environment) '())
      (else (cons (box (unbox (cadr (car instance_environment)))) (initial_instance_values s (cdr instance_environment))))
      )))

; Gets a list of instance field names (for this class and all parent classes) in order of how they're defined
(define all_instance_field_names 
  (lambda (s class_name)
    ;(display "\n\n")
    ;(display "all instance field names: ")
    ;(display class_name)
    ;(display "\n")
    ;(display s)
    (cond
      ((eq? class_name 'null) '())
      (else (append (instance_field_names s (top_layer (instance_field_environment (get_binding class_name s)))) (all_instance_field_names s (parent (get_binding class_name s))) ))
      )))

; You shouldn't ever need to call this function; it's just a helper to the one above.
; Gets a list of a single classes field names (does not include inherited values). These are not reversed.
(define instance_field_names
  (lambda (s instance_environment)
    (cond
      ((null? instance_environment) '())
      (else (cons (car (car instance_environment)) (instance_field_names s (cdr instance_environment))))
      )))

; Returns the value of the boolean expression expr.
; If expr is #t or #f, just return those.
; If it's something else that's not a list, it must be a variable, so return the
;    value of that variable (presumed to be correct type)
; Otherwise, if it's a boolean expression do the corresponding scheme expression
;    with the Mbooleans of the operands (or operand if it's not)
; and if it's an arithmetic comparison, do the corresponding scheme expression
;    with the Mvalues of the operands.
(define Mboolean-cps
  (lambda (expr s return throw class_name instance)
    (cond
      ((boolean? expr) (return expr))
      ((equal? expr 'true) (return #t))
      ((equal? expr 'false) (return #f))
      ((and (not (list? expr)) (eq? (get_binding expr s) 'error)) (return (get_field_binding expr class_name instance s)))
      ((not (list? expr)) (return (get_binding expr s)))
      ((equal? (car expr) '||) (Mboolean-cps (caddr expr) s (lambda (v1) (Mboolean-cps (cadr expr) s (lambda (v2) (return (or v1 v2))) throw class_name instance)) throw class_name instance))
      ((equal? (car expr) '&&) (Mboolean-cps (caddr expr) s (lambda (v1) (Mboolean-cps (cadr expr) s (lambda (v2) (return (and v1 v2))) throw class_name instance)) throw class_name instance))
      ((equal? (car expr) '!=) (return (not (equal? (left_op_val expr s throw class_name instance) (right_op_val expr s throw class_name instance)))))
      ((equal? (car expr) '!) (Mboolean-cps (cadr expr) s (lambda (v) (return (not v))) throw class_name instance))
      ((equal? (car expr) '>) (return (> (left_op_val expr s throw class_name instance) (right_op_val expr s throw class_name instance))))
      ((equal? (car expr) '<) (return (< (left_op_val expr s throw class_name instance) (right_op_val expr s throw class_name instance))))
      ((equal? (car expr) '>=) (return (>= (left_op_val expr s throw class_name instance) (right_op_val expr s throw class_name instance))))
      ((equal? (car expr) '<=) (return (<= (left_op_val expr s throw class_name instance) (right_op_val expr s throw class_name instance))))
      ((equal? (car expr) '==) (return (equal? (left_op_val expr s throw class_name instance) (right_op_val expr s throw class_name instance))))
      ((equal? (car expr) 'funcall) (Mvalue_function_call-cps expr s (lambda (v)
                                                                       (cond
                                                                         ((equal? v 'true) (return #t))
                                                                         ((equal? v 'false) (return #f))
                                                                         (return v))) throw class_name instance))
      )
    ))

(define left_op_val
  (lambda (expr s throw class_name instance)
    ;(display "\n\n")
    ;(display "left op val: ")
    ;(display class_name)
    ;(display "\n")
    ;(display expr)
    ;(display "\n")
    ;(display s)
    ;(display "\n")
    ;(display (Mvalue-cps (cadr expr) s (lambda (v) v) throw class_name))
    (Mvalue-cps (cadr expr) s (lambda (v) v) throw class_name instance)
    ))

(define right_op_val
  (lambda (expr s throw class_name instance)
    (Mvalue-cps (caddr expr) s (lambda (v) v) throw class_name instance)
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


; Class is a 5-tuple: (parent, static_field_environment, static_method_environment, instance_field_environment, instance_method_environment)
(define new_class '( null (()) (()) (()) (()) ))
(define parent car)
(define static_field_environment cadr)
(define static_method_environment caddr)
(define instance_field_environment cadddr)
(define instance_method_environment 
  (lambda (class)
    (car (cddddr class)
  )))

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
        ((equal? 'static-var (car (car body))) (class_body_def (cdr body) (add_to_static_field_environment (car body) s class_name) class_name))       ;add static-field to class's static fields
        ((equal? 'static-function (car (car body))) (class_body_def (cdr body) (add_to_static_method_environment (car body) s class_name) class_name)) ;add static-method to class's static methods
        ((equal? 'var (car (car body))) (class_body_def (cdr body) (add_to_instance_field_environment (car body) s class_name) class_name))            ;add field to class's instance fields
        ((equal? 'function (car (car body))) (class_body_def (cdr body) (add_to_instance_method_environment (car body) s class_name) class_name))      ;add method to class's instance methods
        (else (class_body_def (cdr body) s class_name)))))
        
; Returns the state after adding the variable definition to the class's static fields
; Handles (static-var x) and (static-var x 5)
(define add_to_static_field_environment
    (lambda (expr s class_name)
        (letrec ((class (get_binding class_name s)))
        (cond
          ((null? (cddr expr)) (set_binding class_name  (list (parent class) (set_binding (cadr expr) 'null (static_field_environment class)) (static_method_environment class) (instance_field_environment class) (instance_method_environment class)) s))
          (else (set_binding class_name (list (parent class) (set_binding (cadr expr) (Mvalue-cps (caddr expr) s new_return_continuation throw_error class_name 'null) (static_field_environment class)) (static_method_environment class) (instance_field_environment class) (instance_method_environment class)) s))
          ))))

; Returns the state after adding the new function closure to the class's static methods
; Handles (static-function main () body) and (static-function f ())
(define add_to_static_method_environment
    (lambda (expr s class_name)
        (letrec ((class (get_binding class_name s)))
        (cond
          ((null? (cddr expr)) (set_binding class_name (list (parent class) (static_field_environment class) (set_binding (cadr expr) 'null (static_method_environment class)) (instance_field_environment class) (instance_method_environment class)) s))
          (else (set_binding class_name (list (parent class) (static_field_environment class) (set_binding (cadr expr) (make_closure expr s class_name) (static_method_environment class)) (instance_field_environment class) (instance_method_environment class)) s))
          ))))

; Returns the state after adding the variable definition to the class's instance fields
; Handles (var x) and (var x 5)
(define add_to_instance_field_environment
  (lambda (expr s class_name)
    (letrec ((class (get_binding class_name s)))
    (cond
      ((null? (cddr expr)) (set_binding class_name (list (parent class) (static_field_environment class) (static_method_environment class) (set_binding (cadr expr) 'null (instance_field_environment class)) (instance_method_environment class)) s))
      (else (set_binding class_name (list (parent class) (static_field_environment class) (static_method_environment class) (set_binding (cadr expr) (Mvalue-cps (caddr expr) s new_return_continuation throw_error class_name 'null) (instance_field_environment class)) (instance_method_environment class)) s))
    ))))

; Returns the state after adding the new function closure to the class's instance methods
; Handles (function getX () body) and (function getX ())
(define add_to_instance_method_environment
  (lambda (expr s class_name)
    (letrec ((class (get_binding class_name s)))
      (cond
        ((null? (cddr expr)) (set_binding class_name (list (parent class) (static_field_environment class) (static_method_environment class) (instance_field_environment class) (set_binding (cadr expr) 'null (instance_method_environment class))) s))
        (else (set_binding class_name (list (parent class) (static_field_environment class) (static_method_environment class) (instance_field_environment class) (set_binding (cadr expr) (make_closure expr s class_name) (instance_method_environment class))) s))
      ))))

; Gets i.e. A.x, you would call with (key=x, class_name=A, state=s)
; If the class name exists, search its static_field_environment list for the (tbc)
(define get_field_binding
    (lambda (key class_name instance s)
      ;(display "\n\n")
      ;(display "get_field_binding: ")
      ;(displayy class_name)
      ;(display ", ")
      ;(display instance)
      ;(display "\n")
      ;(display key)
      ;(display "\n")
      ;(display s)
        (cond
            ((and (list? key) (eq? 'dot (car key))) (get_field_binding_dot key class_name instance s))
            (else (get_field_binding_no_dot key class_name instance s))
        )))

; Lookup the value on the RHS from the LHS
(define get_field_binding_dot
  (lambda (key class_name instance s)
    (cond
      ((eq? 'this (cadr key)) (get_instance_value (caddr key) (car instance) instance s))                                                ; (dot this x)
      ;((eq? 'super (cadr key)) (get_field_binding (caddr key) (parent (get_binding class_name s)) instance s))                     ; (dot super x)
      ((eq? 'super (cadr key)) (get_field_binding (caddr key) class_name instance s))                     ; (dot super x)
      ((and (list? (cadr key)) (eq? (caadr key) 'new)) (get_instance_value (caddr key) (create_instance (cadadr key)) s)) ; (dot (new A) x)
      ((is_class? (cadr key) s) (get_field_binding_in_class (caddr key) (cadr key) instance s))                                    ; (dot A x)
      ((is_instance? (cadr key) class_name instance s) (get_instance_value (caddr key) class_name (get_field_binding (cadr key) class_name instance s) s))            ; (dot a x)
      (else (error "Unknown param type to dot function"))                                                                 ; unknown
      )))

; Lookup the value
(define get_field_binding_no_dot
  (lambda (key class_name instance s)
    (cond
      ((defined_in_layer? key (top_layer s)) (get_binding key s))                                                         ; x is a local variable
      ((instance_has_field key instance s) (get_instance_value key class_name instance s))                            ; x is an instance variable
      ((defined? key s) (get_binding key s))                                                                              ; x is a global variable
      (else (get_field_binding_in_class key class_name instance s))                                                       ; x is a class variable (or doesn't exist in this context, which we'll find out here)
    )))

; Gets the value of an instance variable in strategy described by Connamacher
; The field names are stored as a list (a x x y w z) that is the concatenation of my class's instance vars + my parent's + my grandparent's + etc...
; The field values are stored as a list that is reversed wrt the field names
; To get the value of a variable name, we use the "elements after" that key as an index to look up the value from the value list
(define get_instance_value
  (lambda (key runtime_class instance s)
    ;(display "\n\nget_instance_value: ")
    ;(display runtime_class)
    ;(display "\n")
    ;(display instance)
    ;(display "\n")
    ;(display key)
    ;(displayy "\n")
    ;(display s)
    (unbox (list-ref (cadr instance) (elements_after key (all_instance_field_names s runtime_class))))
  ))

(define elements_after
  (lambda (key l)
    (cond
      ((null? l) -1)
      ((eq? key (car l)) (length (cdr l)))
      (else (elements_after key (cdr l))))))
                         

; Checks if a given instance has a given instance variable
(define instance_has_field
  (lambda (key instance s)
    (cond
      ((eq? instance 'null) #f)
      (else (not (eq? (member key (all_instance_field_names s (car instance))) #f)))
    )))

; Checks if a given instance has a given instance method
(define instance_any_has_method
  (lambda (key runtime_class instance s)
    ;(display "\n\ninstance any has method: ")
    ;(display runtime_class)
    ;(display ", ")
    ;(display instance)
    ;(display "\n")
    ;(display key)
    ;(display "\n")
    
    (cond
      ((eq? instance 'null) #f)
      ((eq? runtime_class 'null) #f)
      ((eq? (get_binding key (instance_method_environment (get_binding runtime_class s))) 'error) (instance_has_method key (parent (get_binding runtime_class s)) instance s)) 
      (else #t)
    )))

(define instance_has_method
  (lambda (key runtime_class instance s)
    ;(display "\n\ninstance has method: ")
    ;(display runtime_class)
    ;(display ", ")
    ;(display instance)
    ;(display "\n")
    ;(display key)
    ;(display "\n")
    
    (cond
      ((eq? instance 'null) #f)
      (else (not (eq? (get_binding key (instance_method_environment (get_binding runtime_class s))) 'error)))
    )))
  

; Determines if a variable is a name of a class (based on lookup value in state being a 5-tuple)
(define is_class?
  (lambda (key s)
     (and (defined? key s) (list? (get_binding key s)) (eq? (length (get_binding key s)) 5))
   ))

; Determines if a variable is an instance variable (based on being a 2-tuple)
(define is_instance?
  (lambda (key class_name instance s)
    (and (not (eq? (get_field_binding key class_name instance s) 'error)) (list? (get_field_binding key class_name instance s)) (eq? (length (get_field_binding key class_name instance s)) 2))
  ))

; Lookup a key in a class's static fields
; (get_field_binding_in_class 'x 'A 'null s)
(define get_field_binding_in_class
    (lambda (key class_name instance s)
        (cond
            ((equal? 'error (get_binding key (static_field_environment (get_binding class_name s)))) (get_field_binding key (parent (get_binding class_name s)) instance s))
            (else (get_binding key (static_field_environment (get_binding class_name s))))
        )))

(define get_closure
    (lambda (key class_name instance s)
      ;(display "\n\n")
      ;(display "get_closure: ")
      ;(display class_name)
      ;(display ", ")
      ;(display instance)
      ;(display "\n")
      ;(display key)
      ;(display "\n")
      ;(display s)
        (cond
            ((and (list? key) (eq? 'dot (car key))) (get_closure_dot key class_name instance s))
            (else (get_closure_no_dot key class_name instance s))
        )))

; Lookup the value on the RHS from the LHS
(define get_closure_dot
  (lambda (key class_name instance s)
      ;(display "\n\n")
      ;(display "get_closure dot: ")
      ;(display class_name)
      ;(display ", ")
      ;(display instance)
      ;(display "\n")
      ;(display key)
      ;(display "\n")
      ;(display s)
    (cond
      ((eq? 'this (cadr key)) (get_instance_method (caddr key) (car instance) instance s))                                                ; (dot this x)
      ;((eq? 'super (cadr key)) (get_closure (caddr key) (parent (get_binding class_name s)) s))                            ; (dot super x)
      ((eq? 'super (cadr key)) (get_closure (caddr key) class_name instance s))                            ; (dot super x)
      ((and (list? (cadr key)) (eq? (caadr key) 'new)) (get_instance_method (caddr key) (create_instance (cadadr key)) s)) ; (dot (new A) x)
      ((is_class? (cadr key) s) (get_closure_in_class (caddr key) (cadr key) instance s))                                           ; (dot A x)
      ((is_instance? (cadr key) class_name instance s) (get_instance_method (caddr key) class_name (get_field_binding (cadr key) class_name instance s) s))         ; (dot a x)
      (else (error "Unknown param type to dot function"))                                                                  ; unknown
    )))

; Lookup the value
(define get_closure_no_dot
  (lambda (key class_name instance s)
     ;(display "\n\nget_closure_no_dot: ")
     ;(display class_name)
     ;(display ", ")
     ;(display instance)
     ;(display "\n")
     ;(display key)
     ;(display "\n")
     ;(display s)
     ;(display "\n")
               
    (cond
      ((defined_in_layer? key (top_layer s)) (get_binding key s))                                                         ; x is a local function definition
      ((and (not (eq? instance 'null)) (instance_has_method key class_name instance s)) (get_instance_method key class_name instance s))           ; x is an instance method
      ((and (not (eq? instance 'null)) (instance_has_method key (car instance) instance s)) (get_instance_method key (car instance) instance s))           ; x is an instance method
      ((defined? key s) (get_binding key s))                                                                              ; x is a global function defintion
      ((equal? 'error (get_binding class_name s)) 'error)                                                                 ; allow returning error
      (else (get_closure_in_class key class_name instance s))                                                             ; x is a static method (or doesn't exist in this context, which we'll find out here)
    )))

; Looks up the instance method in the instance's class definition
(define get_instance_method
  (lambda (key runtime_class instance s)
    ;(display "\n\nget_instance_method: ")
    ;(display runtime_class)
    ;(display ", ")
    ;(display instance)
    ;(display "\n")
    ;(display key)
    ;(display "\n")
    ;(display s)
    (cond 
      ((eq? 'error (get_binding key (instance_method_environment (get_binding runtime_class s)))) (get_instance_method key (parent (get_binding runtime_class s)) instance s))
      (else (get_binding key (instance_method_environment (get_binding runtime_class s))))
  )))

; Lookup a function closure in a class's static methods
(define get_closure_in_class
    (lambda (key class_name instance s)
      ;(display "\n\n")
      ;(display "get_closure_in_class: ")
      ;(display class_name)
      ;(display "\n")
      ;(display key)
      ;(display "\n")
      ;(display s)
      ;(display "\n")
        (cond
            ((equal? 'error (get_binding key (static_method_environment (get_binding class_name s)))) (get_closure key (parent (get_binding class_name s)) instance s))
            (else (get_binding key (static_method_environment (get_binding class_name s))))
        )))

; Updates a variable's binding in a class, instance, locally, or globally
(define set_field_binding
  (lambda (key val class_name instance s)
    (cond
      ((and (list? key) (eq? 'dot (car key))) (set_field_binding_dot key val class_name instance s))
      (else (set_field_binding_no_dot key val class_name instance s))
    )))

; Updates a variable's binding in a dot expression
(define set_field_binding_dot
  (lambda (key val class_name instance s)
    (cond
      ((eq? 'this (cadr key)) (set_instance_value (caddr key) val instance s))                                                                      ; (dot this x)
      ;((eq? 'super (cadr key)) (set_field_binding (caddr key) val (parent (get_binding class_name s)) s))                                           ; (dot super x)
      ((eq? 'super (cadr key)) (set_field_binding (caddr key) val class_name instance s))                                           ; (dot super x)
      ((and (list? (cadr key)) (eq? (caadr key) 'new)) (set_instance_value (caddr key) val (create_instance (cadadr key)) s))                       ; (dot (new A) x)
      ((is_class? (cadr key) s) (set_field_binding_in_class (caddr key) val (cadr key) instance s))                                                 ; (dot A x)
      ((is_instance? (cadr key) class_name instance s) (set_instance_value (caddr key) val (get_field_binding (cadr key) class_name instance s) s)) ; (dot a x)
      (else (error "Unknown param type to dot function"))  
    )))

; Updates a variable's binding when not dot expression is given
(define set_field_binding_no_dot
  (lambda (key val class_name instance s)
    (letrec ((class (get_binding class_name s)))
    (cond
      ((defined_in_layer? key (top_layer s)) (set_binding key val s))
      ((defined? key s) (update_binding key val s))
      ((defined? key (static_field_environment class)) (set_binding class_name (list (parent class) (update_binding key val (static_field_environment class)) (static_method_environment class) (instance_field_environment class) (instance_method_environment class)) s))
      ((instance_has_field key instance s) (set_instance_value key val instance s))
      ((equal? 'null class_name) (set_binding key val s))
      (else (set_field_binding_in_class key val class_name instance s))      
    ))))

; Updates a classes static field (or calls on parent)
(define set_field_binding_in_class
  (lambda (key val class_name instance s)
    (cond
      ((equal? 'error (get_binding key (static_field_environment (get_binding class_name s)))) (set_field_binding key val (parent (get_binding class_name s)) instance s)) ; call recursively on parent.
      (else (begin (set_binding key val (static_field_environment (get_binding class_name s))) s))  ; This should update the class definition, i.e. for a static variable.  A.x=5
    )))

; Sets the value of an instance variable in strategy described by Connamacher
; The field names are stored as a list (a x x y w z) that is the concatenation of my class's instance vars + my parent's + my grandparent's + etc...
; The field values are stored as a list that is reversed wrt the field names
; To find which value to set, we use the "elements after" that key as an index to look up the value from the value list
(define set_instance_value
  (lambda (key val instance s)
    ;(display "\n\nset_instance_value: ")
    ;(display instance)
    ;(display "\n")
    ;(display key)
    ;(display "\n")
    ;(display val)
    ;(display "\n")
    ;(display s)
    (begin
      (list-set! (cadr instance) (elements_after key (all_instance_field_names s (car instance))) val)
      s
    )
  ))

; Sets the val of the element at the given index using set-box!
(define (list-set! l k val)
    (if (zero? k)
        (set-box! (car l) val)
        (list-set! (cdr l) (- k 1) val)))

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
      ((equal? 'error (get_binding key (static_method_environment class))) (set_closure_binding key val (parent class) s)) ; call recursively on parent.
      (else (set_binding key val (static_method_environment class)))  ; This should update the class definition, i.e. for a static variable.  A.x=5
    )
  )
)

;(parser "tests5/15")
;(display "\n")
;(initial_environment (parser "tests5/15") 'List)
;(display "\n")
;(interpretClass "tests5/15" 'List)
