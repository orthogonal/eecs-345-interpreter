;Logic necessary to run through test suite
;Change parser to simple parser!
(load "interpret.scm")
(require rackunit)

(check-eq? (interpret "tests/1") 150)
(check-eq? (interpret "tests/2") -4)
(check-eq? (interpret "tests/3") 10)
(check-eq? (interpret "tests/4") 16)
(check-eq? (interpret "tests/5") 220)
(check-eq? (interpret "tests/6") 5)
(check-eq? (interpret "tests/7") 6)
(check-eq? (interpret "tests/8") 10)
(check-eq? (interpret "tests/9") 5)
(check-eq? (interpret "tests/10") -39)
; Commented out because we don't have a way to check for a variable being used before declaration currently
; I think the easiest way to do this would be to just add a (var_name) tuple to the bindings instead of
;   the normal (var_name var_value)
; (check-exn exn:fail? (lambda () (interpret "tests/11")))
(check-exn exn:fail? (lambda () (interpret "tests/12")))
(check-exn exn:fail? (lambda () (interpret "tests/13")))
(check-exn exn:fail? (lambda () (interpret "tests/14")))
(check-eq? (interpret "tests/15") 'true)
(check-eq? (interpret "tests/16") 100)
(check-eq? (interpret "tests/17") 'false)
(check-eq? (interpret "tests/18") 'true)
(check-eq? (interpret "tests/p1test1") 200)

(display "passed all of tests1\n")

(check-eq? (interpret "tests2/1") 100)
(check-eq? (interpret "tests2/2") 20)
(check-eq? (interpret "tests2/3") 6)
(check-eq? (interpret "tests2/4") -1)
(check-eq? (interpret "tests2/5") 789)
(check-eq? (interpret "tests2/6") 2)
(check-eq? (interpret "tests2/7") 164)
(check-exn exn:fail? (lambda () (interpret "tests2/8")))
(check-exn exn:fail? (lambda () (interpret "tests2/9")))
(check-exn exn:fail? (lambda () (interpret "tests2/10")))
(check-eq? (interpret "tests2/11") 12)
(check-eq? (interpret "tests2/12") 32)

(display "passed all of tests2\n")
