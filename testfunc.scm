;Logic necessary to run through test suite
(load "interpret.scm")
(load "functionParser.scm")
(require rackunit)

(check-eq? (interpretFunction "tests3/1") 10)
(check-eq? (interpretFunction "tests3/2") 14)
(check-eq? (interpretFunction "tests3/3") 45)
(check-eq? (interpretFunction "tests3/4") 55)
(check-eq? (interpretFunction "tests3/5") 1)
(check-eq? (interpretFunction "tests3/6") 115)
(check-eq? (interpretFunction "tests3/7") 'true)
(check-eq? (interpretFunction "tests3/8") 20)
(check-eq? (interpretFunction "tests3/9") 24)
(check-eq? (interpretFunction "tests3/10") 2)
(check-eq? (interpretFunction "tests3/11") 35)
(check-exn exn:fail? (lambda () (interpretFunction "tests3/12")))
(check-eq? (interpretFunction "tests3/13") 90)
(check-eq? (interpretFunction "tests3/14") 69)
(check-eq? (interpretFunction "tests3/15") 87)
(check-eq? (interpretFunction "tests3/16") 64)
(check-exn exn:fail? (lambda () (interpretFunction "tests3/17")))

(check-eq? (interpretFunction "tests3/tc1") 9)
(check-eq? (interpretFunction "tests3/tc2") 7)
(check-eq? (interpretFunction "tests3/tc3") 9)
(check-eq? (interpretFunction "tests3/tc4") 9)
(check-eq? (interpretFunction "tests3/tc5") 9)

(display "passed all of tests3\n")