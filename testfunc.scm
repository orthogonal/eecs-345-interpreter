;Logic necessary to run through test suite
(load "interpret.scm")
(require rackunit)

(check-eq? (interpret "tests3/1") 10)
(check-eq? (interpret "tests3/2") 14)
(check-eq? (interpret "tests3/3") 45)
(check-eq? (interpret "tests3/4") 55)
(check-eq? (interpret "tests3/5") 1)
(check-eq? (interpret "tests3/6") 115)
(check-eq? (interpret "tests3/7") 'true)
(check-eq? (interpret "tests3/8") 20)
(check-eq? (interpret "tests3/9") 24)
(check-eq? (interpret "tests3/10") 2)
(check-eq? (interpret "tests3/11") 35)
(check-exn exn:fail? (lambda () (interpret "tests3/12")))
(check-eq? (interpret "tests3/13") 90)
(check-eq? (interpret "tests3/14") 69)
(check-eq? (interpret "tests3/15") 87)
(check-eq? (interpret "tests3/16") 64)
(check-exn exn:fail? (lambda () (interpret "tests3/17")))