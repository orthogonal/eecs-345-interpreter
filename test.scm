;Logic necessary to run through test suite
(require rackunit)
(load "interpret.scm")

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
 