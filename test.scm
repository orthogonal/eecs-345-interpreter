;Logic necessary to run through test suite
(load "interpret.scm")

(define eq_test
    (lambda (test_n val)
        (display test_n)
        (display ": ")
        (display (= (interpret test_n) val))
        (display "\n")
    )
)

(eq_test "tests/1" 150)
(eq_test "tests/2" -4)
(eq_test "tests/3" 10)
(eq_test "tests/4" 16)
(eq_test "tests/5" 220)
(eq_test "tests/6" 5)
(eq_test "tests/7" 6)
(eq_test "tests/8" 10)
(eq_test "tests/9" 5)
(eq_test "tests/10" -39)

#t