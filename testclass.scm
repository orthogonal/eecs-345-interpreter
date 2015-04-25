;Logic necessary to run through test suite
(load "interpret.scm")
(load "classParser.scm")
(require rackunit)

(check-eq? (interpretClass "tests4/1" 'A) 10)
(check-eq? (interpretClass "tests4/2" 'A) 'true)
(check-eq? (interpretClass "tests4/3" 'A) 30)
(check-eq? (interpretClass "tests4/4" 'A) 'false)
(check-eq? (interpretClass "tests4/5" 'A) 30)
(check-eq? (interpretClass "tests4/5" 'B) 510)
(check-eq? (interpretClass "tests4/6" 'A) 30)
(check-eq? (interpretClass "tests4/6" 'B) 530)
(check-eq? (interpretClass "tests4/7" 'A) 105)
(check-eq? (interpretClass "tests4/7" 'B) 1155)
(check-eq? (interpretClass "tests4/8" 'B) 615)
(check-exn exn:fail? (lambda () (interpretClass "tests4/9" 'B)))
(check-eq? (interpretClass "tests4/9" 'C) 4321)
(check-eq? (interpretClass "tests4/10" 'Square) 400)
(check-eq? (interpretClass "tests4/11" 'A) 15)
(check-eq? (interpretClass "tests4/12" 'A) 125)
(check-eq? (interpretClass "tests4/13" 'A) 100)
(check-eq? (interpretClass "tests4/14" 'A) 2000400)
(check-eq? (interpretClass "tests4/15" 'Pow) 64)

(display "passed all of tests4\n")

(check-eq? (interpretClass "tests5/1" 'A) 20)
(check-eq? (interpretClass "tests5/2" 'Square) 400)
(check-eq? (interpretClass "tests5/3" 'B) 530)
(check-eq? (interpretClass "tests5/4" 'B) 615)
(check-eq? (interpretClass "tests5/5" 'C) -716)
(check-eq? (interpretClass "tests5/6" 'A) 15)
(check-eq? (interpretClass "tests5/7" 'A) 12)
(check-eq? (interpretClass "tests5/8" 'A) 110)
(check-eq? (interpretClass "tests5/9" 'A) 125)
(check-eq? (interpretClass "tests5/10" 'A) 36)
(check-eq? (interpretClass "tests5/11" 'A) 54)
(check-eq? (interpretClass "tests5/12" 'C) 26)
(check-eq? (interpretClass "tests5/13" 'Square) 117)
(check-eq? (interpretClass "tests5/14" 'Square) 32)
;(check-eq? (interpretClass "tests5/15" 'List) 15)
(check-eq? (interpretClass "tests5/16" 'Box) 16)
;(check-eq? (interpretClass "tests5/17" 'List) 123456)
;(check-eq? (interpretClass "tests5/18" 'List) 5285)
(check-eq? (interpretClass "tests5/19" 'A) 100)
(check-eq? (interpretClass "tests5/20" 'A) 420)
(check-eq? (interpretClass "tests5/21" 'A) 10)
(check-exn exn:fail? (lambda () (interpretClass "tests4/22" 'A)))

(display "passed all of tests5\n")