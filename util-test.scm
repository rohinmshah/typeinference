(load "util.scm")
(load "test-framework.scm")

(display "Testing util functions\n")

(quick-test 'last-pair
	    equal?
	    '(  (3)  (1 2 3)  )
	    '(  (hello)  (hello)  )
	    '(  (((4)))  ((1) (((2 3) 5) 2) ((4)))  ))

(quick-test 'last-element
	    equal?
	    '(  3  (1 2 3)  )
	    '(  hello  (hello)  )
	    '(  ((4))  ((1) (((2 3) 5) 2) ((4)))  ))

(quick-test 'without-last-element
	    equal?
	    '(  (1 2)  (1 2 3)  )
	    '(  ()  (hello)  )
	    '(  ((1) (((2 3) 5)) 2)  ((1) (((2 3) 5)) 2 ((4)))  ))

(quick-test 'sublist
	    equal?
	    '(  (9 0 2 1 0)  (1 8 2 3 9 0 2 1 0 8 5 7) 4 9  )
	    '(  ()  (4 2 9 1) 0 0  )
	    '(  ()  (4 2 9 1) 2 2  )
	    '(  (2)  (4 2 9 1) 1 2  )
	    '(  ((3 4) (5 6))  ((1 2) (3 4) (5 6) (7 8)) 1 3  )
	    '(  (4 5)  (1 2 3 4 5) 3 10  ))

(define (halves n) (list (/ n 2) (/ n 2)))

(quick-test 'flatmap
	    equal?
	    `(  ()  ,+ ())
	    `(  (1 1 2 2 3 3)  ,halves (2 4 6)  )
	    `(  (9 25 16 64)
		,(lambda (x) (map (lambda (y) (* y y)) x))
		((3) (5 4 8))  ) )

(quick-test 'indexq
	    equal?
	    '(0  the (the rain in spain falls mainly on the plain)  )
	    '(#f  pain (the rain in spain falls mainly on the plain)  )
	    '(3  spain (the rain in spain falls mainly on the plain)  )
	    `(#f  ,(cons 1 2) ,(list (cons 1 2) (cons 1 2))  ) )

; For deepcopy, we need a recursive equality checker that can deal
; with cycles, that checks that the two items are equal? but not eq?.
; It does the normal equal? type recursion on the car and the cdr, but
; it also keeps track of every pair seen.  It avoids cycles by
; checking if the pair has already been seen.  If it has been seen,
; then it must have been seen in both pairs at the same time.
(define (equal-not-eq? x y)
  (let ((a-pairs '())
	(b-pairs '()))
    (define (helper a b)
      (cond ((not (pair? a))
	     (equal? a b))
	    ((indexq a a-pairs) =>
	     (lambda (index) (eq? (list-ref b-pairs index) b)))
	    ((indexq b b-pairs) #f)
	    (else (set! a-pairs (cons a a-pairs))
		  (set! b-pairs (cons b b-pairs))
		  (and (pair? b)
		       (not (eq? a b))
		       (helper (car a) (car b))
		       (helper (cdr a) (cdr b))))))
    (helper x y)))

(define deep-copy-tests (list (list 1 2 3) (cons 7 9) (list 1 2)))

; The first test will have a shared pair
(set-car! (cdar deep-copy-tests) (cddar deep-copy-tests))
; The last test will have a cycle
(set-cdr! (cdaddr deep-copy-tests) (caddr deep-copy-tests))

(quick-test 'deep-copy
	    equal-not-eq?
	    `(  ,(car deep-copy-tests) ,(car deep-copy-tests)  )
	    `(  ,(cadr deep-copy-tests) ,(cadr deep-copy-tests)  )
	    `(  ,(caddr deep-copy-tests) ,(caddr deep-copy-tests)  ) )

(let ((x (deep-copy (car deep-copy-tests))))
  (set-car! (cadr x) 4)
  (if (not (= (caddr x) 4))
      (error "Test failed:  Deepcopied list does not have shared pair")))

(define (set-equal? a b)
  (define (helper x)
    (or (null? x)
	(and (member (car x) b)
	     (helper (cdr x)))))
  (and (= (length a) (length b))
       (helper a)))

(quick-test 'merge
	    set-equal?
	    '(  (1 2 3 4 5 6) (1 4 2 3) (2 4 1 6 5)  )
	    '(  (1 2 4 5 (4 . 5)) (1 2 4 5) (1 2 (4 . 5))  )
	    '(  (1 2) (1 2) ()  )
	    '(  (3) () (3)  ) )

(quick-test 'remove-all
	    equal?
	    '(  (4 5 6 8 9) (1 2 3 7 10) (1 2 3 4 5 6 7 8 9)  )
	    '(  () (1 2 3) (1 2)  )
	    '(  (1 (2)) (2 3 4) (1 (2))  )
	    '(  (1 1 3 3 5) (2 4) (1 1 2 2 3 3 4 4 5 2 4)  ) )

(quick-test 'hof-member
	    equal?
	    `(  ((4 5) 6)
		,equal? ,(list 4 5) ,(list 1 (list 2 3) (list 4 5) 6)  )
	    `(  #f
		,eq? ,(list 4 5) ,(list 1 (list 2 3) (list 4 5) 6)  )
	    `(  (4 3 2 1)
		,< 3 (1 2 3 4 3 2 1)  ) )

(quick-test 'make-list
	    equal?
	    '(  ()  3 0  )
	    '(  (2 2 2 2)  2 4  )
	    '(  ((1 2) (1 2))  (1 2) 2  ) )

(end-of-tests)
