(load "inference.scm")
(load "test-framework.scm")

(display "Testing type inference\n")

(define num make-number-type)
(define string make-string-type)
(define symbol make-symbol-type)
(define bool make-boolean-type)
(define null make-null-type)
(define wd make-word-type)
(define any make-any-type)
(define undef make-undefined-type)
(define pair make-pair-type)
(define union compute-union)
(define func make-function-type)
(define varfunc make-variable-function-type)

(define (inference-test code actual)
  (let ((inferred (typeof code)))
    ; Ideally, actual type should be the same as inferred type
    (if (not (equal-types? actual inferred #f))
	; Actual type must always be a subtype of the inferred type
	(let ((check (check-conversion actual inferred)))
	  (if (not (equal? check 'yes))
	      (begin (set! failed? #t)
		     (display "\nTest failed: "))
	      (display "\nInferred type too general: "))
	  (display code)
	  (display "\nActual type: ")
	  (display-type (raw-type actual))
	  (display "\nInferred type: ")
	  (display-type (raw-type inferred))
	  (newline)))))

(define (inference-error-test code str)
  (let ((port (open-output-string)))
    (if (with-output-to-port port
	  (lambda () (not (catch (typeof code)))))
	(begin (set! failed? #t)
	       (display "\nTest failed: ")
	       (display code)
	       (display "\nShould have thrown an error\n\n")))
    (if (not (string-index str (get-output-string port)))
	(begin (set! failed? #t)
	       (display "\nTest failed: ")
	       (display code)
	       (display "\nThrew the wrong kind of error - expected ")
	       (display str)
	       (display "\n\n")))
    (close-port port)))

; Self-evaluating
(inference-test 3 (num))
(inference-test #t (bool))
(inference-test "hello" (string))

; Variable lookup
(inference-test 'remainder (func (num) (num) (num)))

; Quotation
(inference-test ''(1 . hello)
	  (pair (num) (symbol)))

(inference-test ''() (null))

(inference-test ''(1 #f "string")
		(pair (num)
		      (pair (bool)
			    (pair (string)
				  (null)))))

; Define, let, lambda, begin
(inference-test '(begin (define (square x) (* x x))
			(define foo ((lambda (x) x) 4))
			(define bar (let ((x 4)) x)))
		(undef))

(inference-test 'square (func (num) (num)))
(inference-test 'foo (num))
(inference-test 'bar (num))

; Things that can be evaluated directly (and so, type inference is too general)

; 1. Lots of function applications
(inference-test '(word (bf 'hello) (first 'hello) 'ay)
	  (symbol))

; 2. Word -> number casting
(inference-test '(first (word 34 'hello))
	  (num))

; Errors
(define type_err "Type Error")
(define arity_err "Incorrect Number of Arguments Error")
(define func_err "Bad Function Error")

(inference-error-test '(+ 'hi 3) type_err)
(inference-error-test '(remainder 5 4 3) arity_err)
(inference-error-test '(1 2 3) func_err)
(inference-error-test '(define (sum-squares x y)
			 (+ square x square y))
		      type_err)
(inference-error-test '(define (sum-squares x y)
			 ((square x) + (square y)))
		      func_err)

; Variable number of arguments
(inference-test '(+) (num))
(inference-error-test '(-) arity_err)
(inference-test '(*) (num))
(inference-error-test '(/) arity_err)

(inference-test '(word) (string))

(inference-error-test '(<) arity_err)
(inference-error-test '(>=) arity_err)
(inference-test '(= 2) (bool))

(inference-test '(< 1 2 3 4)
		(bool))

(inference-error-test '(< 1 2 3 "string")
		      type_err)

(inference-test '(if (= 3 5) 3 5)
		(num))

(inference-test '(if (= 3 5) 'hello 5)
		(num))

(inference-error-test '(if (set! + *) 2 3)
		      type_err)

(inference-test '(lambda (n)
		   (cond ((and (= (remainder n 3) 0)
			       (= (remainder n 5) 0))
			  'fizzbuzz)
			 ((= (remainder n 3) 0)
			  'fizz)
			 ((= (remainder n 5) 0)
			  'buzz)
			 (else n)))
		(func
		 (num)
		 (union (num) (symbol))))

(end-of-tests)
