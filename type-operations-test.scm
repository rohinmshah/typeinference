(load "inference.scm")
(load "test-framework.scm")

(display "Testing type operations\n")

(define num make-number-type)
(define str make-string-type)
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

(general-test "Compute union doesn't get rid of unnecessary types"
	      '(union-types (union (bool) (num) (wd)))
	      (list (bool) (wd))
	      (list (wd) (bool)))

; Tests of is-base-subtype?

; Use double negation to force value to be a boolean
(define (basic-subtype-test arg1 arg2 boolean)
  (general-test "Basic subtyping using is-base-subtype?"
		`(not (not (is-base-subtype? (type-tag (raw-type ,arg1))
					     (type-tag (raw-type ,arg2)))))
		boolean))

(basic-subtype-test '(num) '(num) #f)
(basic-subtype-test '(num) '(wd) #t)
(basic-subtype-test '(wd) '(num) #f)
(basic-subtype-test '(symbol) '(wd) #t)
(basic-subtype-test '(wd) '(str) #f)
(basic-subtype-test '(bool) '(wd) #f)
(basic-subtype-test '(null) '(any) #t)
(basic-subtype-test '(pair (bool) (num)) '(any) #t)
(basic-subtype-test '(undef) '(any) #f)
(basic-subtype-test '(pair (bool) (num)) '(bool) #f)
(basic-subtype-test '(num) '(pair (bool) (num)) #f)

; Subtype tests (i.e. check-conversion)

; For each subtype test, if the answer is yes or maybe, then reversing
; the arguments should give either yes or maybe
; If it is no, the reverse order should also be no
(define (subtype-test t1 t2 ans)
  (general-test  "Incorrect subtyping" `(check-conversion ,t1 ,t2) ans)
  (cond ((equal? ans 'no)
	 (general-test "Incorrect subtyping"
		       `(check-conversion ,t2 ,t1) 'no))
	((or (equal? ans 'yes) (equal? ans 'maybe))
	 (general-test "Incorrect subtyping"
		       `(check-conversion ,t2 ,t1) 'yes 'maybe))
	(else (error "Expected yes/maybe/no as the answer, got" ans))))

; Tests for any type
(subtype-test '(num)
	      '(any)
	      'yes)

(subtype-test '(undef)
	      '(any)
	      'no)

(subtype-test '(any)
	      '(num)
	      'maybe)

(subtype-test '(typeof '+)
	      '(any)
	      'yes)

(subtype-test '(union (num) (bool))
	      '(any)
	      'yes)

; Tests for basic subtyping (based on subtyping-tree)
(subtype-test '(wd)
	      '(bool)
	      'no)

(subtype-test '(null)
	      '(symbol)
	      'no)

(subtype-test '(symbol)
	      '(wd)
	      'yes)

(subtype-test '(wd)
	      '(str)
	      'maybe)

(subtype-test '(num)
	      '(wd)
	      'yes)

; Tests for compound types

; Pairs
(subtype-test '(pair (num) (wd))
	      '(pair (num) (num))
	      'maybe)

(subtype-test '(pair (wd) (num))
	      '(pair (bool) (num))
	      'no)

(subtype-test '(pair (bool) (symbol))
	      '(pair (any) (wd))
	      'yes)

(subtype-test '(pair (num) (num))
	      '(func (num) (num))
	      'no)

(subtype-test '(num)
	      '(pair (num) (num))
	      'no)

; Unions
(subtype-test '(union (num) (bool))
	      '(wd)
	      'maybe)

(subtype-test '(union (num) (symbol))
	      '(wd)
	      'yes)

(subtype-test '(wd)
	      '(union (num) (symbol))
	      'maybe)

(subtype-test '(union (num) (bool))
	      '(union (num) (bool) (symbol))
	      'yes)

(subtype-test '(union (num) (bool) (symbol))
	      '(union (num) (bool))
	      'maybe)

(subtype-test '(union (num) (bool))
	      '(union (wd) (bool) (str))
	      'yes)

(subtype-test '(union (any) (num))
	      '(union (bool) (null))
	      'maybe)

(subtype-test '(union (wd) (bool))
	      '(null)
	      'no)

(subtype-test '(union (num) (str))
	      '(union (null) (symbol))
	      'no)

; Normal Functions

(subtype-test '(func (num))
	      '(func (wd))
	      'yes)

(subtype-test '(func (wd))
	      '(func (symbol))
	      'maybe)

(subtype-test '(func (wd))
	      '(func (bool))
	      'no)

(subtype-test '(func (num) (num) (num))
	      '(func (num) (num))
	      'no)

(subtype-test '(func (wd) (any) (num))
	      '(func (num) (symbol) (wd))
	      'yes)

(subtype-test '(func (wd) (any) (num))
	      '(func (any) (any) (wd))
	      'maybe)

(subtype-test '(func (union (num) (symbol)) (num))
	      '(func (symbol) (num))
	      'yes)

(subtype-test '(func (pair (num) (num)) (num))
	      '(func (str) (num))
	      'no)

; Variable number of arguments

(subtype-test '(typeof '+)
	      '(typeof '-)
	      'yes)

(subtype-test '(typeof '-)
	      '(typeof '+)
	      'maybe)

(subtype-test '(typeof '+)
	      '(typeof 'remainder)
	      'yes)

(subtype-test '(typeof 'remainder)
	      '(typeof '+)
	      'maybe)

(subtype-test '(varfunc (null) (pair (num) (bool)) (str) (wd))
	      '(varfunc (null) (pair (num) (bool)) (wd) (wd))
	      'maybe)

(subtype-test '(varfunc (null) (pair (num) (bool)) (wd) (wd))
	      '(varfunc (null) (pair (num) (bool)) (str) (wd))
	      'yes)

(subtype-test '(varfunc (null) (pair (num) (bool)) (wd) (wd))
	      '(varfunc (null) (pair (num) (bool)) (wd) (symbol))
	      'maybe)

(subtype-test '(varfunc (null) (pair (num) (bool)) (wd) (symbol))
	      '(varfunc (null) (pair (num) (bool)) (wd) (wd))
	      'yes)

(subtype-test '(varfunc (null) (pair (num) (bool)) (wd) (wd))
	      '(varfunc (null) (union (num) (bool)) (wd) (wd))
	      'no)

(subtype-test '(varfunc (null) (wd) (wd))
	      '(varfunc (null) (wd) (null) (null) (wd))
	      'no)

(subtype-test '(varfunc (null) (wd) (wd))
	      '(varfunc (null) (wd) (str) (num) (wd))
	      'yes)

(subtype-test '(varfunc (null) (wd) (wd))
	      '(varfunc (null) (wd) (str) (any) (wd))
	      'maybe)

(subtype-test '(varfunc (null) (wd) (wd))
	      '(varfunc (null) (wd) (null) (any) (wd))
	      'no)

(subtype-test '(varfunc (null) (wd) (wd) (symbol))
	      '(varfunc (null) (wd) (symbol))
	      'maybe)

(subtype-test '(varfunc (null) (wd) (wd) (bool) (symbol))
	      '(varfunc (null) (wd) (symbol))
	      'maybe)

; Mix of normal and variable functions
; Checking if a varfunc is a subtype of a normal func.

(subtype-test '(varfunc (num) (num) (num) (num))
	      '(func (num) (num))
	      'no)

(subtype-test '(varfunc (num) (bool) (num))
	      '(func (num) (num))
	      'yes)

(subtype-test '(varfunc (num) (bool) (num))
	      '(func (num) (bool) (num))
	      'yes)

(subtype-test '(varfunc (num) (bool) (null))
	      '(func (num) (bool) (bool) (bool) (null))
	      'yes)

(subtype-test '(varfunc (num) (bool) (null))
	      '(func (num) (num) (num) (bool) (null))
	      'no)

(subtype-test '(varfunc (wd) (any) (num))
	      '(func (num) (bool) (bool) (wd))
	      'yes)

(subtype-test '(varfunc (num) (any) (num))
	      '(func (wd) (bool) (bool) (wd))
	      'maybe)

(subtype-test '(varfunc (wd) (any) (wd))
	      '(func (num) (bool) (bool) (num))
	      'maybe)

; Checking if a normal func is a subtype of a varfunc.
; Reversing the tests above are mostly sufficient.

(subtype-test '(func (num) (num) (num))
	      '(varfunc (num) (num) (num))
	      'maybe)

(subtype-test '(func (num) (num))
	      '(varfunc (num) (num) (num))
	      'maybe)

(subtype-test '(func (num) (num))
	      '(varfunc (num) (num) (num) (num))
	      'no)

; TODO: Right now, the assumption is that if (check-conversion t1 t2) returns
; 'maybe, then intersect must return a non-false answer.  Is this true?
(define (intersection-test t1 t2 ans)
  (if ans
      (begin
	(general-test  "Incorrect intersection"
		       `(equal-types? (intersect ,t1 ,t2) ,ans #f)
		       #t)
	(general-test  "Incorrect intersection"
		       `(equal-types? (intersect ,t2 ,t1) ,ans #f)
		       #t)
	(subtype-test ans t1 'yes)
	(subtype-test ans t2 'yes))
      (begin
	(general-test "Incorrect intersection"
		      `(intersect ,t1 ,t2)
		      #f)
	(general-test "Incorrect intersection"
		      `(intersect ,t2 ,t1)
		      #f)
	(subtype-test t1 t2 'no))))

; Basic tests

(intersection-test '(wd)
		   '(num)
		   '(num))

(intersection-test '(str)
		   '(wd)
		   '(str))

(intersection-test '(bool)
		   '(any)
		   '(bool))

(intersection-test '(null)
		   '(wd)
		   #f)

; Unions

(intersection-test '(union (undef) (num) (bool))
		   '(bool)
		   '(bool))

(intersection-test '(union (num) (str) (bool))
		   '(wd)
		   '(union (num) (str)))

(intersection-test '(union (null) (symbol))
		   '(union (wd) (bool))
		   '(symbol))

; Pairs (for now only covariance)

; TODO: Co/contravariance

(intersection-test '(pair (num) (any))
		   '(pair (wd) (bool))
		   '(pair (num) (bool)))

(intersection-test '(pair (num) (null))
		   '(pair (wd) (bool))
		   #f)

(intersection-test '(union (pair (symbol) (any))
			   (pair (str) (bool)))
		   '(pair (wd) (num))
		   '(pair (symbol) (num)))

(intersection-test '(pair (num) (null))
		   '(union (num) (null))
		   #f)

; Function types
; Normal function types

(intersection-test '(func (num) (num))
		   '(func (wd) (wd))
		   '(func (wd) (num)))

(intersection-test '(func (bool) (any) (wd))
		   '(func (str) (null) (symbol))
		   '(func (union (bool) (str)) (any) (symbol)))

(intersection-test '(func (bool) (num) (num))
		   '(func (bool) (num) (num) (num) (num))
		   '(varfunc (bool) (num) (num) (num)))

(end-of-tests)
