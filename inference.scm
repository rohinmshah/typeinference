; Type inference for Scheme.

(load "util.scm")
(load "mce-stuff.scm")
(load "types.scm")
(load "type-operations.scm")

(define (typeof exp)
  (type-check exp the-global-environment))

; TODO: Handle more primitives, recursive types, polymorphism,
; simultaneous types. 
; TODO: Once we have simultaneous types, handle "123", which is both a
; number and a string. 
; TODO: Allow function types to have a lambda as the range type.
; Invoking the lambda with the types of subexpressions should give the
; range.  Would be useful for apply, for example.

; Type checks an expression in the given typing environment.
; Returns a tagged type (i.e. it is determined/undetermined)
; Typing environment is a mapping from variables to tagged types.
(define (type-check exp env)
  (cond ((null? exp) (make-determined-type (make-null-type)))
	((number? exp) (make-determined-type (make-number-type)))
	((boolean? exp) (make-determined-type (make-boolean-type)))
	((string? exp) (make-determined-type (make-string-type)))
	;((vector? exp) (make-determined-type (make-vector-type)))
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (quoted-type-check (text-of-quotation exp)))
	((assignment? exp) (type-check-assignment exp env))
	((definition? exp)
	 (type-check-define (desugar-define exp) env))
	((if? exp) (type-check-if exp env))
	((lambda? exp) (type-check-lambda exp env))
	((let? exp) (type-check-let exp env))
	((begin? exp) (type-check-sequence (begin-actions exp) env))
	((cond? exp) (type-check (cond->if exp) env))
	((application? exp)
	 (type-check-application (type-check (operator exp) env)
				 (map (lambda (x) (type-check x env))
				      (operands exp))
				 exp
				 env))
	(else (error "Error: Unknown expression type --- " exp))))

; Returns the type of exp, assuming it was quoted
(define (quoted-type-check exp)
  (cond ((null? exp) (make-determined-type (make-null-type)))
	((number? exp) (make-determined-type (make-number-type)))
	((boolean? exp) (make-determined-type (make-boolean-type)))
	((string? exp) (make-determined-type (make-string-type)))
	((symbol? exp) (make-determined-type (make-symbol-type)))
	((pair? exp)
	 (make-determined-type
	  (make-pair-type (quoted-type-check (car exp))
			  (quoted-type-check (cdr exp)))))
	(else (error "Error: Unknown expression type --- " exp))))

(define (type-check-sequence exps env)
  ; Since env can be mutated, order of for-each is important
  (for-each (lambda (x) (type-check x env))
	    (without-last-element exps))
  (type-check (last-element exps) env))

 
(define (type-check-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (type-check (assignment-value exp) env)
   env)
  (make-determined-type (make-undefined-type)))

(define (type-check-define exp env)
  (define-variable! 
    (cadr exp) 
    (type-check (caddr exp) env) 
    env)
  (make-determined-type (make-undefined-type)))

; TODO: What if first-type is undetermined and second-type is determined?
(define (type-check-if exp env)
  (let ((type-of-test (type-check (if-predicate exp) env)))
    (check-one (type-check (if-predicate exp) env)
	       (make-any-type)
	       (if-predicate exp))
    (if (equal? 'no (check-conversion type-of-test (make-boolean-type)))
	(warn-if-test exp type-of-test)))
  (let* ((second-env (deep-copy env))
	 (first-type (type-check (if-consequent exp) env))
	 (second-type (type-check (if-alternative exp) second-env))
	 (final-type (compute-union first-type second-type)))
    (merge-into! second-env env)
    final-type))

(define (warn-if-test exp type)
  (display "Warning:  In the if expression ")
  (display exp)
  (display " the test ")
  (display (if-predicate exp))
  (display " has type ")
  (display type)
  (display " which can never be #f"))

; Merges two typing environments.
; Modifies actual and does not modify second.
; actual will become a typing environment such that the type bindings
; in the original environments are subtypes of the corresponding
; bindings in the modified version of actual.
; TODO: Determined vs. undetermined
(define (merge-into! second actual)

  (define (env-loop second actual)

    (define (scan-actual vars types frame)
      (if (null? vars)
	  'done
	  (begin
	    (let ((other (lookup-in-frame (car vars) frame)))
	      (if other
		  (set-car! types (compute-union (car types) other))
		  ; Otherwise, leave the type as is, but print a warning
		  ; TODO: Make a better warning message
		  (display "Warning: A variable in one environment is not present in the other one --- merge-into!")))
	    (scan-actual (cdr vars) (cdr types) frame))))

    (cond ((and (eq? second the-empty-environment)
		(eq? actual the-empty-environment))
	   'done)
	  ((or (eq? second the-empty-environment)
	       (eq? actual the-empty-environment))
	   (error "Internal error - environments have different number of frames --- merge-into"))
	  (else
	   (let ((fst (first-frame actual))
		 (snd (first-frame second)))
	     (scan-actual (frame-variables fst)
			  (frame-values fst)
			  snd)
	     (for-each (lambda (var)
			 (display "Warning: A variable in one environment is not present in the other one --- merge-into!")
			 (define-variable-in-frame!
			   var
			   (lookup-in-frame var snd)
			   fst))
		       (remove-all (frame-variables fst)
				   (frame-variables snd))))
	   (env-loop (enclosing-environment second)
		     (enclosing-environment actual)))))

  (env-loop second actual))

(define (type-check-lambda exp env)
  (let* ((newenv (add-undetermined (lambda-parameters exp) env))
	 (range (type-check-sequence (lambda-body exp) newenv)))
    (make-determined-type
     (apply make-function-type
	    (append
	     (map (lambda (x) (lookup-variable-value x newenv))
		  (lambda-parameters exp))
	     (list range))))))

(define (add-undetermined params env)
  (extend-environment
   params
   (map (lambda (x) (make-undetermined-type (make-type-variable)))
	params)
   env))

; Don't just turn it into a lambda - we can do better.
; Since the lambda will only be used once, don't create a generic function type
; Instead, go directly to the function application step
; Since we eliminate the generic function type, this can do better type checking
; Example:  (let ((x 0)) x)
; If converted to a lambda, the expression would have an any type
; Using this version, the expression would have a number type
; Note: Example would no longer be true if using type variables.
(define (type-check-let exp env)
  (type-check-sequence
   (let-body exp)
   (extend-environment (let-parameters exp)
		       (map (lambda (x) (type-check x env))
			    (let-arguments exp))
		       env)))

(define (type-check-application func args exp env)
  ; TODO: Undetermined function inference
  (if (not (function-type? func))
      (begin (display "Bad Function Error: In the expression ")
	     (display exp)
	     (display " the first expression has type ")
	     (display-type func)
	     (display " which is not a function type.")
	     (newline)
	     (error "First expression is not a function --- " exp)))

  (let* ((normal? (normal-function-type? func))
	 (problem? (not ((if normal? = >=) 
			 (length args)
			 (length ((if normal? domain-types normal-domain-types)
				  func))))))
    (if problem?
	(begin (display "Incorrect Number of Arguments Error: In the expression ")
	       (display exp)
	       (display " the function takes ")
	       (if (not normal?) (display "at least "))
	       (display (length ((if normal? domain-types normal-domain-types)
				 func)))
	       (display " arguments, but you gave ")
	       (display (length args))
	       (display " arguments.")
	       (newline)
	       (error "wrong number of arguments --- " exp)))

    (if normal?
	(map check-one args (domain-types func) (cdr exp))
	(let ((normals (normal-domain-types func)))
	  (map check-one
	       (sublist args 0 (length normals)) 
	       normals
	       (sublist (cdr exp) 0 (length normals)))
	  (map (lambda (x y) (check-one x (extra-domain-type func) y))
	       (sublist args (length normals) (length args))
	       (sublist (cdr exp) (length normals) (length args)))))
    (make-determined-type (range-type func))))

; Checks if a single type is compatible with the expected type
; exp is taken as an argument just for the error message.
(define (check-one actual expected exp)
  (let ((answer (check-conversion actual expected)))
    (cond ((equal? answer 'yes)
	   'okay)
	  ((and (equal? answer 'maybe) (undetermined? actual))
	   ; TODO: What if intersect returns #f?
	   (set-raw-type! actual (raw-type (intersect actual expected))))
	  (else
	   (cond ((equal? answer 'maybe)
		  (display "Warning: "))
		 ((equal? answer 'no)
		  (display "Type Error: ")))
	   (if (or (equal? answer 'maybe) (equal? answer 'no))
	       (begin (display "The expression ")
		      (display exp)
		      (display " has type ")
		      (display-type (raw-type actual))
		      (display " but the expected type was ")
		      (display-type (raw-type expected))
		      (newline)))
	   (if (equal? answer 'no)
	       (error "Type checking failed - cannot convert " (raw-type actual) " to " (raw-type expected)))))))

(define (check-expression exp expected env)
  (check-one (type-check exp env) expected exp))

; TODO: (define (define-adt name constructors selectors mutators))

; Adds the name to the global environment without type checking it.
(define (define-type name type)
  (define-variable! name (make-determined-type type) the-global-environment))

(let ((word (make-word-type))
      (number (make-number-type))
      (bool (make-boolean-type))
      (any (make-any-type))
      (var make-type-variable)
      (undefined (make-undefined-type)))
  (define-type 'word (make-variable-function-type word word))

  (define-type 'first (make-function-type word word))
  (define-type 'bf (make-function-type word word))
  (define-type 'butfirst (make-function-type word word))

  (define-type 'last (make-function-type word word))
  (define-type 'bl (make-function-type word word))
  (define-type 'butlast (make-function-type word word))

  (define-type '+ (make-variable-function-type number number))
  (define-type '- (make-variable-function-type number number number))
  (define-type '* (make-variable-function-type number number))
  (define-type '/ (make-variable-function-type number number number))
  (define-type 'quotient (make-function-type number number number))
  (define-type 'remainder (make-function-type number number number))

  (define-type '< (make-variable-function-type number number bool))
  (define-type '> (make-variable-function-type number number bool))
  (define-type '<= (make-variable-function-type number number bool))
  (define-type '>= (make-variable-function-type number number bool))
  (define-type '= (make-variable-function-type number number bool))

  (define-type 'eq? (make-function-type any any bool))
  (define-type 'equal? (make-function-type any any bool))

  (let ((var1 (var))
	(var2 (var)))
    (define-type 'and (make-variable-function-type var1 var1))
    (define-type 'or (make-variable-function-type var2 var2)))
  (define-type 'not (make-function-type any bool))

  (let ((var1 (var))
	(var2 (var)))
    (define-type 'cons (make-function-type var1 var2 (make-pair-type var1 var2))))
  (let ((var1 (var))
	(var2 (var)))
    (define-type 'car (make-function-type (make-pair-type var1 var2) var1)))
  (let ((var1 (var))
	(var2 (var)))
    (define-type 'cdr (make-function-type (make-pair-type var1 var2) var2)))
  
  (define-type 'display (make-function-type any undefined))
  (define-type 'write (make-function-type any undefined))
  (define-type 'print (make-function-type any undefined)))
