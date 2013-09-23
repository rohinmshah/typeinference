; ADTs for the various types

; When we intersect two types and change an undetermined type, all
; instances of that type have to be changed.  So, use mutation, and
; think about how to use eq? and equal?

(load "util.scm")

(define attach-tag cons)
(define type-tag car)
(define contents cdr)
(define tagged? pair?)
(define set-type-tag! set-car!)
(define set-contents! set-cdr!)

; Takes a basic type and creates a type ADT out of it.
(define (make-type-from-base base-type)
  (let ((result (attach-tag 'type-adt '())))
    (add-pair-to-type! result 'raw-type base-type)
    result))

; Returns true if the type has options (i.e. it is not a raw type).
(define (type-adt? type)
  (and (tagged? type)
       (equal? (type-tag type) 'type-adt)))

; Adds a key-value pair to the type via mutation.  Returns the type.
(define (add-pair-to-type! type key val)
  (if (type-adt? type)
      (let ((pair (assoc key (contents type))))
	(if pair
	    (set-cdr! pair val)
	    (set-contents! type (cons (cons key val) (contents type))))
	type)
      (error "Not a type ADT --- " type)))

; Returns the value of the key, or #f if the key does not exist.
(define (get-value-of-key key type)
  (if (type-adt? type)
      (let ((val (assoc key (contents type))))
	(if val (cdr val) #f))
      (error "Not a type ADT --- " type)))

(define (key-checker key)
  (lambda (type)
    (pair? (assoc key (contents type)))))

; Adds a flag via mutation. Returns the type.
(define (add-flag-to-type! type flag)
  (add-pair-to-type! type flag #t))

; Removes a flag via mutation. Returns the type.
; It is legal to call this before calling add-flag-to-type!
(define (remove-flag-from-type! type flag)
  (add-pair-to-type! type flag #f))

; Returns a function that checks whether a certain flag is present in
; a type.
; Since a flag is represented at a key with a boolean value, we can
; just return that value.  If the key does not exist, should return #f.
(define (flag-checker flag)
  (lambda (type) (and (type-adt? type)
		      (get-value-of-key flag type))))

; Creates a type variable.  If no arguments are given, the type
; variable can take on any type.
; If an argument is given, it must be an existing type.  Then the type
; variable can only take on subtypes of that type.
(define make-type-variable
  (let ((global-id 0))
    (lambda args
      (let ((type (cond ((null? args) (make-any-type))
			((null? (cdr args)) (car args))
			(else (error "Too many arguments to make-type-variable")))))
	(set! global-id (1+ global-id))
	(add-pair-to-type! type 'type-variable global-id)
	type))))

(define type-variable? (key-checker 'type-variable))

(define (type-variable-id type-var)
  (if (type-variable? type-var)
      (get-value-of-key 'type-variable type-var)
      (error "Not a type variable --- " type-var)))

(define (make-undetermined-type type)
  (add-flag-to-type! type 'undetermined))
(define undetermined? (flag-checker 'undetermined))

(define (make-determined-type type)
  (remove-flag-from-type! type 'undetermined))
(define (determined? type)
  (not (undetermined? type)))

(define (set-raw-type! type newtype)
  (if (undetermined? type)
      (add-pair-to-type! type 'raw-type newtype)
      (error "Not an undetermined type --- " type)))

(define (raw-type type)
  (if (type-adt? type)
      (get-value-of-key 'raw-type type)
      (error "Not a type ADT --- " type)))

; Creates a function that checks whether a raw type represents the
; type given by tag.
(define (type-checker tag)
  (lambda (type)
    (and (type-adt? type)
	 (let ((base-type (raw-type type)))
	   (and (tagged? base-type)
		(equal? (type-tag base-type) tag))))))

(define (make-any-type) 
  (make-type-from-base (attach-tag 'any '())))
(define any-type? (type-checker 'any))

(define (make-word-type) 
  (make-type-from-base (attach-tag 'word '())))
(define word-type? (type-checker 'word))

(define (make-boolean-type) 
  (make-type-from-base (attach-tag 'boolean '())))
(define boolean-type? (type-checker 'boolean))

(define (make-number-type) 
  (make-type-from-base (attach-tag 'number '())))
(define number-type? (type-checker 'number))

(define (make-symbol-type) 
  (make-type-from-base (attach-tag 'symbol '())))
(define symbol-type? (type-checker 'symbol))

(define (make-string-type) 
  (make-type-from-base (attach-tag 'string '())))
(define string-type? (type-checker 'string))

(define (make-null-type)
  (make-type-from-base (attach-tag 'null '())))
(define null-type? (type-checker 'null))

(define (make-undefined-type) 
  (make-type-from-base (attach-tag 'undefined '())))
(define undefined-type? (type-checker 'undefined))

(define (make-function-type . args)
  (if (null? args)
      (error "Didn't specify a range for the function type")
      (make-type-from-base
       (attach-tag 'function (cons (last-element args)
				   (without-last-element args))))))

(define normal-function-type? (type-checker 'function))

(define (domain-types func)
  (if (normal-function-type? func)
      (cdr (contents (raw-type func)))
      (error "Not a normal function type --- " func)))

; Range-type works on normal and variable function types - see below

(define (make-variable-function-type . args)
  (if (or (null? args) (null? (cdr args)))
      (error "Didn't specify a range and/or default type for the function")
      (make-type-from-base
       (attach-tag 'var-func (cons (last-element args)
				   (without-last-element args))))))

(define variable-function-type? (type-checker 'var-func))

(define (function-type? func)
  (or (normal-function-type? func) (variable-function-type? func)))

(define (normal-domain-types func)
  (if (variable-function-type? func)
      (without-last-element (cdr (contents (raw-type func))))
      (error "Not a variable function type --- " func)))

(define (extra-domain-type func)
  (if (variable-function-type? func)
      (last-element (cdr (contents (raw-type func))))
      (error "Not a variable function type --- " func)))

(define (range-type func)
  (if (function-type? func)
      (car (contents (raw-type func)))
      (error "Not a function type --- " func)))

(define (make-union-type . types)
  (make-type-from-base (attach-tag 'union types)))
(define union-type? (type-checker 'union))

(define (union-types union)
  (if (union-type? union)
      (contents (raw-type union))
      (error "Not a union type --- " union)))

(define (make-pair-type t1 t2)
  (make-type-from-base (attach-tag 'pair (cons t1 t2))))

(define pair-type? (type-checker 'pair))

(define (first-pair-type pair)
  (if (pair-type? pair)
      (car (contents (raw-type pair)))
      (error "Not a pair type --- " pair)))

(define (second-pair-type pair)
  (if (pair-type? pair)
      (cdr (contents (raw-type pair)))
      (error "Not a pair type --- " pair)))

(define (make-vector-type base-type)
  (make-type-from-base (attach-tag 'vector base-type)))

(define vector-type? (type-checker 'vector))

(define (vector-base-type vec)
  (if (vector-type? vec)
      (contents (raw-type vec))
      (error "Not a vector type --- " vec)))

; TODO: Write a good display-type
(define (display-type type)
  (cond ((function-type? type)
	 (for-each (lambda (x) (display-type x) (display " -> "))
		   (domain-types type))
	 (display-type (range-type type)))
	(else (display type))))
