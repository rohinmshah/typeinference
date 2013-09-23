; Operations on the type ADTs defined in types.scm

; May or may not return a union type
; Uses check-conversion, defined later in this file
(define (compute-union . types)
  (if (null? types)
      (error "compute-union called with no arguments")
      (let ((flatten (flatmap (lambda (x)
				(if (union-type? x)
				    (union-types x)
				    (list x)))
			      types)))
	(define (remove-dups lst sofar)
	  (cond ((null? lst) sofar)
		((member 'yes (map (lambda (x) (check-conversion (car lst) x))
				   (append sofar (cdr lst))))
		 (remove-dups (cdr lst) sofar))
		(else (remove-dups (cdr lst) (cons (car lst) sofar)))))
	(let ((set (remove-dups flatten '())))
	  (if (null? (cdr set))
	      (car set)
	      (apply make-union-type set))))))

(define make-tree cons)
(define datum car)
(define children cdr)
(define set-datum! set-car!)
(define set-children! set-cdr!)
; the-empty-tree is '().

; Finds a node whose datum is x in tree.
; Returns the node whose datum is x if it exists, #f otherwise.
(define (find-node x tree)
  (if (equal? x (datum tree))
      tree
      (accumulate (lambda (x y) (or x y))
		  #f
		  (map (lambda (t) (find-node x t))
		       (children tree)))))

; subtyping-tree is a tree of the type tags (not types themselves)
; t1 is descendant of t2 ==> t1 is a subtype of t2
(define subtyping-tree
  (make-tree (type-tag (raw-type (make-any-type))) '()))

; Adds a type to the subtyping tree.  It is a child of parent.
(define (add-type! type parent)
  (let ((parent-tag (type-tag (raw-type parent)))
	(child-tag (type-tag (raw-type type))))
    (define (helper node)
      (if (equal? parent-tag (datum node))
	  (if (null? (children node))
	      (set-children! node (list (make-tree child-tag '())))
	      (set-cdr! (last-pair (children node))
			(list (make-tree child-tag '()))))
	  (map helper (children node))))
    (helper subtyping-tree)))

(add-type! (make-word-type) (make-any-type))
(add-type! (make-number-type) (make-word-type))
(add-type! (make-string-type) (make-word-type))
(add-type! (make-symbol-type) (make-word-type))
(add-type! (make-boolean-type) (make-any-type))
(add-type! (make-null-type) (make-any-type))
(add-type! (make-vector-type (make-boolean-type)) (make-any-type))
(add-type! (make-pair-type (make-any-type) (make-any-type))
	   (make-any-type))
(add-type! (make-union-type (make-number-type)) (make-any-type))
(add-type! (make-function-type (make-any-type)) (make-any-type))
(add-type! (make-variable-function-type (make-word-type) (make-word-type))
	   (make-word-type))

; Checks if t1 is a subtype of t2 based on the subtyping tree.
; t1 and t2 are type tags.
; Since they are type tags, in the case that they are equal, based on
; the subtyping tree t1 is a subtype of t2, but since the actual types
; may not be subtypes, it will return #f when t1 is equal to t2.
; Returns node corresponding to t1 if t1 is a proper descendant of t2,
; #f otherwise.
(define (is-base-subtype? t1 t2)
  (and (not (equal? t1 t2))
       (let ((node (find-node t2 subtyping-tree)))
	 (and node (find-node t1 node)))))

; Or for yes, maybe, and no.
; yes is like #t, no is like #f, and maybe is in between.
(define (triple-or . args)
  (cond ((member 'yes args) 'yes)
	((member 'maybe args) 'maybe)
	(else 'no)))

; And for yes, maybe, and no.
; yes is like #t, no is like #f, and maybe is in between.
(define (triple-and . args)
  (cond ((member 'no args) 'no)
	((member 'maybe args) 'maybe)
	(else 'yes)))

; Checks whether we can convert type t1 to type t2.
; If we can (i.e. t1 is a subtype of t2), returns 'yes
; If it may work (eg. number + bool to number), returns 'maybe
; If it will not work (eg. number to bool), returns 'no
; Note: "t1 is a subtype of t2" means that t1 can be substituted in
; any place that uses a t2.
; TODO: Covariance + contravariance for pairs and vectors.
; Vector of numbers is a subtype of vector of words if you only read
; from it
; Vector of words is a subtype of vector of numbers if you only write
; to it
; Same thing for pairs.  Suggested fix - add a co/contra variance flag
; to any data structure type (i.e. pairs and vectors).
(define (check-conversion t1 t2)
  (cond ((equal-types? t1 t2 #f)
	 'yes)
	((is-base-subtype? (type-tag (raw-type t1)) (type-tag (raw-type t2)))
	 'yes)
	((is-base-subtype? (type-tag (raw-type t2)) (type-tag (raw-type t1)))
	 'maybe)
	; The next check must be for union types.  Otherwise, if say
	; we checked for pairs first, a pair would not be considered a
	; subtype of the union of a pair and a number.
	((union-type? t1)
	 ; This includes the case where both t1 and t2 are unions.
	 ; For a yes, every type in t1 must be convertible to t2.
	 ; For a no, no type in t1 can be convertible to t2.
	 (let ((subchecks (map (lambda (x) (check-conversion x t2))
			       (union-types t1))))
	   (cond ((equal? 'yes (apply triple-and subchecks))
		  'yes)
		 ((equal? 'no (apply triple-or subchecks))
		  'no)
		 (else 'maybe))))
	((union-type? t2)
	 ; t1 is a subtype of a union type if it is a subtype of any
	 ; of the constituent types.
	 ; Note: Here we know t1 is not a union type.  This is crucial.
	 ; If t1 = (union (bool) (num)) and t2 = (union (bool) (word))
	 ; t1 is in fact a subtype of t2, and so we should get 'yes. 
	 ; However, t1 is NOT a subtype of any of the constituent
	 ; types, and so this code would say 'maybe.
	 (apply triple-or
		(map (lambda (x) (check-conversion t1 x))
		     (union-types t2))))
	((and (pair-type? t1) (pair-type? t2))
	 ; Pair types can only be subtypes of other pair types (if you ignore
	 ; union types).  The constituent types must also be subtypes.
	 (triple-and (check-conversion (first-pair-type t1)
				       (first-pair-type t2))
		     (check-conversion (second-pair-type t1)
				       (second-pair-type t2))))
	((or (pair-type? t1) (pair-type? t2))
	 'no)
	((and (normal-function-type? t1)
	      (normal-function-type? t2)
	      (= (length (domain-types t1)) (length (domain-types t2))))
	 ; For t1 to replace t2, t1 and t2 must take the same number
	 ; of arguments.
	 ; In order to prevent invalid argument errors, t1 must accept
	 ; at *least* the same arguments as t2, and so the domain
	 ; types of t2 must be subtypes of those of t1.
	 ; If the rest of the code expects the range type of t2, t1's
	 ; range type should be a subtype of t2 to prevent errors.
	 (apply triple-and
		(check-conversion (range-type t1) (range-type t2))
		(map check-conversion (domain-types t2) (domain-types t1))))
	((and (variable-function-type? t1) (variable-function-type? t2))
	 ; This is similar to normal functions with more caveats.
	 ; 1. In order to replace t2 everywhere, t1 must not require
	 ; more arguments than t2.
	 ; 2. When checking the domain types, the lengths of the lists
	 ; of required domain types should be extended so that they
	 ; are of equal length, to allow proper comparison.
	 ; 3. Even if the extra domain types are not compatible, the
	 ; answer could still be a maybe.  Consider a function call in
	 ; which there are no extra arguments - in that case it
	 ; doesn't matter that the extra domain types are incompatible.
	 (let* ((normal1 (normal-domain-types t1))
		(normal2 (normal-domain-types t2))
		(extra1 (extra-domain-type t1))
		(extra2 (extra-domain-type t2))
		(len1 (length normal1))
		(len2 (length normal2))
		(new1 (if (>= len1 len2) ; Point 2
			  normal1
			  (append normal1 (make-list extra1 (- len2 len1)))))
		(new2 (if (>= len2 len1) ; Point 2
			  normal2
			  (append normal2 (make-list extra2 (- len1 len2))))))
	   (apply triple-and
		  (if (<= len1 len2) ; Point 1
		      'yes
		      'maybe)
		  (triple-or 'maybe (check-conversion extra2 extra1)) ; Point 3
		  (check-conversion (range-type t1) (range-type t2))
		  (map check-conversion new2 new1))))
	((and (variable-function-type? t1)
	      (normal-function-type? t2))
	 ; Similar and slightly simpler than two variable functions.
	 ; In this case however, if t1 requires too many arguments, it
	 ; can never be a replacement for t2.
	 (let* ((types1 (normal-domain-types t1))
		(types2 (domain-types t2))
		(extra (extra-domain-type t1))
		(len1 (length types1))
		(len2 (length types2)))
	   (if (< len2 len1)
	       'no
	       (apply triple-and
		      (check-conversion (range-type t1) (range-type t2))
		      (map check-conversion
			   types2
			   (append types1
				   (make-list extra (- len2 len1))))))))
	((and (normal-function-type? t1)
	      (variable-function-type? t2))
	 ; A normal function can never replace all possible cases for
	 ; a variable function, so this can be at most a maybe.
	 (triple-and 'maybe (check-conversion t2 t1)))
	((or (function-type? t1) (function-type? t2))
	 ; Two cases - either only one of t1 and t2 is a function type
	 ; or t1 and t2 are normal function types that take different
	 ; numbers of arguments.
	 'no)
	(else 'no)))

; Computes the intersection of two types.
; Returns a type t such that any value that is of both type t1 and type
; t2 is also of type t.  Ideally, t would be the most general possible type.
; If there is no such type, returns #f.
; TODO: Finish this (is very much broken right now)
(define (intersect t1 t2)
  (let ((subtype-status (check-conversion t1 t2)))
    (cond ((equal? subtype-status 'yes)
	   t1)
	  ((and (equal? subtype-status 'maybe)
		(equal? (check-conversion t2 t1) 'yes))
	   t2)
	  ((union-type? t1)
	   (let ((lst 
		  (remove #f
			  (map (lambda (type)
				 (intersect type t2))
			       (union-types t1)))))
	     (and (not (null? lst)) (apply compute-union lst))))
	  ((union-type? t2)
	   (let ((lst
		  (remove #f
			  (map (lambda (type)
				 (intersect t1 type))
			       (union-types t2)))))
	     (and (not (null? lst)) (apply compute-union lst))))
	  ((and (pair-type? t1) (pair-type? t2))
	   ; TODO: Co/contravariance (see check-conversion)
	   (let ((fst (intersect (first-pair-type t1)
				 (first-pair-type t2)))
		 (snd (intersect (second-pair-type t1)
				 (second-pair-type t2))))
	     (and fst snd (make-pair-type fst snd))))
	  ((and (normal-function-type? t1)
		(normal-function-type? t2)
		(= (length (domain-types t1))
		   (length (domain-types t2))))
	   (let ((lst (append (map compute-union
				   (domain-types t1)
				   (domain-types t2))
			      (list (intersect
				     (range-type t1)
				     (range-type t2))))))
	     (and (not (member #f lst)) (apply make-function-type lst))))
	  ((and (normal-function-type? t1) (normal-function-type? t2))
	   ; Interestingly, although (check-conversion t1 t2) would return 'no,
	   ; there is an intersection - a variable function.
	   ; This is more of an engineering problem than a problem with the
	   ; theory - in principle, we could always infer that something is a
	   ; variable function and never use normal functions for inference.
	   ; However, it's pretty rare that a function is intended to take
	   ; variable functions - usually normal functions work just fine.
	   ; So, we infer normal functions first, so that the inferred types
	   ; make more sense to the user.
	   ; Only if we have proof that it is not a normal function (i.e. takes
	   ; different number of arguments) do we use a variable function.
	   (let* ((domain1 (domain-types t1))
		  (domain2 (domain-types t2))
		  (len1 (length domain1))
		  (len2 (length domain2))
		  (normal (map compute-union domain1 domain2))
		  (extra (apply compute-union
				(if (>= len1 len2)
				    (sublist domain1 (- len1 len2) len1)
				    (sublist domain2 (- len2 len1) len2))))
		  (range (intersect (range-type t1) (range-type t2)))
		  (args (append normal (list extra) (list range))))
	     (and (not (member #f args))
		  (apply make-variable-function-type args))))
	  ((and (normal-function-type? t1)
		(variable-function-type? t2)
		(>= (length (domain-types t1))
		    (length (normal-domain-types t2))))
	   (let* ((domain1 (domain-types t1))
		  (domain2 (normal-domain-types t2))
		  (extra (extra-domain-type t2))
		  (len1 (length domain1))
		  (len2 (length domain2))
		  (new (append domain2
			       (make-list extra (- len1 len2))))
		  (args (append (map compute-union domain1 new)
				(list extra
				      (intersect (range-type t1)
						 (range-type t2))))))
	     (and (not (member #f args))
		  (apply make-variable-function-type args))))
	  ((and (variable-function-type? t1) (normal-function-type? t2))
	   (intersect t2 t1))
	  ((and (variable-function-type? t1) (variable-function-type? t2))
	   #f)
	  (else #f))))

; Checks if two types are equal.
; Does not take determined/undetermined into account.
; Assumption:  Any union types were created by compute-union
(define (equal-types? t1 t2 consider-type-variable?)
  (define (recurse t1 t2)
    (cond ((and consider-type-variable? (type-variable? t1))
	   ; As long as the ids match, they are equal.  The raw types
	   ; should be equal if the ids are equal.
	   (and (type-variable? t2)
		(= (type-variable-id t1) (type-variable-id t2))))
	  ((and consider-type-variable? (type-variable? t2))
	   #f)
	  ((and (normal-function-type? t1) (normal-function-type? t2))
	   (and (= (length (domain-types t1))
		   (length (domain-types t2)))
		(not (member #f (map recurse
				     (domain-types t1)
				     (domain-types t2))))
		(recurse (range-type t1) (range-type t2))))
	  ((and (variable-function-type? t1) (variable-function-type? t2))
	   (and (= (length (normal-domain-types t1))
		 (length (normal-domain-types t2)))
		(not (member #f (map recurse
				     (normal-domain-types t1)
				     (normal-domain-types t2))))
		(recurse (extra-domain-type t1) (extra-domain-type t2))
		(recurse (range-type t1) (range-type t2))))
	  ((and (union-type? t1) (union-type? t2))
	   (not (member
		 #f
		 (append
		  (map (lambda (type)
			 (hof-member recurse type (union-types t2)))
		       (union-types t1))
		  (map (lambda (type)
			 (hof-member recurse type (union-types t1)))
		       (union-types t2))))))
	  ((and (pair-type? t1) (pair-type? t2))
	   (and (recurse (first-pair-type t1) (first-pair-type t2))
		(recurse (second-pair-type t1) (second-pair-type t2))))
	  ((and (vector-type? t1) (vector-type? t2))
	   (recurse (vector-base-type t1) (vector-base-type t2)))
	  (else (equal? (raw-type t1) (raw-type t2)))))
  (recurse t1 t2))
