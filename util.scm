; Returns the last pair of a list.
(define (last-pair lst)
  (if (or (null? lst) (not (list? lst)))
      (error "Invalid argument to last-pair --- " lst)
      (if (null? (cdr lst))
	  lst
	  (last-pair (cdr lst)))))

(define (last-element lst)
  (car (last-pair lst)))

(define (without-last-element lst)
  (if (or (null? lst) (not (list? lst)))
      (error "Invalid argument to without-last-element --- " lst)
      (if (null? (cdr lst))
	  '()
	  (cons (car lst) (without-last-element (cdr lst))))))

; Returns a list of the elements at indices from start (inclusive) to
; end (exclusive).  Any incorrect indices are ignored.
(define (sublist lst start end)
  (cond ((or (null? lst) (= end 0)) '())
	((= start 0)
	 (cons (car lst) (sublist (cdr lst) 0 (- end 1))))
	(else (sublist (cdr lst) (- start 1) (- end 1)))))

(define (flatmap fn lst)
  (accumulate append '()
	      (map fn lst)))

(define (indexq elmt lst)
  (define (helper lst sofar)
    (cond ((null? lst) #f)
	  ((eq? elmt (car lst)) sofar)
	  (else (helper (cdr lst) (+ 1 sofar)))))
  (helper lst 0))

; Copies a cons structure by creating new pairs, so that mutating one
; of the structures doesn't affect the other.
(define (deep-copy pair)
  (let ((seen '())
	(newpairs '()))
    (define (helper pair)
      (if (not (pair? pair))
	  pair
	  (let ((index (indexq pair seen)))
	    (if index
		(list-ref newpairs index)
		(let ((new-pair (cons '() '())))
		  (set! seen (cons pair seen))
		  (set! newpairs (cons new-pair newpairs))
		  (set-car! new-pair (helper (car pair)))
		  (set-cdr! new-pair (helper (cdr pair)))
		  new-pair)))))
    (helper pair)))

; Computes the union of two sets (represented as lists).
(define (merge x y)
  (cond ((null? x) y)
	((member (car x) y)
	 (merge (cdr x) y))
	(else (merge (cdr x) (cons (car x) y)))))

; Returns a list of all the elements of lst that are not present in
; items.  Note that an element in items need not be present in lst.
(define (remove-all items lst)
  (cond ((null? lst) '())
	((member (car lst) items)
	 (remove-all items (cdr lst)))
	(else
	 (cons (car lst) (remove-all items (cdr lst))))))

; member parameterized by an equality procedure.
(define (hof-member equality? elmt lst)
  (cond ((null? lst) #f)
	((equality? elmt (car lst)) lst)
	(else (hof-member equality? elmt (cdr lst)))))

(define (make-list elmt num)
  (if (= num 0)
      '()
      (cons elmt (make-list elmt (- num 1)))))
