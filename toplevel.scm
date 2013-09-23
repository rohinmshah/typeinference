; File used to test an interface to the type inference.

(load "inference")

; Need to figure out how to evaluate the expression returned by read in the current environment instead of global environment.
#|
(define repl
  (let ((old-define define))
    (define (loop)
      (display "Typed STk> ")
      (flush)
      (print (eval (read)))
      (loop))
    (define-macro (define arg . body)
      (let* ((desugared (desugar-define `(define ,arg ,@body)))
	     (type (typeof desugared)))
	(display (cadr desugared))
	(display " has type ")
	(display-type (typeof (cadr desugared)))
	(newline)
	`(old-define ,@(cdr desugared))))
    loop))
|#

(define-macro (typed-define arg body)
  (let ((desugared (desugar-define `(define ,arg ,body))))
    (typeof desugared)
    (display (cadr desugared))
    (display " has type ")
    (display-type (typeof (cadr desugared)))
    (newline)
    desugared))

(define-macro (silent-typed-define arg body)
  (let ((desugared `(define ,arg ,body)))
    (typeof desugared)
    desugared))

(define (repl)
  (display "Typed STk> ")
  (flush)
  (let ((exp (read)))
    (typeof exp)
    (if (definition? exp)
	(begin (display (definition-variable exp))
	       (display " has type ")
	       (display-type (typeof (definition-variable exp)))
	       (newline)))
    (print (eval exp))
    (repl)))
