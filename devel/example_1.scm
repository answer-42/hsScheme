(define fact 
  (lambda (n)
    (define fact-iter 
      (lambda (i acc)
	(cond ((= i 0) acc)
	      (else (fact-iter (- i 1) (* acc i))))))
    (fact-iter n 1)))

(define fact 
  (lambda (n)
    (letrec ((fact-iter 
	      (lambda (i acc)
		(cond ((= i 0) acc)
		      (else (fact-iter (- i 1) (* acc i)))))))
      (fact-iter n 1))))


(define fib
  (lambda (n)
    (define fib-iter
      (lambda (i a b)
	(cond ((= i 1) b)
	      (else (fib-iter (- i 1) b (+ a b))))))
    (fib-iter n 0 1)))

(display (fact 10))
(newline)
(display (fib 5))

(map fib '(1 2 3 4 5))
(map fib (list 1 2 3 4 5))

(define-syntax change 
  (syntax-rules () 
    ((change x) 
     (set! x 10))
    ((change x y)
     (set! x y))))
