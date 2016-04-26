
(use srfi-1 binary-heap test)

(define (key< x y) (- x y))

(define (make-heap alst apred)
  (let ((h (make-binary-heap apred)))
    (fold (lambda (x h) (heap-insert x x h)) h alst)))

(define (heap-sort alst apred)
  (define (extract-until lst aheap)
    (if (zero? (heap-size aheap)) lst
        (extract-until (cons (heap-get-max aheap) lst) (heap-delete-max aheap))))
  (extract-until (list) (make-heap alst apred)))

(test-group "heap-sort"

	    (test (map car (heap-sort '(3 5 7 0 6 5 34 3 6 9 67 5 4 4 3 1 2 3) key<))
		  '(0 1 2 3 3 3 3 4 4 5 5 5 6 6 7 9 34 67)))

(test-exit)
