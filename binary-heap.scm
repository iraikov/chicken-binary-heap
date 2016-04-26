;;
;; 
;; Binary heap implementation.  Based on the Ocaml heap implementation
;; by Jean-Christophe Filliatre. Comments in the code are from the
;; original implementation.
;;
;; Copyright 2009-2016 Ivan Raikov.
;;
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.

(module binary-heap

  (
   make-binary-heap
   alist->binary-heap
   heap-empty?
   heap-for-each
   heap-fold
   heap-insert
   heap-size
   heap-get-max
   heap-delete-max
   )

  (import scheme chicken data-structures) 
  (require-library srfi-1)
  (import (only srfi-1 fold first second third fourth fifth sixth)) 
  (require-extension datatype matchable)


;;
;;  Heaps are encoded as binary trees that have the heap property,
;;  namely the value of any node is greater or equal than the nodes in
;;  its left and right subtrees.
;;
;;  The representation invariant is the following: the number of nodes
;;  in the left subtree is equal to the number of nodes in the right
;;  subtree, or exceeds it by exactly once. In the first case, we use
;;  the constructor [Same] and in the second the constructor [Diff].
;;  Then it can be proved that [2^(h-1) <= n <= 2^h] when [n] is the
;;  number of elements and [h] the height of the tree.
;;

(define-record-type binary-heap
  (cons-binary-heap root size key-compare)
  binary-heap?
  (root binary-heap-root)
  (size binary-heap-size)
  (key-compare binary-heap-key-compare)
  )


(define-datatype tree tree?
  (Empty )
  (Same (left tree?) (elt pair?) (right tree?)) ;; same number of elements on both sides 
  (Diff (left tree?) (elt pair?) (right tree?)) ;; left has [n+1] nodes and right has [n] 
  )


(define (tree-tag x)
  (cases tree x
	 (Empty () 'Empty)
	 (Same (l x r) 'Same)
	 (Diff (l x r) 'Diff)))


(define-record-printer (tree x out)
  (cases tree x 
	 (Empty () (display "#(Empty)" out))
	 (Same (l x r)
	       (display "#(Same " out)
	       (display (tree-tag l) out)
	       (display (conc " " x " ") out)
	       (display (tree-tag r) out)
	       (display ")" out))
	 (Diff (l x r)
	       (display "#(Diff " out)
	       (display (tree-tag l) out)
	       (display (conc " " x " ") out)
	       (display (tree-tag r) out)
	       (display ")" out))))


;;
;; This macro was borrowed from treap.scm by Oleg Kiselyov
;;
(define-syntax dispatch-on-key 
  (lambda (x r c)
    (let ((key-compare (second x))
          (key (third x)) (node-key (fourth x))
	  (on-greater (fifth x)) (on-less (sixth x)))
      (let ((%let   (r 'let))
	    (%cond  (r 'cond))
	    (%else  (r 'else))
	    (%positive?  (r 'positive?))
	    (result      (r 'result)))
	`(,%let ((,result (key-compare ,key ,node-key )))
		(,%cond
		 ((,%positive? ,result) ,on-greater)
		 (,%else                ,on-less)))))))


(define (insert key value root key-compare) 
  (let recur ((key key) (value value) (root root))
    (cases tree root
           (Empty ()       (Same (Empty) (cons key value) (Empty)))
           ;; insertion to the left
           (Same (l y r)   (dispatch-on-key
                            key-compare key (car y)
                            (Diff (recur (car y) (cdr y) l) (cons key value) r)
                            (Diff (recur key value l) y r)))
           ;; insertion to the right
           (Diff (l y r)   (dispatch-on-key
                            key-compare key (car y)
                            (Same l (cons key value) (recur (car y) (cdr y) r))
                            (Same l y (recur key value r)))))
    ))


(define (maximum root)
  (cases tree root
         (Empty () #f)
         (Same (l x r)  x)
         (Diff (l x r)  x)))


;; extracts one element on the bottom level of the tree, while
;; maintaining the representation invariant
(define (extract-last root)
  (match root
         (($ tree 'Empty) #f)
         (($ tree 'Same ($ tree 'Empty) x ($ tree 'Empty)) 
          (list x (Empty)))
         (($ tree 'Same l x r)  
          (match-let (((y r1)  (extract-last r)))
                     (list y (Diff l x r1))))
         (($ tree 'Diff l x r)  
          (match-let (((y l1)  (extract-last l)))
                     (list y (Same l1 x r))))))


;; removes the topmost element of the tree and inserts a new element 
(define (descent key value root key-compare)
  (let recur ((root root))
    (match root
           (($ tree 'Empty)  #f)
           (($ tree 'Same ($ tree 'Empty) _ ($ tree 'Empty))
            (Same (Empty) (cons key value) (Empty)))
           (($ tree 'Diff (and l ($ tree 'Same _ z _)) _ ($ tree 'Empty))
            (dispatch-on-key key-compare key (car z)
                             (Diff l (cons key value) (Empty))
                             (Diff (Same (Empty ) (cons key value) (Empty)) z (Empty))))
           (($ tree tag l _ r)
            (let ((op (case tag ((Same) Same) ((Diff) Diff)))
                  (ml (maximum l)) (mr (maximum r)))
              (if (and (positive? (key-compare key (car ml)))
                       (positive? (key-compare key (car mr))))
                  (op l (cons key value) r)
                  (dispatch-on-key key-compare (car ml) (car mr)
                                   (op (recur l) ml r) 
                                   (op l mr (recur r)))))))
    ))

(define (delete root key-compare)
  (match root
         (($ tree 'Empty)  #f)
         (($ tree 'Empty ($ tree 'Empty) x ($ tree 'Empty))
          (Empty))
         (else  (match-let (((y root1) (extract-last root)))
                           (descent (car y) (cdr y) root1 key-compare)))))


(define (make-binary-heap key-compare)
  (cons-binary-heap (Empty) 0 key-compare))

(define (heap-empty? h)
  (cases tree (binary-heap-root h)
         (Empty () #t)
         (else #f)))


(define (heap-for-each f h)
  (let recur ((root (binary-heap-root h)))
    (match root
           (($ tree 'Empty)  (begin))
           (($ tree tag l x r)
            (begin (recur l)
                   (f x)
                   (recur r)))
           ))
  )


(define (heap-fold f init h)
  (define (foldf tree ax)
    (match tree
           (($ tree 'Empty)  (begin))
           (($ tree tag l x r)
            (foldf l (f x (foldf r ax))))))
  (foldf (binary-heap-root h) (f init)))


(define (heap-insert key value h)
  (let ((new-root (insert key value (binary-heap-root h) (binary-heap-key-compare h))))
    (cons-binary-heap new-root (+ 1 (binary-heap-size h))
                      (binary-heap-key-compare h))))

(define (alist->binary-heap alst key-compare)
  (let ((aheap (cons-binary-heap (Empty) 0 key-compare)))
    (fold (lambda (x h) (heap-insert (car x) (cdr x) h)) aheap alst)
    ))

  
(define (heap-size h) 
  (binary-heap-size h))


(define (heap-get-max h) 
  (maximum (binary-heap-root h)))

	  
(define (heap-delete-max h)  
  (let ((new-root (delete (binary-heap-root h) (binary-heap-key-compare h))))
    (cons-binary-heap new-root (- (binary-heap-size h) 1)
                      (binary-heap-key-compare h))))

)
	
