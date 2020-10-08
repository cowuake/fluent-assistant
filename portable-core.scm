;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FLUENT SCHEME ASSISTANT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2020 Riccardo Mura
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     1. Redistributions of source code must retain the above copyright notice,
;;        this list of conditions and the following disclaimer.
;;     2. Redistributions in binary form must reproduce the above copyright
;;        notice, this list of conditions and the following disclaimer in the
;;        documentation and/or other materials provided with the distribution.
;;     3. Neither the name of the copyright holder nor the names of its
;;        contributors may be used to endorse or promote products derived from
;;        this software without specific prior written permission.
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;
;; BASE UTILITIES
;;       (REUSABLE)
;;;;;;;;;;;;;;;;;;;;

(define add1
  (lambda (x)
    (+ x 1)))


(define sub1
  (lambda (x)
    (- x 1)))


(define range
  (lambda (first increment last)
    (if (<= first last)
	(cons first (if (= first last)
			'()
			(range (+ first increment) increment last))))))


(define sequence
  (lambda (first last)
      (range first 1 last)))


;; TODO Assign proper name
(define dummy
  (lambda (x y)
    (if (>= x y)
       (range 0 y x)
       (range 0 x y))))


(define get-value-from-alist
  (lambda (value alist)
    (cdr (assoc value alist))))


;(define transform-alist-keys
;  (lambda (function alist)
;    (



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERACTION WITH THE SHELL
;;                   (REUSABLE)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;
;; BASE MATH
;;  (REUSABLE)
;;;;;;;;;;;;;;;

(define add
  (lambda (x)
    (reduce + x)))

(define multiply
  (lambda (x)
    (reduce * x)))


(define recursive-op
  (lambda (operator neutral f lower upper)
    (if (<= lower upper)
	(operator (f lower)
		  (if (= lower upper)
		      neutral
		      (recursive-op operator neutral f (+ lower 1) upper))))))


(define sum
  (lambda (f lower upper)
    (recursive-op + 0 f lower upper)))


(define product
  (lambda (f lower upper)
    (recursive-op * 1 f lower upper)))


(define factorial
  (lambda (n)
    (cond ((= n 0) 1)
	  ((> n 0) (* n (factorial (- n 1))))
	  ((< n 0) '()))))


(define binomial-coefficient
  (lambda (p q)
    ( / (factorial p)
        (* (factorial q)
           (factorial (- p q))))))


(define positive-integer?
  (lambda (n)
    (and (integer? n)
         (positive? n))))


(define non-negative-integer?
  (lambda (n)
    (or (positive-integer? n)
        (equal? n 0))))


(define binet
  (lambda (n)
    (when (non-negative-integer? n)
        (let* ((a (sqrt 5))
               (b (/ (+ 1 a) 2))
               (c (/ (- 1 a) 2)))
          (/ (- (expt b n)
                (expt c n))
             a)))))


;; This is a non-tail-recursive implementation for finding a Fibonacci number
(define fibonacci-number-direct
  (lambda (n)
    (when (non-negative-integer? n)
	(cond
	 ((equal? n 1) 0)
	 ((equal? n 2) 1)
	 (else (+ (fibonacci-number-direct (- n 1))
		  (fibonacci-number-direct (- n 2))))))))


;; This is a tail-recursive implementation for finding a Fibonacci number
(define fibonacci-number-tail-recursive
  (lambda (n)
    (define aux
      (lambda (acc1 acc2 current-n)
	(if (equal? current-n n)
	    (+ acc1 acc2)
	    (aux acc2
		 (+ acc1 acc2)
		 (add1 current-n)))))
    (cond
     ((equal? n 1) 0)
     ((equal? n 2) 1)
     (else (aux 0 1 3)))))


;; On a "good" Scheme implementation, this will lead to the correct Fibonacci
;; sequence no matter how elements are computed - this need arbitrary precision
;; for integers. On Fluent Scheme, computing the first 100 entries is enough in
;; order to observe numbers with the wrong sign because of integer overflow.
(define fibonacci-slow
  (lambda (n)
    (if (positive-integer? n)
        (cond ((equal? n 1) '(0))
              ((equal? n 2) '(0 1))
              (else (let* ((previous
                            (fibonacci-slow (sub1 n)))
                           (reversed
                            (reverse previous))
                           (last
                            (list-ref reversed 0))
                           (before-last
                            (list-ref reversed 1)))
		      (append previous (list (+ last before-last)))))))))


;; This exists only for showing that an even slower compund procedure for
;; computing a Fibonacci sequence can exist.
(define fibonacci-slower
  (lambda (n)
    (if (positive-integer? n)
        (cond ((equal? n 1) '(0))
              ((equal? n 2) '(0 1))
              (else (append (fibonacci-slower (sub1 n))
                            (list (+ (list-ref (reverse (fibonacci-slower (sub1 n))) 0)
                                     (list-ref (reverse (fibonacci-slower (sub1 n))) 1)))))))))


;; This is a fast implementation for computing a Fibonacci sequence. Since it
;; relies on the Binet formula, the number of entries which can be computed is
;; limited by float overflow.
(define fibonacci-fast
  (lambda (n)
    (if (positive-integer? n)
	(map binet (sequence 0 (sub1 n))))))


;; Chosen implementation for Fibonacci sequences
(define fibonacci
  (lambda (n)
    (fibonacci-fast n)))


;; Chosen implementation for Fibonacci numbers
(define fibonacci-number
  (lambda (n)
    (fibonacci-number-tail-recursive n)))


;; ROOT FINDING ###############################################################

(define illinois
  (lambda (f x1 x2 . args)
    (let ((n-iter (if (null? args)
		      1000
		      (car args)))
	  (tol (if (or (null? args)
		       (null? (cdr args)))
		   1e-16
		   (cadr args))))
      (let* ((f1 (f x1))
	     (f2 (f x2))
	     (xc (/ (- (* x1 f2) (/ (* x2 f1) 2))
		    (- f2 (/ f1 2))))
	     (fc (f xc)))
	(if (>= n-iter 1)
	    (if (> (abs fc) tol)
		(if (< (* (f xc) f2) 0)
		    (illinois f xc x2 (sub1 n-iter) tol)
		    (illinois f x1 xc (sub1 n-iter) tol))
		(cons (exact->inexact xc)
		      (cons n-iter
			    (cons (exact->inexact fc) '())))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRING MANIPULATION
;;            (REUSABLE)
;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-string?
  (lambda (x)
    (equal? x "")))


(define not-empty-string?
  (lambda (x)
    (not (empty-string? x))))


;; There is already string-join
(define merge-strings
  (lambda (strings . args)
    (let* ((merging-char (if (not (null? args))
			    (car args)
			    ""))
	   (tail (cdr strings))
	   (last-element? (equal? (cdr tail) '()))
	   (head (string-append (car strings)
				merging-char)))
      (string-append head
		     (if last-element?
			 (car tail)
			 (merge-strings tail merging-char))))))


(define char-in-string?
  (lambda (char string)
    (> (length (char-positions-in-string char string)) 0)))


(define raw-string->number
  (lambda (string)
    (string->number
     (if (char-in-string? #\tab
			  string)
	 (list->string (parse-line (if (char-in-string? #\newline)
				       (list->string (parse-line string
								 #\newline))
				       string)
				   #\tab))
	 string))))


(define my-substring?
  (lambda (a b)
    (let* ((a-length (string-length a))
	   (b-length (string-length b))
	   (test (> b-length a-length)))
      (if test
	  (if (equal? a (substring b 0 a-length))
	      #t
	      (substring? a (list->string (cdr (string->list b)))))
	  #f))))

(define case-insensitive-substring?
  (lambda (a b)
    (my-substring? (string-downcase a) (string-downcase b))))


(define string-alter
  (lambda (string alteration-function)
    (list->string (map alteration-function
                       (string->list string)))))


(define string-upcase
  (lambda (string)
    (string-alter string char-upcase)))


(define string-downcase
  (lambda (string)
    (string-alter string char-downcase)))


(define case-insensitive-substring?
  (lambda (a b)
    (substring? (string-downcase a)
                         (string-downcase b))))


(define split-string-by-indexes
  (lambda (string indexes . args)
    (let* ((splitter (car indexes))
	   (suppress-splitter (if (null? args)
				  #t
				  (car args)))
           (head (substring string
                            0
			    (if suppress-splitter
				splitter
				(+ splitter 1))))
           (tail (substring string
			    (+ splitter 1)
			    (string-length string))))
      (cons head
            (if (equal? (length indexes) 1)
                (cons tail '())
                (split-string-by-indexes tail
					 (map (lambda (x)
						(- x
						   (string-length head)
						   (if suppress-splitter
						       1
						       0)))
					      (cdr indexes))
					 suppress-splitter))))))


(define find-char-in-string
  (lambda (char string . args)
    (let* ((char-list (string->list string))
	   (list-length (length char-list))
	   (master-length (if (null? args)
			      list-length
			      (car args))))
      (cons (if (char=? (car char-list) char)
		(- master-length list-length))
	    (if (> list-length 1)
		(find-char-in-string char
				     (list->string (cdr char-list))
				     master-length)
		'())))))

(define char-positions-in-string
  (lambda (char string)
    (filter number?
	    (find-char-in-string char string))))


(define string-list->string
  (lambda (string-list . args)
    (cond ((null? string-list)
	   "")
	  ((pair? (car string-list))
	   (string-list->string
	    (map string-list->string string-list)))
	  (else
	   (if (null? args)
	       (set! args (car string-list)))
	   (string-append (if (equal? (car string-list)
				      args)
			      "("
			      " ")
			  (car string-list)
			  (string-list->string (cdr string-list)
					       args)
			  (if (equal? (cdr string-list)
				      '())
			      ")"
			      ""))))))



;;;;;;;;;;;;;;;;;;;;;;;
;; LIST MANIPULATION
;;          (REUSABLE)
;;;;;;;;;;;;;;;;;;;;;;;

(define improper-list?
  (lambda (x)
    (and (not (equal? (car x) '()))
	 (not (pair? (cdr x))))))


(define list?
  (lambda (x)
    (and (pair? x)
	 (not (improper-list? x)))))


(define transpose
  (lambda (structured-list)
    (apply map list structured-list)))


(define cons-cell->list
  (lambda (cell)
    (list (car cell)
	  (cdr cell))))


;; TODO Extend in order to deal with improper lists (cons cells)
(define flatten
  (lambda (input-list)
    (cond ((null? input-list) '())
          ((list? (car input-list))
           (append (flatten (car input-list))
                   (flatten (cdr input-list))))
          (else (cons (car input-list)
		      (flatten (cdr input-list)))))))

(define sublist
  (lambda (lst indexes)
    (if (pair? indexes)
        (cons (list-ref lst (car indexes))
              (sublist lst (cdr indexes)))
        '())))


(define slice
  (lambda (lst first . args)
    (let ((last (if (null? args)
                    (sub1 (length lst))
                    (sub1 (car args)))))
      (sublist lst
               (sequence first
                         last)))))


(define filter
  (lambda (test lst)
    (if (null? lst)
        '()
        (let ((present (car lst)))
          (append (if (test present)
                      (list present)
                      '())
                  (filter test (cdr lst)))))))


(define reduce
  (lambda (func lst)
    (let ((n (length lst)))
      (define aux
        (lambda (acc current-n)
          (if (equal? current-n
                      (sub1 n))
              acc
              (aux (func acc
                         (list-ref lst (add1 current-n)))
                   (add1 current-n)))))
      (cond ((equal? n 0) '())
            ((equal? n 1) lst)
            (else (aux (func (car lst)
                             (cadr lst))
                       1))))))


(define quick-sort
  (lambda (unsorted)
    (if (null? unsorted)
        '()
        (let* ((pivot (car unsorted))
               (rest (cdr unsorted))
               (head (filter (lambda (x) (< x pivot)) rest))
               (tail (filter (lambda (x) (>= x pivot)) rest)))
          (append (quick-sort head)
                  (list pivot)
                  (quick-sort tail))))))


(define sort
  (lambda (unsorted)
    (quick-sort unsorted)))



;;;;;;;;;;;;;;;;;;;;;;;;
;; VECTORS AND ARRAYS
;;           (REUSABLE)
;;;;;;;;;;;;;;;;;;;;;;;;

(define vector-append
  (lambda (vect ext)
    (let ((head (vector->list vect))
          (tail (if (number? ext)
                    (list ext)
                    (vector->list ext))))
      (list->vector (append head tail)))))


;; In the case of vectors it is reasonable to assume that only a contiguous
;; subset of its elements can be of any use
;; Already defined in CHICKEN - Fluent Scheme to be checked
(define subvector
  (lambda (vec first last)
    (list->vector (slice (vector->list vec)
                         first
                         last))))


(define array-ref
  (lambda (data index . args)
    (if (vector? data)
	(if (null? args)
            (vector-ref data index)
            (apply array-ref
		   (append (list (vector-ref data index))
                           (cons (car args)
                                 (cdr args)))))
        data)))


(define shape
  (lambda (data)
    (if (not (vector? data))
        '()
        (append (list (vector-length data))
                (shape (vector-ref data 0))))))


(define dimension
  (lambda (data)
    (length (shape data))))


(define matrix?
  (lambda (data)
    (and (vector? data)
	 (equal? (dimension data)
		 2))))


(define n-rows
  (lambda (data)
    (when (matrix? data)
      (car (shape data)))))


(define n-columns
  (lambda (data)
    (when (matrix? data)
      (cadr (shape data)))))


(define 1D?
  (lambda (data)
    (equal? (dimension data)
	    1)))


(define 2D?
  (lambda (data)
    (equal? (dimension data)
	    2)))


(define 3D?
  (lambda (data)
    (equal? (dimension data)
	    3)))


(define vector-scalar-product
  (lambda (vec1 vec2)
    (define aux
      (lambda (acc n)
	(if (< n (vector-length vec1))
	    (aux (+ acc (* (vector-ref vec1 n)
			   (vector-ref vec2 n)))
		 (add1 n))
	    acc)))
    (aux (* (vector-ref vec1 0)
	    (vector-ref vec2 0))
	 1)))


(define scalar-mult
  (lambda (num data)
    '()))


(define dot
  (lambda (data1 data2)
    (cond ((number? data1)
	   (scalar-mult data1 data2))
	  ((and (1D? data1)
	       (1D? data2)
	       (equal? (vector-length data1)
		       (vector-length data2)))
	   (vector-scalar-product data1 data2)))))



;;;;;;;;;;;;;;
;; BASE I/O
;; (REUSABLE)
;;;;;;;;;;;;;;

(define import-raw-text
  (lambda (file-name)
    (with-input-from-file file-name
	(lambda ()
	  (let reading ((chars '()))
	    (let ((char (read-char)))
	      (if (eof-object? char)
		  (reverse chars)
		  (reading (cons char chars)))))))))


(define to-file
  (lambda (output file-name mode)
    (let ((output-port (open-output-file file-name
					 mode)))
      (for-each (lambda (line)
		  (newline output-port)
		  (display line
			   output-port))
		output)
      (newline output-port)
      (close-output-port output-port))))


(define write-to-file
  (lambda (output file-name)
    (to-file output file-name "w")))


(define append-to-file
  (lambda (output file-name)
    (to-file output file-name "a")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA IMPORT / EXPORT / TRANSLATION
;;                           (REUSABLE)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse-line
  (lambda (line delimiter . args)
    (split-string-by-indexes line
			     (char-positions-in-string delimiter
						       line)
			     (if (null? args)
				 '()
				 (car args)))))


(define parse-csv-line
  (lambda (line)
    (parse-line line #\,)))


(define read-lines
  (lambda (file-name)
    (parse-line (list->string (import-raw-text file-name))
		#\newline)))
