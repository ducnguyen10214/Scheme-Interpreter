(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement

(define (zip pairs)
    (cons (map car pairs) (cons (map cadr pairs) nil))
)


;; Problem 16
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 16
  (define (helper s count)
    (if (null? s) 
      nil
      (cons (cons count (cons (car s) nil)) (helper (cdr s) (+ count 1)))
    )
  )
  (helper s 0)
)
  ; END PROBLEM 16

;; Problem 17

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 17  (8 6 5) (7 1 1) => (8 7 6 5 1 1)
  (cond 
    ((null? list1) list2)
    ((null? list2) list1)
    ((comp (car list1) (car list2))
      (cons (car list1) (merge comp (cdr list1) list2))
    )
    (else
      (cons (car list2) (merge comp list1 (cdr list2)))
    )
  )
)
  ; END PROBLEM 17


;; Problem 18

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (cons form (cons params (let-to-lambda body)))
           ; END PROBLEM 18
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
            (define zipped_list (zip values))
            (cons (cons 'lambda (cons (car zipped_list) (let-to-lambda body))) (let-to-lambda (cadr zipped_list)))
           ; END PROBLEM 18
           ))
        (else
         ; BEGIN PROBLEM 18
         (map let-to-lambda expr)
         ; END PROBLEM 18
         )))

