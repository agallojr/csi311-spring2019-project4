#lang racket 

(define (getNums ls)
  (if
   (null? ls) '()
   (cons (cadddr (car ls)) (getNums (cdr ls)))))

(define (timeSort x y)
   (< (car x) (car y)))

(define make-tswb
  (lambda ()
    (let ((ls '()))
      (lambda (msg . args)
        (cond
         ((eqv? msg 'empty?)   (null? ls))
         ((eqv? msg 'clear!)   (set! ls '()))
         ((eqv? msg 'add!)     (set! ls (cons (car args) ls)))
         ((eqv? msg 'get)      (if (null? args) (sort ls timeSort) (sort (filter (car args) ls) timeSort)))
         ((eqv? msg 'analytic) ((car args) (if (null? (cdr args)) (getNums (reverse ls)) (getNums (filter (cadr args) ls)))))
          (else "oops"))))))




(define (sum ls)
  (if
    (null? ls) 0
    (+ (car ls) (sum (cdr ls)))))

(define (average ns) (/ (apply + ns) (length ns)))


(define (std-dev lst)
    (sqrt
        (/  
            (innerFunction lst (average lst))
            (length lst)
        )
    )
)
(define (innerFunction lst avg)
  (if (null? lst)
      0
      (+  (* (- (car lst) avg)
             (- (car lst) avg))
          (innerFunction (cdr lst) avg)))) ; here was the error

(define (minimum ls)
  (apply min ls))


(define (maximum ls)
  (apply max ls))


(define (count ls)
  (length ls))



; -----------------------------------------------

(define tswb (make-tswb))                                                ; make a new time series workbench 

(tswb 'empty?)                                                           ; is the db empty?
(tswb 'add!     '(2 123 "temp1"  72.1))                                  ; add some data - notice time=2 - we will sort later
(tswb 'add!     '(1 123 "temp1"  72.0))                                  ; add some data, each is a list of 4 values
(tswb 'add!     '(2 123 "press1" 29.9213))                               ; 
(tswb 'add!     '(1 123 "press1" 29.9212))                               ;        - time 
(tswb 'add!     '(1 456 "temp1"  87.3))                                  ;        - device #
(tswb 'add!     '(1 456 "temp2"  87.4))                                  ;        - field name 
(tswb 'add!     '(1 456 "press1" 28.9234))                               ;        - field value 
(tswb 'empty?)
(tswb 'get)                                                              ; return all records in time order
(tswb 'get      (lambda (l) (eqv? (cadr l) 456)))                        ; return all the records for device 456 in time order
(tswb 'get      (lambda (l) (eqv? (caddr l) "temp1")))                   ; return all the temp1 fields for all devices, time ordered
(define (temp1-123 l)                                                    ; define a filter 
  (and (eqv? (cadr l) 123) (eqv? (caddr l) "temp1")))                    
(tswb 'get      temp1-123)                                               ; get all the temp1 fields for device 123
(tswb 'analytic sum     temp1-123)                                       ; run some analytics against the data from this filter 
(tswb 'analytic average temp1-123) 
(tswb 'analytic std-dev temp1-123)
(tswb 'analytic minimum temp1-123)
(tswb 'analytic maximum temp1-123)
(tswb 'analytic count   temp1-123)
(tswb 'analytic sum)                                                     ; run the analytic against all data, no filter 
(tswb 'clear!)                                                           ; clear the db 
(tswb 'empty?)


















