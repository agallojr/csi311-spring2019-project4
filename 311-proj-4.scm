#lang racket 

(define team  '(("Emp1" (57 57 80 47 68 56 84 65 67 54))
                ("Emp2" (57 69 57 84 87 71 77 69 61 48))
                ("Emp3" (46 47 61 65 81 64 40 77 51 78))
                ("Emp4" (70 68 89 41 45 45 71 81 58 74))
                ("Emp5" (45 48 74 83 40 44 70 85 98 86))
                ))

(define (sum s)
 (cond ((null? s) 0)
       (else (+ (car s) (sum (cdr s))))))

(define (getSum s)
  (list (car s) (sum (car (cdr s)))))

(define (getEmpTotals s)
  (cond ((null? s) '())
        (else (cons (getSum (car s)) (getEmpTotals (cdr s))))))

(define (getTeamTotalWorker s)
  (cond ((null? s) 0)
        (else (+ (sum (cdr (car s))) (getTeamTotalWorker (cdr s))))))

(define (getTeamTotal s)
  (getTeamTotalWorker(getEmpTotals s)))

(getEmpTotals team)
(getTeamTotal team)


