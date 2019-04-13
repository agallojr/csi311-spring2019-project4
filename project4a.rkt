; Andrew Pulver
(define team  '(("Emp1" (57 57 80 47 68 56 84 65))
                ("Emp2" (57 69 57 84 87 71 77 69 61 48))
                ("Emp3" (46 47 61 65 81 64 40 77 51 78))
                ("Emp4" (70 68 89 41))
                ("Emp5" (45 48 74 83 40 44 70 85 98 86))))


(define (sum-indiv teamlist)
  (map (lambda (x) (list (car x) (foldl + 0 (cadr x)))) teamlist))

(define (sum-total teamlist)
  (foldl + 0 (map cadr (sum-indiv teamlist))))


(println (sum-indiv team))
(println (sum-total team))
