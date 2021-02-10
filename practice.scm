(define (print expression) (display expression) (display "\n"))
(define separator "--------------------------------------------")
(define (header expression) (print expression) (print separator))
(define (content expression value expected) (display value) (display " | Value : ") (display value) (display " | Expected : ") (display expected) (display "\n"))
(define (footer) (print separator) (print "\n"))


; Function returning if the given list consist of all the elements as S-expression
(define lat?
  (lambda (l)
    (if (null? l)
        #t
        (and (atom? (car l)) (lat? (cdr l))))))
; (lat? '(bacon and egg))
; (lat? '())
; (lat? '(bacon and (egg)))


; Function returning if the parameter `a` found in the given list `lat`
(define member?
  (lambda (a lat)
    (if (null? lat) #f
        (or (eq? a (car lat)) (member? a (cdr lat))))))
; (member? 4 '(3 5 '(4) 4))
; (member? 4 '(3 5 (4 5)))


; Function returning a new list where parameter `a` vanished from list `lat`
(define rember?
  (lambda (a lat)
    (if (null? lat)
        (quote())
        (if (eq? (car lat) a)
            (rember? a (cdr lat))
            (cons (car lat) (rember? a (cdr lat)))))))
; (rember? 4 '(4 5 4 (5 4)))
; (rember? 'sauce '(soy sauce and tomato sauce))


; Function returning first item in the given list `lat`
(define first?
  (lambda (lat) (car lat)))
; (first? '(1 2 3))


; Function returning second item in the given list `lat`
(define second?
  (lambda (lat) (car (cdr lat))))
; (second? '(1 2 3))


; Function extracting list of first items of lists inside `lat`
(define firsts?
  (lambda (lat)
    (if (null? lat)
        (quote())
        (cons (first? (first? lat)) (firsts? (cdr lat))))))
; (firsts? '((1 2) (3 4) (5 6 7)))


; Function extracting list of second items of lists inside `lat`
(define seconds?
  (lambda (lat)
    (if (null? lat)
        (quote())
        (cons (second? (first? lat)) (seconds? (cdr lat))))))
; (seconds? '((1 2) (3 4) (5 6 7)))


; Function returning a list where `new` value is inserted right after the `old` value in the list `lat`
(define insertR
  (lambda (new old lat)
    (if (null? lat)
        (quote())
        (if (eq? old (car lat))
            (cons (car lat) (cons new (insertR new old (cdr lat))))
            (cons (car lat) (insertR new old (cdr lat)))))))
; (insertR 'e 'd '(a b c d f g h))


; Function returning a list where `new` value is inserted left before the `old` value in the list `lat`
(define insertL
  (lambda (new old lat)
    (if (null? lat)
        (quote())
        (if (eq? old (car lat))
            (cons new (cons (car lat) (insertL new old (cdr lat))))
            (cons (car lat) (insertL new old (cdr lat)))))))
; (insertL 'e 'd '(a b c d f g h))


; Function returning a list where `new` value is on top of `old` value. Note that, lists inside `lat` are not visited
(define subst
  (lambda (new old lat)
    (if (null? lat)
        (quote())
        (if (eq? (car lat) old)
            (cons new (subst new old (cdr lat)))
            (cons (car lat) (subst new old (cdr lat)))))))
; (subst 'topping 'fudge '(ice cream with fudge for dessert))
