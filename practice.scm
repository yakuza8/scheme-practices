(define (print expression) (display expression) (display "\n"))
(define separator "-------------------------------------------------")
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


; Function returning incremented value of the given parameter
(define (add1 n) (+ n 1))
;(add1 5)


; Function returning decremented value of the given parameter
(define (sub1 n) (- n 1))
;(sub1 5)


; Function checking the given number `n` is equal to 1 (one)
(define (one? n) (zero? (sub1 n)))
; (one? 2)


; Function that takes two tuples of numbers as parameter and sums all the corresponding numbers with the same index
; Note that if the length of tuples differ then it will evaluate the longest length tuple and zip the shorter one with zero
(define tup+
  (lambda (tup1 tup2)
    (if (and (null? tup1) (null? tup2))
        (quote())
        (if (null? tup1)
            tup2
            (if (null? tup2)
                tup1
                (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))))
; (tup+ '() '())
; (tup+ '(1 2 3) '(5 6))
; (tup+ '(1) '(5 6))
; (tup+ '(15 73 88) '(5 7 2))


; Function returning the length of list
(define length?
  (lambda (lat)
    (if (null? lat) 0 (+ 1 (length? (cdr lat))))))
; (length? '())
; (length? '(1 2 3))
; (length? '(1 (1 2 3 4) 3))


; Function returning the value at the given index. Note that index is 1 based
(define pick
  (lambda (n lat)
    (if (zero? (sub1 n))
        (car lat)
        (pick (sub1 n) (cdr lat)))))
; (pick 1 '(2))
; (pick 1 '(4 5 6 7))
; (pick 3 '(4 5 6 7))


; Function returning a list where the value is removed at index `n`. Note that index is 1 based
(define rempick
  (lambda (n lat)
    (if (one? n)
        (cdr lat)
        (cons (car lat) (rempick (sub1 n) (cdr lat))))))
; (rempick 1 '(2))
; (rempick 1 '(4 5 6 7))
; (rempick 3 '(4 5 6 7))


; Function returning the list composed of non-number elements of list `lat`
(define no-nums
  (lambda (lat)
    (if (null? lat)
        (quote())
        (if (number? (car lat))
            (no-nums (cdr lat))
            (cons (car lat) (no-nums (cdr lat)))))))
; (no-nums '(5 pears 6 prunes 9 dates))


; Function returning the list composed of number elements of list `lat`
(define all-nums
  (lambda (lat)
    (if (null? lat)
        (quote())
        (if (number? (car lat))
            (cons (car lat) (all-nums (cdr lat)))
            (all-nums (cdr lat))))))
; (all-nums '(5 pears 6 prunes 9 dates))


; Function returning the number of occurrences of atom `a` inside list `lat`
(define occur
  (lambda (a lat)
    (if (null? lat)
        0
        (if (eq? (car lat) a)
            (add1 (occur a (cdr lat)))
            (occur a (cdr lat))))))
; (occur 1 '(1 2 1 4 5 1 4 (1 2 3)))


; Function returning a list purified from atom `a` from list `lat`
(define rember*
  (lambda (a lat)
    (if (null? lat)
        (quote())
        (if (atom? (car lat))
            (if (eq? a (car lat))
                (rember* a (cdr lat))
                (cons (car lat) (rember* a (cdr lat))))
            (cons (rember* a (car lat)) (rember* a (cdr lat)))))))
; (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
; (rember* 'sauce ' (((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))


; Function returning a list where we place parameter `new` nearby right of parameter `old` of list `lat`
(define insertR*
  (lambda (new old l)
    (if (null? l)
        (quote())
        (if (atom? (car l))
            (if (eq? old (car l))
                (cons old (cons new (insertR* new old (cdr l))))
                (cons (car l) (insertR* new old (cdr l))))
            (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))
; (insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))


; Function returning a list where we place parameter `new` nearby left of parameter `old` of list `lat`
(define insertL*
  (lambda (new old l)
    (if (null? l)
        (quote())
        (if (atom? (car l))
            (if (eq? old (car l))
                (cons new (cons old (insertL* new old (cdr l))))
                (cons (car l) (insertL* new old (cdr l))))
            (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))
; (insertL* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))


; Function returning the count of atom `a` inside list `l`by searching deeply
(define occur*
  (lambda (a l)
    (if (null? l)
        0
        (if (atom? (car l))
            (if (eq? a (car l))
                (add1 (occur* a (cdr l)))
                (occur* a (cdr l)))
            (+ (occur* a (car l)) (occur* a (cdr l)))))))
; (occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
; (occur* 'split '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))


; Function substituting a parameter `new` onto parameter `old` inside list `l` by replacing all of the occurence deeply
(define subst*
  (lambda (new old l)
    (if (null? l)
        (quote())
        (if (atom? (car l))
            (if (eq? old (car l))
                (cons new (subst* new old (cdr l)))
                (cons (car l) (subst* new old (cdr l))))
            (cons (subst* new old (car l)) (subst* new old (cdr l)))))))
; (subst* 'orange 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))


; Function returning a parameter `a` found in list `l`by searching deeply
(define member*
  (lambda (a l)
    (if (null? l)
        #f
        (if (atom? (car l))
            (or (eq? a (car l)) (member* a (cdr l)))
            (or (member* a (car l)) (member* a (cdr l)))))))
; (member* 'chips '((potato) (chips ((with) fish) (chips))))


;-------------------- Shadows --------------------;
(define eval-expression
  (lambda (first-operand-process second-operand-process operator-process expression)
    (if (number? expression)
        expression
        (if (eq? '+ (operator-process expression))
            (+ (eval-expression first-operand-process second-operand-process operator-process (first-operand-process expression)) (eval-expression first-operand-process second-operand-process operator-process (second-operand-process expression)))
            (if (eq? '* (operator-process expression))
                (* (eval-expression first-operand-process second-operand-process operator-process (first-operand-process expression)) (eval-expression first-operand-process second-operand-process operator-process (second-operand-process expression)))
                (expt (eval-expression first-operand-process second-operand-process operator-process (first-operand-process expression)) (eval-expression first-operand-process second-operand-process operator-process (second-operand-process expression))))))))

; Prefix operator operand1 and operand2 definition with prefix evaluation of expression
(define (prefix-operator expression) (car expression))
(define (prefix-first-operand expression) (car (cdr expression)))
(define (prefix-second-operand expression) (car (cdr (cdr expression))))
(define (eval-prefix expression) (eval-expression prefix-first-operand prefix-second-operand prefix-operator expression))
; (eval-prefix '(+ (* 3 6) (! 8 2)))


; Infix operator operand1 and operand2 definition with infix evaluation of expression
(define (infix-operator expression) (car (cdr expression)))
(define (infix-first-operand expression) (car expression))
(define (infix-second-operand expression) (car (cdr (cdr expression))))
(define (eval-infix expression) (eval-expression infix-first-operand infix-second-operand infix-operator expression))
; (eval-infix '((3 * 6) + (8 ! 2)))


; Postfix operator operand1 and operand2 definition with postfix evaluation of expression
(define (postfix-operator expression) (car (cdr (cdr expression))))
(define (postfix-first-operand expression) (car expression))
(define (postfix-second-operand expression) (car (cdr expression)))
(define (eval-postfix expression) (eval-expression postfix-first-operand postfix-second-operand postfix-operator expression))
; (eval-postfix '((3 6 *) (8 2 !) +))


;--------------- Friends and Relations ---------------;
; Function returning whether the given list holds being set properties i.e. each element occurs once in the list
(define set?
  (lambda (lat)
    (if (null? lat)
        #t
        (and (not (member? (car lat) (cdr lat))) (set? (cdr lat))))))
; (set? '())
; (set? '(apple peaches apple plum))
; (set? '(apples peaches pears plums))
; (set? '(apple 3 pear 4 9 apple 3 4))


; Function returning set from a list in the given order of elements
(define makeset
  (lambda (lat)
    (if (null? lat)
        (quote())
        (if (member? (car lat) (cdr lat))
            (cons (car lat) (rember? (car lat) (makeset (cdr lat))))
            (cons (car lat) (makeset (cdr lat)))))))
; (makeset '(apple peach pear peach plum apple lemon peach))
; (makeset '(apple 3 pear 4 9 apple 3 4))


; Function returning boolean parameter representing the given `set1` is subset of `set2`
(define subset
  (lambda (set1 set2)
    (if (null? set1)
        #t
        (and (member? (car set1) set2) (subset (cdr set1) set2)))))
; (subset '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
; (subset '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))


; Function returning whether the given two sets are equal or not
(define (eqset? set1 set2) (and (subset set1 set2) (subset set2 set1)))
; (eqset? '(6 large chickens with wings) '(6 chickens with large wings))
