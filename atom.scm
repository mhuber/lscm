#lang r5rs

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

;;(define l (list "ad" "sab" "loo"))

;;(atom? l)

;; p. 79

(define one?
  (lambda (n)
    (= n 1)))

(define sub1
  (lambda (a)
    (- a 1)))

(define rempick
  (lambda (n lat)
    (cond 
      ((one? n)(cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))

;; p. 81

(define l (list ( list ( list "coffee") "cup" (list ( list "tea") "cup") (list "and" (list "hick")) "cup")))

(define rember*
  (lambda (a l)
    (cond
      ((null? l)(quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

;;(rember* "cup" l)                          

;; p. 85

(define add1
  (lambda (n)
    (+ n 1)))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l))
               (occur* a (cdr l)))))))

;; p. 85 cntd.

(define subst*
  (lambda (new old l)
    (cond
      ((null? l)(quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new
                (subst* new old (cdr l))))
         (else (cons (car l)
                     (subst* new old (cdr l))))))
      (else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))))))

;; (subst* "glass" "cup" l)

;; p. 86

(define insertl*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new
                (cons old
                      (insertl* new old (cdr l)))))
         (else
          (cons (car l)
                (insertl* new old (cdr l))))))
      (else (cons (insertl* new old (car l))
                  (insertl* new old (cdr l)))))))

;; (insertl* "full" "cup" l)

;; p. 87

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a)
           (member* a (cdr l))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))

;; p. 88

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
                
;; p. 91

; helper functions

(define equan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1)(number? a2))
       (= a1 a2))
      ((or (number? a1)(number? a2))
       #f)
      (else (eq? a1 a2)))))

; eqlist?

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))
    
; equal? p. 92

(define equal?
  (lambda (s1 s2)
    (cond 
      ((and (atom? s1) (atom? s2))
       (equan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

; sample data

(define l1 (list 'a 2 (list 3 'b '())))
(define l2 (list 'a 2 (list 3 'b '())))

; (eqlist? l1 l2)


;; 7. Friends and Relations p. 111

; helper

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a)
                (member? a (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

; sample data
(define lat2 (list 'apple 'plum 'pear 'peach 'pear))
(define lat3 (list 'apple 3 'pear 4 3 9 'pear 'pear))

;(set? lat2)

;; p. 112

(define makeset
 (lambda (lat)
   (cond
     ((null? lat) (quote ()))
     ((member? (car lat) (cdr lat))
      (makeset (cdr lat)))
     (else
      (cons (car lat) (makeset (cdr lat)))))))

; sample dat
(define lat4 (list 'apple 'peach 'pear 'peach 'plum 'apple 'lemon 'peach))

; helper

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((equal? (car lat) a)
          (multirember a (cdr lat)))
         (else (cons (car lat)
                     (multirember a (cdr lat)))))))))

(define makeset2
 (lambda (lat)
   (cond
     ((null? lat) (quote()))
     (else
     (cons (car lat)
           (multirember (car lat)(makeset2 (cdr lat))))))))

;; p. 113

; sample data
(define set1 (list 5 'chicken 'wings))
(define set2 (list 5 'hamburgers 2 'pieces 'fried 'chicken 'and 'light 'duckling 'wings))

(define subset?
  (lambda (set1 set2)
    (cond 
      ((null? set1) #t)
      (else
       (cond
         ((and (member? (car set1) set2)
            (subset? (cdr set1) set2)))
         (else #f))))))
      
;; p. 132

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))
;; p. 133

(define yyy
  (lambda (a l)
    ((insert-g segrem) #f a l))) ;; #f damit ersatzlos gelöscht wird

(define segrem
  (lambda (new old l)
    l))

(yyy 'pear lat2)

