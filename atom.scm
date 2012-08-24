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
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else
       (cond
         ((member? (car lat) (cdr lat)) #f)
       (else (set? (cdr l))))))))

; sample data
(define lat2 (list 'apple 'plum 'pear 'peach 'apple))

;(set? lat2)

