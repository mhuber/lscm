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
      
      
      