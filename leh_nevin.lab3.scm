#lang scheme
(define (f lst)
  ; (a) ;
  (if (null? lst)
      ; (b) ;
      '()
      ; (c) ;
      (cons (+ 1 (car lst)) (f (cdr lst)))))


(define (member? e lst)
  (cond 
    ; condition returns false if the list is empty
    [(null? lst) #f]
    ;  checks if the first element in the list is the element we are searching for. ;
    ; If so return true;
    [(equal? e (car lst)) #t ]
    ;  calls member again with the first element of the list removed ;
    [else (member? e (cdr lst))]))


(define (set? lst)
 (cond
   ; returns true if the lst is empty:
   [(null? lst) #t]
   ; checks if the first element of the lst is anywhere else in the list ;
   ; if so return false ;
   [(member? (car lst) (cdr lst)) #f]
   ;calls set? again without the first element of the list ;
   [else (set? (cdr lst))]))


(define (union lst1 lst2)
  (cond
    ; returns lst1 if lst2 is null l;
    [(null? lst2) lst1]
    ;if the first element of lst2 is in lst1 call union with lst1 and lst2(without its first element) ;
    [(member? (car lst2) lst1) (union lst1 (cdr lst2))]
    ; call union with list1 combined with the first element of lst2 and lst2(without its first element) :
    [else (union (cons (car lst2) lst1) (cdr lst2 ))]))


(define (intersect lst1 lst2)
  (cond
     ;returns empty list if lst2 is null
    [(null? lst2) '()]
    ;returns the first element of lst2 combined with a recursive call to intersect with lst1 and lst2(without its first element);
    [(member? (car lst2) lst1) (cons (car lst2) (intersect lst1 (cdr lst2)))]
    ; returns intersect called with lst1 and lst2(without its first element ;
    [else (intersect lst1 (cdr lst2))]))




































