#lang scheme

; function f adds one to each element of the list. Use for questions 1-6.
(define (f lst)
; (a) ;
(if (null? lst)
; (b) ;
`()
; (c) ;
(cons (+ 1 (car lst)) (f (cdr lst)))))

; The memeber function determines if element e is part of the list lst.
(define (member? e lst)
  (cond
    
    [(null? lst) #f] ; automatically return false if the list is empty
    
    [(equal? e (car lst)) #t] ; find first element of list and check if it is equal to e
    
    [else (member? e (cdr lst))] ; otherwise, recursively call member with the remainder of the list
    
    )
)

; The set function checks if the list lst is a well formed set (no duplicates)
(define (set? lst)

  (cond
    
    [(null? lst) #t] ; if the list is empty, it is a set, return true.
    
    [(member? (car lst) (cdr list)) #f] ; use member function to check if this element occurs anywhere else in the list
    
    [else (set? (cdr lst))] ; otherwise perform recursive call to evaluate other members
    
    )
)

; The union function creates a mathematical union of tow lists
(define (union lst1 lst2)
  (cond
    
    [(null? lst2) lst1] ; simply return lst1 if lst2 is null
    
    ; If this member of lst2 is in lst1, call union with the remainder of the lst
    [(member? (car lst2) lst1) (union lst1 (cdr lst2))] 
    
    ; Otherwise combine this member with lst1 and make recursive call.
    [else (union (cons (car lst2) lst1) (cdr lst2))]
    
    )
)

; return mathematical intersection of two lists.
(define (intersect lst1 lst2)
  (cond
     ;returns empty list if lst2 is null
    [(null? lst2) '()]
    
    ;returns the first element of lst2 combined with a recursive call to intersect with lst1 and lst2(without its first element);
    [(member? (car lst2) lst1) (cons (car lst2) (intersect lst1 (cdr lst2)))]
    
    ; returns intersect called with lst1 and lst2(without its first element ;
    [else (intersect lst1 (cdr lst2))]
    
    )
)






