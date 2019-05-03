#lang racket

(require "challenge5.rkt"
         rackunit)
(provide hamming)

;; a way of calculating hamming distance is to xor the
;; bytes and then count the 1's.

(define (listofhamming str1 str2)
  (hamming/list (bytes->list str1) (bytes->list str2)))

(define (hamming/list lst1 lst2)
  (cond
    [ (empty? lst1) empty]
    [else (cons  (bitwise-xor  (first lst1)  (first lst2) )
               (hamming/list (rest lst1) (rest lst2)))]))

(define (hamming bstr1 bstr2)
  (hamming2/list (listofhamming bstr1 bstr2)))

(define (hamming2/list lst)
  (cond
    [(empty? lst) 0]
    [else (+ (count-1 (string->list (number->string (first lst) 2)))
            (hamming2/list (rest lst)))]))

(define (count-1 lst)
  (cond
    [(empty? lst) 0]
    [(char=? (first lst) #\1) (+ 1 (count-1 (rest lst)))]
    [else (count-1 (rest lst))]))
  

(check-equal? (hamming #"this is a test" #"wokka wokka!!!") 37)



  
  
  
  


