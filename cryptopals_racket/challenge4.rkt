#lang racket
;; Challenge 4
;; Detect single-character XOR
;; One of the 60-character strings in this file has been encrypted by single-character XOR.
;; Find it.
;; (Your code from #3 should help.)

(require "challenge1.rkt"
         "challenge2.rkt"
         "challenge3.rkt"
         rackunit)
(provide detectxor)



(define (detectxor file)
(max_score2 (filter (lambda (x) (not-utf8 (first x)))
   (map
    (lambda (line) (list 
                         
                    (first (max_score (single_char_xor (hex->ascii line))) )
                     line))
    (file->bytes-lines file)) )))

( check-equal? (detectxor "4.txt") '(#"Now that the party is jumping\n" #"7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f"))



