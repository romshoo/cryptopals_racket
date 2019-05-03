; Challenge 2
;; Fixed XOR
#lang racket/base
(require "challenge1.rkt"
         rackunit)
(provide xorstring)



; xorstring : bytes bytes -> bytes
; XOR two byte strings
(define (xorstring bstr1 bstr2)
  (list->bytes
   (map bitwise-xor
        (bytes->list bstr1)
        (bytes->list bstr2))))



 ; Challenge 2 solution
  (define bstr1 (hex->ascii #"1c0111001f010100061a024b53535009181c"))
  (define bstr2 (hex->ascii #"686974207468652062756c6c277320657965"))

  
     (check-equal? (ascii->hex (xorstring bstr1 bstr2))
                #"746865206b696420646f6e277420706c6179")

