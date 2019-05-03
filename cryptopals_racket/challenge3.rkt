#lang racket
;; Challenge 3
;; Single-byte XOR cipher

;; The hex encoded string:
;; 1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
;;  has been XOR'd against a single character. Find the key, decrypt the message.
;; You can do this by hand. But don't: write code to do it for you.
;; How? Devise some method for "scoring" a piece of English plaintext. Character frequency is a good metric. Evaluate each output and choose the one with the best score.

(require "challenge1.rkt"
         "challenge2.rkt"
         rackunit)

(provide single_char_xor
         max_score
         not-utf8
         max_score2)
 
(define char_freq  (make-immutable-hash (list (list #\a 8.167)
 (list #\b 1.492)
(list #\c 2.782)
(list #\d 4.253)
(list #\e 12.7)
(list #\f 2.22)
(list #\g 2.015)
(list #\h 6.094)
(list #\i 6.966)
(list #\j 0.153)
(list #\k 0.772)
(list #\l 4.025)
(list #\m 2.406)
(list #\n 6.749)
(list #\o 7.507)
(list #\p 1.929)
(list #\q 0.095)
(list #\r 5.987)
(list #\s 6.327)
(list #\t 9.056)
(list #\u 2.758)
(list #\v 0.978)
(list #\w 2.360)
(list #\x 0.150)
(list #\y 1.974)
(list #\z 0.074)
(list #\ 13))))

(define (single_char_xor bytestring1)
  (map (lambda (x) (list (list->bytes (map (lambda (y) (bitwise-xor y x)) (bytes->list bytestring1))) x)) (build-list 256 (lambda (z) z)) ))

(define (score bytestring1)
  (cond
    [ (empty? (string->list bytestring1)) 0]
    [ (hash-has-key? char_freq (first (string->list  bytestring1))) (+ (first (hash-ref char_freq (first (string->list bytestring1))))
                                                                     (score (list->string (rest  (string->list bytestring1)))))]
    [else (score (list->string (rest (string->list bytestring1))))]))



(define (not-utf8 bytestring)
  (not-utf8/list (bytes->list bytestring)))

(define (not-utf8/list bytelist)
  (cond
    [(empty? bytelist) #t]
    [
          (> (first bytelist) 127)
     #f]
    [else (not-utf8/list (rest bytelist))]))

 ;; max_score: ASL -> (list bytestring Nat)
(define ( max_score asl)
  (max_score/acc  (rest asl) (first asl)))

(define (max_score/acc asl maxsofar)
    (cond
      [(empty? asl) maxsofar]
      [(not (not-utf8 (first (first asl)))) (max_score/acc (rest asl) maxsofar)]
      [ ( >  (score (string-downcase (bytes->string/utf-8
         (first (first asl))))) (score (string-downcase (bytes->string/utf-8 (first maxsofar))))) (max_score/acc (rest asl) (first asl))]
      [else (max_score/acc (rest asl) maxsofar)]))
      




(check-equal? (max_score (single_char_xor (hex->ascii
                  #"1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))) '(#"Cooking MC's like a pound of bacon" 88))

(define ( max_score2 asl)
  (max_score/acc2  (rest asl) (first asl)))
(define (max_score/acc2 asl maxsofar)
    (cond
      [(empty? asl) maxsofar]
      [ ( >  (score (string-downcase (bytes->string/utf-8
         (first (first asl))))) 
                                (score (string-downcase (bytes->string/utf-8 (first maxsofar))))) (max_score/acc2 (rest asl) (first asl))]
      [else (max_score/acc2 (rest asl) maxsofar)]))
      




( check-equal? (max_score2 (filter (lambda (x) (not-utf8 (first x))) (single_char_xor (hex->ascii
                  #"1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")))) '(#"Cooking MC's like a pound of bacon" 88))







      








    
  



  












