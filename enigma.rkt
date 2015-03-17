#lang racket
(require racket/include)
(require (prefix-in string: (file "string.rkt")))
(require (prefix-in rotor: (file "rotors.rkt")))

(define ns (make-base-namespace))

; Encode a letter given a reflector, three rotors, and the letter to be encoded
(define (encode-letter reflector rotor-l rotor-m rotor-r letter)
  (list-ref rotor:alphabet
    (rotor:translate-right rotor-r
      (rotor:translate-right rotor-m
        (rotor:translate-right rotor-l
          (rotor:reflect reflector
            (rotor:translate-left rotor-l
              (rotor:translate-left rotor-m
                (rotor:translate-left rotor-r
                  (rotor:letter-to-index letter))))))))))

;Encode a list of chars given a char list, reflector, and rotors
(define (encode-list cipher-list reflector rotor-l rotor-m rotor-r [encoded `()])
  (let
    ([rotated-rotor-l (rotor:rotate rotor-l (if (rotor:at-notch? rotor-m) 1 0))]
     [rotated-rotor-m (rotor:rotate rotor-m (if (or (rotor:at-notch? rotor-m) (rotor:at-notch? rotor-r)) 1 0))]
     [rotated-rotor-r (rotor:rotate rotor-r 1)])
    (if
      (empty? cipher-list)
      (list->string (reverse encoded))
      (encode-list (cdr cipher-list)
        reflector
        rotated-rotor-l
        rotated-rotor-m
        rotated-rotor-r
        (cons (encode-letter
               reflector
               rotated-rotor-l
               rotated-rotor-m
               rotated-rotor-r
               (car cipher-list))
              encoded)))))

; Encode a string given a string, reflector and rotors
(define (encode-string cipher-text reflector rotor-l rotor-m rotor-r)
  (encode-list (string->list (string:clean-input cipher-text)) reflector rotor-l rotor-m rotor-r))

; Test Plain
(equal? (encode-string
  "ENGIMA"
  rotor:reflector-b
  (rotor:rotor-i #\M)
  (rotor:rotor-ii #\C)
  (rotor:rotor-iii #\K)) "QMDGDO")

;Test Rotor Step
(equal? (encode-string
  "ENGIMA"
  rotor:reflector-b
  (rotor:rotor-i #\M)
  (rotor:rotor-ii #\A)
  (rotor:rotor-iii #\U)) "IFAMZT")

;Test Double Step
(equal? (encode-string
  "ENGIMA"
  rotor:reflector-b
  (rotor:rotor-i #\K)
  (rotor:rotor-ii #\D)
  (rotor:rotor-iii #\V)) "HGXEDI")


