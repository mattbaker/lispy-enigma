#lang racket

;; Assumption: Not dealing with ring settings (though this wouldn't be hard)

(require (prefix-in collection: (file "collection.rkt")))

(provide (all-defined-out))

; Wheel constants
(define alphabet    (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define wheel-i     (string->list "EKMFLGDQVZNTOWYHXUSPAIBRCJ"))
(define wheel-ii    (string->list "AJDKSIRUXBLHWTMCQGZNPYFVOE"))
(define wheel-iii   (string->list "BDFHJLCPRTXVZNYEIWGAKMUSQO"))
(define reflector-a (string->list "EJMZALYXVBWFCRQUONTSPIKHGD"))
(define reflector-b (string->list "YRUHQSLDPXNGOKMIEBFZCWVJAT"))
(define reflector-c (string->list "FVPJIAOYEDRZXWGCTKUQSBNMHL"))

; Generate list of 26 boolean values, "true" at the notch position in the alphabet
(define (notch-list notch-letter) (map (curry eq? notch-letter) alphabet))

; Rotate a list by an offset mod 26
(define (rotate list offset)
  (let ([mod-offset (modulo offset 26)])
    (append (list-tail list mod-offset) (take list mod-offset))))

; Translate a char to its 0-based position in the alphabet
(define (letter-to-index letter) (collection:item-position alphabet letter))

; Rotate a list to the position in the alphabet of the supplied letter
(define (rotate-to-position wheel letter) (rotate wheel (letter-to-index letter)))

; Return whether or not the rotor is at a notch position (position 0 of a rotor is the "window" in Enigma terms)
(define (at-notch? rotor) (eq? (third (car rotor)) true))

; Produce a rotor given a wheel, notch position and ground
(define (rotor wheel notch ground)
  (rotate-to-position
   (collection:zip
    alphabet
    wheel
    (notch-list notch))
   ground))

; Given a reflector and a position, "reflect" the input position to an output position
(define (reflect reflector position) (collection:item-position alphabet (list-ref reflector position)))

; Translate the position of a source letter into a destination position across a given rotor (e.g. intra-rotor mapping)
(define (translate rotor position source destination)
  (collection:item-position rotor (collection:find-item-by rotor (source (list-ref rotor position)) destination)))

; Translate an input from the right side of a wheel to the left side
(define translate-left (curryr translate second first))

; Translate an input from the left side of wheel to the right side
(define translate-right (curryr translate first second))

; Define the Enigma rotors I, I and III with their notch positions
; These return new functions that, given a ground, return a list of correctly aligned rows
(define rotor-i (curry rotor wheel-i #\Q))
(define rotor-ii (curry rotor wheel-ii #\E))
(define rotor-iii (curry rotor wheel-iii #\V))
