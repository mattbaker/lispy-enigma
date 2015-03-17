#lang racket

(provide (all-defined-out))

(define (clean-input text) (string-replace (string-normalize-spaces (string-upcase text)) " " ""))

