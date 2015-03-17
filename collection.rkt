#lang racket

(provide (all-defined-out))

; zip lists, e.g. (zip `(1 2) `(3 4)) ;-> `(`(1 3) `(2 4))
(define (zip . xs) (apply map list xs))

; Given a list and an item, return the index of that item in the list
(define (item-position list item [offset 0])
  (if (empty? list)
      -1
      (if (equal? item (car list))
          offset
          (item-position (cdr list) item (+ offset 1)))))

; Find a struct in a list where the value accessed by `accessor` is equal to `value`
(define (find-item-by list value accessor) (findf (lambda (tuple) (eq? value (accessor tuple))) list))