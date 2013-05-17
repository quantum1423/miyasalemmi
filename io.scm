#lang racket

(define (len obj)
  (cond
    [(number? obj) (len (number->string obj))]
    [(symbol? obj) (len (symbol->string obj))]
    [(string? obj) (len (string->list obj))]
    [(list? obj) (length obj)]
    [else (error "len: not a number, symbol, string or list:" obj)]))

(define (pprint obj space)
  (let [(ol (len obj))]
    (when (>= ol space) (error "pprint: obj does not fit in space"))
    (display obj)
    (for [(i (- space (len obj)))]
      (display " "))))

(provide len pprint)
