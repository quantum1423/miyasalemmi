#lang racket
(define-struct word (eng mys dml cls ety))
(require "io.scm")

(define dict (map (lambda (x)
                    (match x
                      [`(word ,eng ,mys ,dml ,cls ,ety)
                       (word eng mys dml cls ety)]))
                  (read (open-input-file "dict.dat"))))

(define ccomm #f)

(define (dispdict ctr fltr)
  (newline)
  (define intv 30)
  (display "[ID]\t")
  (pprint "ENGLISH" intv)
  (pprint "TAMAISSAN" intv)
  ;(pprint "DÜMLYAGHZHIC" intv)
  (pprint "CLASS" intv)
  ;(pprint "ETYMOLOGY" intv)
  (newline)(newline)
  (for-each (λ(wrd)
              (when (fltr wrd)
                (printf "~a\t" ctr)
                (pprint (word-eng wrd) intv)
                (pprint (word-mys wrd) intv)
                ;(pprint (word-dml wrd) intv)
                (display (word-cls wrd))
                (newline))
              (set! ctr (add1 ctr)))
            dict) (newline))

(define (sublist? a b) ;is a a sublist of b?
  (cond
   [(empty? a) #t]
   [(empty? b) #f]
   [(equal? (car a) (car b)) (sublist? (cdr a) (cdr b))]
   [else (sublist? a (cdr b))]))

(define (substring? a b) ;is a a substring of b?
  (sublist? (string->list a) (string->list b)))

(define (sublist lst a b)
  (take (list-tail lst a) (add1 (- b a))))

(define (newword)
  (read-line)
  (display "English: ")
  (define eng (read-line))
  (display "Miyasan: ")
  (define tms (read-line))
  ;(display "Dümlyaghzhic: ")
  (define dml "TBD")
  (display "Word class: ")
  (define cls (read-line))
  ;(display "Etymology: ")
  ;(define ety (read-line))
  (word eng tms dml cls ""))

(define (sync)
  (sleep 1)
  (when ccomm
    (display-to-file "(" "dict.dat" #:exists 'replace)
    (map (λ(x)
           (match x
             [(struct word (a b c d e))
              (write-to-file `(word ,a ,b ,c ,d ,e) "dict.dat" #:exists 'append)
              (display-to-file "\n" "dict.dat" #:exists 'append)])) dict)
    (display-to-file ")" "dict.dat" #:exists 'append)
    (set! ccomm #f))
  (sync))

(define (rpl)
  (display "clm> ")
  (let [(cmd (read))]
    (match cmd
      [`(ddict) (dispdict 0 identity)]
      [`(ddict mys)
       (set! dict
             (sort dict (λ(a b) (string<? (word-mys a)
                                          (word-mys b)))))
       (dispdict 0 identity)
       (set! ccomm #t)]
      [`(ddict ,a ,b)
       (let [(backup dict)]
         (set! dict (sublist dict a b))
         (dispdict a identity)
         (set! dict backup))]
      [`(ddict eng)
       (set! dict
             (sort dict (λ(a b) (string<? (word-eng a)
                                          (word-eng b)))))
       (dispdict 0 identity)
       (set! ccomm #t)]
      [`(ddict dml)
       (set! dict
             (sort dict (λ(a b) (string<? (word-dml a)
                                          (word-dml b)))))
       (dispdict 0 identity)
       (set! ccomm #t)]
      
      [`(edit ,num)
       (set! dict
             (append* (list
                       (take dict num)
                       (list (newword))
                       (list-tail dict (add1 num)))))
       (set! ccomm #t)]
      [`(newword)
       (set! dict
             (cons (newword) dict))
       (set! ccomm #t)]
      [`(search ,wrd)
       (let [(backup dict) (q (symbol->string wrd))]
         (dispdict 0 (lambda (x)
                         (match x
                           [(struct word (a b c d e)) (or
                                                       (substring? q a)
                                                       (substring? q b)
                                                       (substring? q c))])))
         (set! dict backup))]
      [`(exit) (exit)]
      [x (display "Malformed query: ") (displayln x)]
      
      ))
  (rpl)
  )
(void (thread (λ() (sync))))
(rpl)
