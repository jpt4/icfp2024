;;  nocksche.rkt
;;  Scheme 2024
;;  Nock 4K in Racket
;;  Racket v8.12

#lang racket
(require racket/match)
(require string-interpolation)

(define (noun n)              
  (or (atom n) (cell n)))     

(define (atom a)              
  (natural? a))               

(define (cell c)              
  (match c
    [`(,a ,b)                 
     (and (noun a) (noun b))] 
    [_ #f]))

(define (ras a)
  (match a
   [(? noun) a]
   [`(,x ,y . ,z)
    #:when (not (null? z))
    `[,(ras (car a)) ,(ras (cdr a))]
    ]
   [`(,x ,y . ,z)
    #:when (null? z)
    `[,(ras (car a)) ,(ras (cadr a))]
    ]
   [_ 'error-not-a-noun]
   ))

(define (nock a) 
  (tar (ras a)))

(define (wut a)
  (match a
    [ `[,a ,b] 0 ] 
    [ (? atom) 1 ]))

(define (lus a)
  (match a
    [ `[,a ,b] (string->symbol "+@{`[,a ,b]}") ]
    [ (? atom) (+ 1 a) ]))

(define (tis a)
  (match a
    [ `[,a ,a] #:when (atom a)                                   0 ] 
    [ `[,a ,b] #:when (and (atom a) (atom b) (not (equal? a b))) 1 ]))
  
(define (fas a)
  (match a
    [ `[1 ,a]                       a ]
    [ `[2 [,a ,b]]                  a ]
    [ `[3 [,a ,b]]                  b ]
    [ `[,a ,b] 
      #:when 
      (and (even? a) (> a 2))       (fas `[2 ,(fas `[,(/ a 2) ,b])]) ] 
    [ `[,a ,b]
      #:when (and (odd? a) (> a 3)) (fas `[3 ,(fas `[,(/ (- a 1) 2) ,b])]) ] 
    [ (? nexp)                      (string->symbol "/@{a}")]))

(define (hax a)
  (match a
    [ `[1 [,a ,b]]                   a ]
    [ `[,a [,b ,c]] 
      #:when (and (even? a) (> 1 a)) (hax `[,a [[,b ,(fas `[,(+ a 1) ,c])] ,c]]) ]
    [ `[,a [,b ,c]] 
      #:when (and (odd? a) (> 1 a))  (hax `[,a [[,(fas `[,(- a 1) ,c]) ,b] ,c]]) ]
    [ (? nexp)                       (string->symbol "#@{a}") ]))

(define (tar a) 
  (match a 
    [ `[,a [[,b ,c] ,d]]      `[,(tar `[,a [,b ,c]]) ,(tar `[,a ,d])] ] 
    [ `[,a [0 ,b]]            (fas `[,b ,a]) ]
    [ `[,a [1 ,b]]            b ] 
    [ `[,a [2 [,b ,c]]]       (tar `[,(tar `[,a ,b]) ,(tar `[,a ,c])]) ]
    [ `[,a [3 ,b]]            (wut (tar `[,a ,b])) ]
    [ `[,a [4 ,b]]            (lus (tar `[,a ,b])) ]
    [ `[,a [5 [,b ,c]]]       (tis `[,(tar `[,a ,b]) ,(tar `[,a ,c])]) ]
    [ `[,a [6 [,b [,c ,d]]]]  (tar `[,a ,(tar `[[,c ,d] [0 ,(tar `[[2 3] [0 ,(tar `[,a [4 [4 ,b]]])]])]])]) ]
    [ `[,a [7 [,b ,c]]]       (tar `[,(tar `[,a ,b]) ,c]) ]
    [ `[,a [8 [,b ,c]]]       (tar `[[,(tar `[,a ,b]) ,a] ,c]) ]
    [ `[,a [9 [,b ,c]]]       (tar `[,(tar `[,a ,c]) [2 [[0 1] [0 ,b]]]]) ]
    [ `[,a [10 [[,b ,c] ,d]]] (hax `[,b [,(tar `[,a ,c]) ,(tar `[,a ,d])]]) ]
    [ `[,a [11 [[,b ,c] ,d]]] (tar `[[,(tar `[,a ,c]) ,(tar `[,a ,d])] [0 3]]) ]
    [ `[,a [11 [,b ,c]]]      (tar `[,a ,c]) ]
    [ (? nexp)                (string->symbol "*@{a}")]))
