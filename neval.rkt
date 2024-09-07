;;  neval.rkt
;;  Scheme 2024
;;  Nock 4K in Racket
;;  Racket v8.12

#lang racket
(require racket/match)

(define (noun n)              
  (or (atom n) (cell n)))     

(define (atom a)              
  (natural? a))               

(define (cell c)              
  (match c
    [`(,a ,b)                 
     (and (noun a) (noun b))] 
    [_ #f]))

(define nops '(nock wut lus tis fas hax tar)) 

(define (nop n) (member n nops))

(define (nexp e)
  (match e
    [`(,n ,a)
     #:when (and (nop n) (nexp a))
     #t]
    [(? noun) #t]
    [_ #f]))

(define (ras-nir a) 
  (match a
    [(? nexp) a]
    [`(,x ,y . ,z)
     #:when (and (nop x) (not (null? z)))
     `[,x ,(ras-nir (cdr a))]
     ]
    [`(,x ,y . ,z)
     #:when (and (nop x) (null? z))
     `[,x ,(ras-nir (cadr a))]
     ]
    [`(,x ,y . ,z)
    #:when (not (null? z))
    `[,(ras-nir (car a)) ,(ras-nir (cdr a))]
    ]
    [`(,x ,y . ,z)
     #:when (null? z)
     `[,(ras-nir (car a)) ,(ras-nir (cadr a))]
     ]
    [_ 'error-not-a-nexp]))

(define (neval n)
  (match n
    [ `(nock ,a) #:when (noun a) (neval `(tar ,a)) ]
    [ a #:when (not (nexp a))    (neval `,(ras-nir a)) ] 

    [ `(wut [,a ,b])                 0 ]
    [ `(wut ,a)                      1 ]
    [ `(lus [,a ,b])                 (neval `(lus [,a ,b]))  ] 
    [ `(lus ,a)                      (+ 1 a) ]
    [ `(tis [,a ,a])                 0 ] 
    [ `(tis [,a ,b])                 1 ]

    [ `(fas [1 ,a])                  a ]
    [ `(fas [2 [,a ,b]])             a ]
    [ `(fas [3 [,a ,b]])             b ]
    [ `(fas [,a ,b]) 
      #:when (and (even? a) (> a 2)) (neval `(fas [2 ,(neval `(fas [,(/ a 2) ,b]))])) ]
    [ `(fas [,a ,b]) 
      #:when (and (odd? a) (> a 3))  (neval `(fas [3 ,(neval `(fas [,(/ (- a 1) 2) ,b]))])) ]
    [ `(fas ,a)                      (neval `(fas ,a)) ]
    
    [ `(hax [1 [,a ,b]])             a ]
    [ `(hax [,a [,b ,c]]) 
      #:when (and (even? a) (> a 1)) (neval `(hax [,(/ a 2) [[,b ,(neval `(fas [,(+ a 1) ,c]))] ,c]])) ]
    [ `(hax [,a [,b ,c]]) 
      #:when (and (odd? a) (> a 2))  (neval `(hax [,(/ (- a 1) 2) [[,(neval `(fas [,(- a 1) ,c])) ,b] ,c]])) ]
    [ `(hax ,a)                      (neval `(hax ,a)) ]

    [ `(tar [,a [[,b ,c] ,d]])       `[,(neval `(tar [,a [,b ,c]])) ,(neval `(tar [,a ,d]))] ]
    
    [ `(tar [,a [0 ,b]])             (neval `(fas [,b ,a])) ]
    [ `(tar [,a [1 ,b]])             b ]
    [ `(tar [,a [2 [,b ,c]]])        (neval `(tar [,(neval `(tar [,a ,b])) ,(neval `(tar [,a ,c]))])) ]
    [ `(tar [,a [3 ,b]])             (neval `(wut ,(neval `(tar [,a ,b])))) ]
    [ `(tar [,a [4 ,b]])             (neval `(lus ,(neval `(tar [,a ,b])))) ]
    [ `(tar [,a [5 [,b ,c]]])        (neval `(tis [,(neval `(tar [,a ,b])) ,(neval `(tar [,a ,c]))])) ]
    
    [ `(tar [,a [6 [,b [,c ,d]]]])   (neval `(tar [,a ,(neval `(tar [[,c ,d] [0 ,(neval `(tar [[2 3] [0 ,(neval `(tar [,a [4 [4 ,b]]]))]]))]]))])) ]
    [ `(tar [,a [7 [,b ,c]]])        (neval `(tar [,(neval `(tar [,a ,b])) ,c])) ]
    [ `(tar [,a [8 [,b ,c]]])        (neval `(tar [[,(neval `(tar [,a ,b])) ,a] ,c])) ]
    [ `(tar [,a [9 [,b ,c]]])        (neval `(tar [,(neval `(tar [,a ,c])) [2 [[0 1] [0 ,b]]]])) ]
    [ `(tar [,a [10 [[,b ,c] ,d]]])  (neval `(hax [,b [,(neval `(tar [,a ,c])) ,(neval `(tar [,a ,d]))]])) ]
    
    [ `(tar [,a [11 [[,b ,c] ,d]]])  (neval `(tar [[,(neval `(tar [,a ,c])) ,(neval `(tar [,a ,d]))] [0 3]])) ]
    [ `(tar [,a [11 [,b ,c]]])       (neval `(tar [,a ,c])) ]
    
    [ (? nexp)                       (neval `(tar ,n)) ]    
    ))
