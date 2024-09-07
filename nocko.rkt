;;  nocko.rkt
;;  Scheme 2024
;;  Nock 4K in miniKanren
;;  Racket v8.12

#lang racket

(require minikanren)
(require minikanren/numbers)

(define (nouno i)
  (conde
   [(atomo i)]
   [(cello i)]))

(define (atomo i)
  (fresh (d)
	 (== `(nat . ,d) i)
	 (nato d)))

(define (nato i)
  (fresh (d)
	 (conde
	  [(== '(0) i)]
	  [(== '(1) i)]
	  [(== `(0 . ,d) i)
	   (=/= '(0) d)
	   (nato d)]
	  [(== `(1 . ,d) i)
	   (=/= '(0) d)
	   (nato d)])))
   
(define (cello i)
  (fresh (a d)
	 (== `(,a ,d) i)
	 (nouno a)
	 (nouno d)))

(define (nocko i o) 
  (fresh (a)
         (raso i a)
         (taro a o)))

(define (wuto i o)
  (fresh (a b)
	 (conde
	  [ (== `[,a ,b] i) (== 0 o) ]
	  [ (atomo i)       (== 1 o) ]
	  )))

(define (luso i o)
  (fresh (a b)
	 (conde
	  [ (== `[,a ,b] i) (== `[lus ,i] o) ]
	  [ (atomo i)       (nadd1o i o) ] 
	  )))

(define (tiso i o)
  (fresh (a b)
	 (conde
	  [ (== `[,a ,a] i)           (== 0 o) ]
	  [ (== `[,a ,b] i) (=/= a b) (== 1 o) ]
	  )))

(define (faso i o)
  (fresh (a b ind num res n)
	 (conde
	  [ (== `[(nat 1) ,a] i)        (== a o) ]
	  [ (== `[(nat 0 1) [,a ,b]] i) 
	    (nouno a) (nouno b)         (== a o) ] 
	  [ (== `[(nat 1 1) [,a ,b]] i) 
	    (nouno a) (nouno b)         (== b o) ]
	  [ (== `[,ind ,b] i)
	    (=/= '(nat 0) ind)
	    (=/= '(nat 0 1) ind)
	    (npluso a a ind)            (faso `[,a ,b] res) (faso `[(nat 0 1) ,res] o) ]
	  [ (== `[,ind ,b] i)
	    (=/= '(nat 1) ind)
	    (=/= '(nat 1 1) ind)
	    (npluso n '(nat 1) ind) 
	    (npluso a a n)              (faso `[,a ,b] res) (faso `[(nat 1 1) ,res] o) ]
	  )))

(define (haxo i o) 
  (fresh (a b ind c n res)
         (conde
          [ (== `[(nat 1) [,a ,b]] i) 
            (nouno a) (nouno b)                 (== a o) ]
          [ (== `[,ind [,b ,c]] i)
            (nouno b) (nouno c)
            (nadd1o ind n) (npluso a a ind) 
	    (haxo `[,a [[,b ,res] ,c]] o) (faso `[,n ,c] res) ] 
          [ (== `[,ind [,b ,c]] i)
            (nouno b) (nouno c)
            (nadd1o n ind) (npluso a a n) 
	    (haxo `[,a [[,b ,res] ,c]] o) (faso `[,n ,c] res) ]
          )))

(define (taro i o) 
  (fresh (a b c d resa resb resc)
         (conde
          [ (== `[,a [[,b ,c] ,d]] i) 
            (taro `[,a [,b ,c]] resa) 
            (taro `[,a ,d] resb)                                 (== `[,resa ,resb] o) ]

          [ (== `[,a [(nat 0) ,b]] i)                            (faso `[,b ,a] o) ]
          [ (== `[,a [(nat 1) ,b]] i)                            (== b o) ]
          [ (== `[,a [(nat 0 1) [,b ,c]]] i) 
            (taro `[,a ,b] resa)
            (taro `[,a ,c] resb)                                 (taro `[,resa ,resb] o) ]
          [ (== `[,a [(num 1 1) ,b]] i) 
            (taro `[,a ,b] resa)                                 (wuto resa o) ]
          [ (== `[,a [(num 0 0 1) ,b]] i)
            (taro `[,a ,b] resa)                                 (luso resa o) ]
          [ (== `[,a [(num 1 0 1) [,b ,c]]] i) 
            (taro `[,a ,b] resa)
            (taro `[,a ,c] resb)                                 (tiso `[,resa ,resb] o) ]

          [ (== `[,a [(num 0 1 1) [,b [,c ,d]]]] i) 
            (taro `[,a [(num 0 0 1) [num (0 0 1) ,b]]] resa)
            (taro `[[(num 0 1) (num 1 1)] [(num 0) ,resa]] resb)
            (taro `[[,c ,d] [(num 0) ,resb]] resc)
                                                                 (taro `[,a ,resc] o) ]
          [ (== `[,a [(num 1 1 1) [,b ,c]]] i) 
            (taro `[,a ,b] resa)                                 (taro `[,resa ,c] o) ]
          [ (== `[,a [(num 0 0 0 1) [,b ,c]]] i) 
            (taro `[,a ,b] resa)                                 (taro `[[,resa ,a] ,c] o) ]
          [ (== `[,a [(num 1 0 0 1) [,b ,c]]] i) 
            (taro `[,a ,c] resa)                                 (taro `[,resa [(num 0 1) [[(num 0) (num 1)] [(num 0) ,b]]]] o) ]
          [ (== `[,a [(num 0 1 0 1) [[,b ,c] ,d]]] i) 
            (taro `[,a ,c] resa)
            (taro `[,a ,d] resb)                                 (haxo `[,b [,resa ,resb]] o) ]
          
          [ (== `[,a [(num 1 1 0 1) [[,b ,c] ,d]]] i)
            (taro `[,a ,c] resa)
            (taro `[,a ,d] resb)                                 (taro `[[,resa ,resb] [(num 0) (num 3)]] o) ]
          [ (== `[,a [(num 1 1 0 1) [,b ,c]]] i)                 (taro `[,a ,c] o) ]
          )))

(define (numo n)
  (fresh (a b c)
         (conde
          [(== '(num ()) n)]
          [(== '(num (1)) n)]
          [(>1o a) (== `(num ,a) n)])))

(define (raso i o)
  (fresh (a b c d e resa resb resc resd)
         (conde
          [ (atomo i) (== i o) ] 
          [ (== `[,a ,b . ,c] i) 
            (== `(,d . ,e) c) (=/= '() e) 
	    (=/= 'nat d) 
            (raso a resa) (raso b resb) (raso c resc) 
            (== `[,resa [,resb ,resc]] o) 
            ]
          [ (== `[,a ,b . ,c] i) 
            (== `(,d . ()) c) 
	    (raso a resa) (raso b resb) (raso d resd)             
            (== `[,resa [,resb ,resd]] o) 
            ]
          [ (== `[,a ,b . ,c] i) 
            (== '() c) 
            (raso a resa) (raso b resb)
            (== `[,resa ,resb] o) ]
          )))

(define (npluso m n o) 
  (fresh (a b c)
	 (== `(nat . ,a) m) 
	 (== `(nat . ,b) n) 
	 (== `(nat . ,c) o) 
	 (conde
	  [(== '(0) a) (== '(0) b) (== '(0) c)]
	  [(== '(0) a) (=/= '(0) b) (pluso '() b c) (nato b)]
	  [(=/= '(0) a) (== '(0) b) (pluso a '() c) (nato a)]
	  [(=/= '(0) a) (=/= '(0) b) (pluso a b c) (nato a) (nato b)] 
	  )))

(define (nadd1o i o) (npluso i '(nat 1) o))

