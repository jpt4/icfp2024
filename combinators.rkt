;Naive strict application
;Is[trict] := [0 1] - correct for all variants
(define Is '[0 1])
;
;Obligatory curried application
;S := [[1 [1 2]] [[1 [0 1]] [[1 1] [0 1]]]]
(define Sc '[[1 [1 2]] [[1 [0 1]] [[1 1] [0 1]]]])

;K := [8 [[1 1] [0 1]]]
(define Kc '[8 [[1 1] [0 1]]])

;Adaptive partial application
;S := [[1 [1 2]] [[1 [0 1]] [[1 1] [0 1]]]]
(define S '[[1 [1 2]] [[1 [0 1]] [[1 1] [0 1]]]])

;K := [6 [[3 [0 1]] [[0 3] [[8 [[1 1] [0 1]]]]]]]
(define K '[6 [[3 [0 1]] [[0 3] [[8 [[1 1] [0 1]]]]]]])
