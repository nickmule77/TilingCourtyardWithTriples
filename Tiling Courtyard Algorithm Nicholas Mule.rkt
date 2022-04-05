#lang Racket
;; Tiling The Courtyard Assignment
;; Author: Nicholas Mule
;; Pledge: I pledge to abide by the Stevens Honor System
;; To represent each square will be the coordinates of the bottom-right of the square, in a list. All coordinates will be in the format of squares of (+x, +y), as they will be in the I region of a X-Y graph.
;; For example, (0, 1) will be the bottom left coordinate of any graph of 2^n where n >= 2.
;; Each graph will have the same "hole" placement at (3, 2).
;; This program will use recursive logic to create the graphs hopefully

(define (hole)
  (list 3 2))

;;build main method using hole 


;;final function; Creates a grid of 2^n by 2^n
;;Type Signature -> (b-exp n st dir) -> n = int n, st = starting location, dir = direction
(define (build-grid n st dir)
 (cond[(<= n 2) 
  (cond
      [(= n 1) (b-exp st dir)]
      [(= n 2) (append (expand-each-square (caddr (build-grid 1 st dir)) dir) (build-grid (- n 1) st dir))]
      [(= n 3) (append (expand 2 (3-holes 2 st dir) dir) (build-grid (- n 1) st dir))]
      [else (append (expand (- n 1) (cdr (b-exp (findCorner (build-grid (- n 1) st dir) 2 (- n 1)) dir)) dir) (build-grid (- n 1) st dir))])]
   [else
    (append (cond
      [(= n 1) (b-exp st dir)]
      [(= n 2) (append (expand-each-square (caddr (build-grid 1 st dir)) dir) (build-grid (- n 1) st dir))]
      [(= n 3) (append (expand 2 (3-holes 2 st dir) dir) (build-grid (- n 1) st dir))]
      [else (append (expand (- n 1) (cdr (b-exp (findCorner (build-grid (- n 1) st dir) 2 (- n 1)) dir)) dir) (build-grid (- n 1) st dir))]) (build-cap st))]))
    


;; b-exp - short for base expand AKA create 2x2
;; Type Signature -> (b-exp st dir) -> st = starting point, dir = expansion in direction of coordinate graph regions, ex: 1 is up-right
(define (b-exp st dir) 
  (cond  
    [(= (modulo dir 4) 1) (append (list st) (list (list (car st) (+ 1 (cadr st))) (list (+ 1 (car st)) (+ 1 (cadr st))) (list (+ 1 (car st)) (cadr st))  ))]
    [(= (modulo dir 4) 2) (append (list st) (list (list (- (car st) 1) (cadr st)) (list (- (car st) 1) (+ 1 (cadr st))) (list (car st) (+ 1 (cadr st)))   ))]
    [(= (modulo dir 4) 3) (append (list st) (list (list (- (car st) 1) (cadr st)) (list (- (car st) 1) (- (cadr st) 1)) (list (car st) (- (cadr st) 1))   ))]
    [(= (modulo dir 4) 0)(append (list st) (list  (list (car st) (- (cadr st) 1)) (list (+ 1 (car st)) (- (cadr st) 1)) (list (+ 1 (car st)) (cadr st))  ))]))
    ;;order of expansion: left, diagonal, right
    ;;expand up-right = st x+1, st x+1 y+1, st y+1




;; expand - main recursive expansion function
;; Type Signature -> (expand n stL dir) -> n = number of grids to build to expand, stL = start location list, dir = direction to expand in
(define (expand n stL dir)
  (cond
       [(or (= (modulo dir 4) 1) (= (modulo dir 4) 2))
        (for/list ([s stL]
             [d (list (+ dir 1) dir (- dir 1))])
    (build-grid n s d))]
       [else
         (for/list ([s stL]
             [d (list (- dir 1) dir (+ dir 1))])
    (build-grid n s d))]))
 

;;expand-each-square, builds a 4x4 by doing a base-expand from an unrecorded base-expand in direction dir.
(define (expand-each-square st dir)
  (cond
       [(or (= (modulo dir 4) 1) (= (modulo dir 4) 2))
        (for/list ([s (cdr (b-exp st dir))]
             [d (list (+ dir 1) dir (- dir 1))])
    (b-exp s d))]
       [else
         (for/list ([s (cdr (b-exp st dir))]
             [d (list (- dir 1) dir (+ dir 1))])
    (b-exp s d))]))


;;cap - Build "cap" for other side of hole
(define (build-cap st) ;;now in the bottom left
 (no-last (build-grid 2 (list (+ 1 (car st))(+ (cadr st) 1)) 3)))

;;noLast - helper function for build cap, to remove repeated coordinates from starting hole
(define (no-last L)
  (cond
    [(null? (cddddr L)) '()]
    [else (cons (car L) (no-last (cdr L)))]))

;;3-holes - find three start points to treat as holes and allow expansion for grid n = 3; we want the "hole" which is 1 in from the diagonal, always first in the list 
(define (3-holes n st dir)
(map (lambda (list) (car list))(build-grid n st dir)))

  
;;Find the corner -> grid is grid to search through, dir = direction to look in (relative to grid), level = n of grid
(define (findCorner grid dir level)
  (cond
    [(= (modulo dir 3) 1) (dive (car grid) (- level 1))] ;;left = car grid
     [(= (modulo dir 3) 2) (dive (cadr grid) (- level 1))];; diagonal = cadr grid
     [(= (modulo dir 3) 0) (dive (caddr grid) (- level 1))]));;right = caddr grid

;;dive - helper function to findCorner -> keep going deeper until outer corner, which means cdr continously, then take third element in list
(define (dive grid level)
  (cond
    [(= level 1) (caddr grid)]
    [else (dive (cadr grid) (- level 1))]))

;;range - Create list of (a...b)
(define (range a b)
  (cond
    [(> a b) '()]
    [else (cons a (range (+ a 1) b))]))





