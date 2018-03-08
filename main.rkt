#lang racket/base
(require racket/flonum
         racket/fixnum
         racket/list
         racket/match
         data/heap
         raart)

(define-syntax-rule (while test . body)
  (let loop () (when test (begin . body) (loop))))
(define-syntax-rule (until test . body)
  (while (not test) . body))
(define-syntax-rule (swap! x y)
  (let ([tmp y])
    (set! y x)
    (set! x tmp)))

(define (tic-tac-toe player1 player2 #:display? [display? #f])
  (define board (make-vector 9 #f))
  (define (same? x y z)
    (and (eq? x y) (eq? y z) x))
  (define (check x y z)
    (same? (vector-ref board x)
           (vector-ref board y)
           (vector-ref board z)))
  (define (check-row s)
    (check (+ s 0) (+ s 1) (+ s 2)))
  (define (check-col s)
    (check (+ s 0) (+ s 3) (+ s 6)))
  (define (game-complete?)
    (or (and (for/and ([x (in-vector board)]) x)
             'Tie)
        (check-row 0) (check-row 3) (check-row 6)
        (check-col 0) (check-col 1) (check-col 2)
        (check 0 4 8) (check 6 4 2)))

  (define (valid-move? m)
    (match-define (cons who cell) m)
    (not (vector-ref board cell)))
  (define (evaluate-move! m)
    (match-define (cons who cell) m)
    (vector-set! board cell who))

  (define active-player (player1 'O))
  (define inactive-player (player2 'X))
  (when display? (display-board board))
  (until (game-complete?)
    (define player-move #f)
    (until (and player-move (valid-move? player-move))
      (set! player-move (active-player board)))
    (evaluate-move! player-move)
    (when display? (display-board board))
    (swap! active-player inactive-player))
  (game-complete?))

(define label-table
  (table (text-rows '((0 1 2) (3 4 5) (6 7 8)))))
(define (display-board board)
  (define (draw-row start)
    (for/list ([i (in-range start (+ start 3))])
      (char (match (vector-ref board i)
              [#f #\space]
              ['O #\O]
              ['X #\X]))))
  (draw-here
   (happend
    (table
     (list (draw-row 0)
           (draw-row 3)
           (draw-row 6)))
    (blank 3 7)
    label-table)))

(define tic-tac-toe-interactive
  (λ (who-am-i)
    (λ (board)
      (display-board board)
      (let loop ()
        (printf "~a: " who-am-i)
        (flush-output)
        (match (read)
          [(? number? n) (cons who-am-i n)]
          [_ (loop)])))))

;; Tree-based AI

;; Language inspired by --- http://www.ijcte.org/papers/799-T098.pdf
;; --- It has other ideas about how to run the tournament efficiently
;; (essentially sample the opponents rather than play everyone)

;; This paper ---
;; https://www.jair.org/media/1338/live-1338-2278-jair.pdf --- could
;; be interesting to implement.

(struct ai ())
(struct :or2 ai (l r))
(struct :and2 ai (l r))
(struct :X ai (pos))
(struct :O ai (pos))
(struct :! ai (pos))

(define (ai-eval me opp board ai-expr)
  (let loop ([ai-expr ai-expr])
    (match ai-expr
      [(:or2 l r)
       (or (loop l) (loop r))]
      [(:and2 l r)
       (define lv (loop l))
       (and lv
            (let ([rv (loop r)])
              (and rv
                   (if (number? rv) rv lv))))]
      [(:X pos)
       (eq? (vector-ref board pos) opp)]
      [(:O pos)
       (eq? (vector-ref board pos) me)]
      [(:! pos)
       (and (not (vector-ref board pos)) pos)])))

(define (binary->nary add)
  (λ args
    (define (loop args)
      (match args
        [(list one) one]
        [(cons one more)
         (add one (loop more))]))
    (loop args)))

(define :or (binary->nary :or2))
(define :and (binary->nary :and2))
(define (:check :X x y z)
  (:or (:and (:! x) (:X y) (:X z))
       (:and (:X x) (:! y) (:X z))
       (:and (:X x) (:X y) (:! z))))

(define (:block-row s) (:block (+ s 0) (+ s 1) (+ s 2)))
(define (:block-col s) (:block (+ s 0) (+ s 3) (+ s 6)))
(define (:block x y z) (:check :X x y z))
(define (:win-row s) (:win (+ s 0) (+ s 1) (+ s 2)))
(define (:win-col s) (:win (+ s 0) (+ s 3) (+ s 6)))
(define (:win x y z) (:check :O x y z))
(define (:or-if x y z)
  (:and (:X x) (:or (:! y) (:! z))))
(define (:opposite-corner x y)
  (:or (:and (:X x) (:! y))
       (:and (:! x) (:X y))))
(define optimal-ai
  (:or
   ;; Win
   (:win-row 0) (:win-row 3) (:win-row 6)
   (:win-col 0) (:win-col 1) (:win-col 2)
   (:win 0 4 8) (:win 6 4 2)
   ;; Block
   (:block-row 0) (:block-row 3) (:block-row 6)
   (:block-col 0) (:block-col 1) (:block-col 2)
   (:block 0 4 8) (:block 6 4 2)
   ;; XXX Fork
   ;; XXX Block fork
   ;; Center
   (:! 4)
   ;; Opposite corner
   (:opposite-corner 0 8) (:opposite-corner 2 6)
   ;; Empty Corner
   (:! 0) (:! 2) (:! 6) (:! 8)
   ;; Empty Side
   (:! 1) (:! 3) (:! 5) (:! 7)))

(define (tic-tac-toe-tree-ai ai-expr)
  (λ (who-am-i)
    (define opp (match who-am-i ['X 'O] ['O 'X]))
    (λ (board)
      (cons who-am-i (ai-eval who-am-i opp board ai-expr)))))

(define (tic-tac-toe-script script)
  (λ (who-am-i)
    (λ (board)
      (define next (first script))
      (set! script (rest script))
      (cons who-am-i next))))

(module+ test
  #;(tic-tac-toe (tic-tac-toe-tree-ai optimal-ai)
                 (tic-tac-toe-tree-ai optimal-ai)
                 #:display? #t)
  #;(tic-tac-toe tic-tac-toe-interactive
                 (tic-tac-toe-tree-ai optimal-ai))
  (begin (tic-tac-toe (tic-tac-toe-script
                       '(2 6 8 5)
                       ;; Tested
                       #;'(4 8 5 2)
                       #;'(4 8 2 6)
                       #;'(4 6 7 1))
                      (tic-tac-toe-tree-ai optimal-ai)
                      #:display? #t)
         (exit 1)))

;; Simple Neural Network AI

(define (vecmat-mult! m p A B C)
  (unless (= (flvector-length A) m)
    (error 'vecmat-multiply "A is wrong size"))
  (unless (= (flvector-length C) p)
    (error 'vecmat-multiply "A is wrong size"))
  (for ([j (in-range p)])
    (flvector-set!
     C j
     (for/sum ([a (in-flvector A)]
               [b (in-flvector B (fx* j m))])
       (fl* a b)))))

(define (tic-tac-toe-matrix-ai ai-matrix)
  (define input-vec (make-flvector 18))
  (define output-vec (make-flvector 9))
  (λ (who-am-i)
    (λ (board)
      ;; 0-8 are "spot is open"
      (for ([i (in-range 0 9)]
            [x (in-vector board)])
        (flvector-set! input-vec i (if (not x) 1.0 0.0)))
      ;; 9-17 are "spot is me"
      (for ([i (in-range 0 9)]
            [x (in-vector board)])
        (flvector-set! input-vec (+ 9 i) (if (eq? x who-am-i) 1.0 0.0)))

      (vecmat-mult! 18 9 input-vec ai-matrix output-vec)

      (for/fold ([spot #f]
                 [spot-score -inf.0]
                 #:result (cons who-am-i spot))
                ([i (in-naturals)]
                 [x (in-flvector output-vec)])
        (if (and (not (vector-ref board i))
                 (< spot-score x))
          (values i x)
          (values spot spot-score))))))

(define (random-but-not used total)
  (define next (random total))
  (if (= next used)
    (random-but-not used total)
    next))
(define (evolve population-size how-many-generations
                retain% survive% mutate%
                spontaneous-generation
                fitness mutate breed)
  (define retain-count (inexact->exact (round (* population-size retain%))))
  (define mutate-count (inexact->exact (round (* population-size mutate%))))
  (define survive-count (inexact->exact (round (* population-size survive%))))
  (define parent-count (+ retain-count survive-count))
  (define number-of-children (- population-size parent-count))
  (define final-population
    (for/fold ([population
                (build-list population-size (λ (_) (spontaneous-generation)))])
              ([gen (in-range how-many-generations)])
      (define-values (fit unfit) (fitness retain-count population))
      (define surviving-unfit (take (shuffle unfit) survive-count))
      (define-values (will-mutate wont-mutate)
        (split-at surviving-unfit mutate-count))
      (define mutated (map mutate will-mutate))
      (define parents (append fit mutated wont-mutate))
      (define children
        (for/list ([i (in-range number-of-children)])
          (define male-idx (random parent-count))
          (define female-idx (random-but-not male-idx parent-count))
          (define male (list-ref parents male-idx))
          (define female (list-ref parents female-idx))
          (breed male female)))
      (append parents children)))
  (define-values (best others) (fitness 1 final-population))
  (first best))

(define (lerp x y %)
  (fl+ x (fl* % (fl- y x))))

(define (random-ai-matrix-cell)
  (define prng (current-pseudo-random-generator))
  (flrandom prng))

(define (random-ai-matrix)
  (define AI (make-flvector (* 18 9) 0.0))
  (for ([i (in-naturals)] [x (in-flvector AI)])
    (flvector-set! AI i (random-ai-matrix-cell)))
  AI)

(define (mutate-ai-matrix! B)
  (define prng (current-pseudo-random-generator))
  (for ([i (in-range (random 10))])
    (define i (random (* 18 9)))
    (define old (flvector-ref B i))
    (flvector-set! B i (fl* old (fl- (fl* 0.5 (flrandom prng)) 0.5))))
  B)

(define (ai-matrix-breed B1 B2)
  (define prng (current-pseudo-random-generator))
  (define B3 (make-flvector (* 18 9)))
  (for ([i (in-naturals)] [m (in-flvector B1)] [f (in-flvector B2)])
    (flvector-set! B3 i (lerp m f (flrandom prng))))
  B3)

;; P1 loses to P2 if does not tie or if does not win both games
(define (tic-tac-toe<=? P1d P2d)
  (define P1 (tic-tac-toe-matrix-ai P1d))
  (define P2 (tic-tac-toe-matrix-ai P2d))
  (match (cons (tic-tac-toe P1 P2) (tic-tac-toe P2 P1))
    [(or (cons 'O 'X)
         (cons 'Tie 'Tie))
     #f]
    [_ #t]))

(define ((tournament game<=?) how-many players)
  (define h (make-heap game<=?))
  (define-values (first-k-players rest-players) (split-at players how-many))
  (for ([p (in-list first-k-players)])
    (heap-add! h p))
  (define left-overs '())
  (for ([p (in-list rest-players)])
    (heap-add! h p)
    (set! left-overs (cons (heap-min h) left-overs))
    (heap-remove-min! h))
  (values (vector->list (heap->vector h))
          left-overs))

(module+ test
  (define (test-evolve)
    (define evolved-ai-matrix
      (evolve 128 50
              0.2 0.10 0.10
              random-ai-matrix
              (tournament tic-tac-toe<=?)
              mutate-ai-matrix! ai-matrix-breed))
    (match
        (tic-tac-toe (tic-tac-toe-matrix-ai evolved-ai-matrix)
                     (tic-tac-toe-tree-ai optimal-ai))
      ['O
       (newline)
       (tic-tac-toe (tic-tac-toe-matrix-ai evolved-ai-matrix)
                    (tic-tac-toe-tree-ai optimal-ai)
                    #:display? #t)
       (exit 1)]
      ['Tie 'S]
      ['X 'F]))

  (for/fold ([r (hasheq)]) ([i (in-range 80)])
    (define e (test-evolve))
    (display e) (flush-output)
    (hash-update r e add1 0)))
