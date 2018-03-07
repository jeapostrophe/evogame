#lang racket/base
(require racket/flonum
         racket/fixnum
         racket/list
         racket/match)

(define-syntax-rule (while test . body)
  (let loop () (when test (begin . body) (loop))))
(define-syntax-rule (until test . body)
  (while (not test) . body))
(define-syntax-rule (swap! x y)
  (let ([tmp y])
    (set! y x)
    (set! x tmp)))

(define (tic-tac-toe player1 player2)
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
  (until (game-complete?)
    (define player-move #f)
    (until (and player-move (valid-move? player-move))
      (set! player-move (active-player board)))
    (evaluate-move! player-move)
    (swap! active-player inactive-player))
  (game-complete?))

(define tic-tac-toe-interactive
  (λ (who-am-i)
    (local-require raart)
    (define label-table
      (table (text-rows '((0 1 2) (3 4 5) (6 7 8)))))
    (λ (board)
      (define (display-board)
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

      (display-board)
      (let loop ()
        (printf "~a: " who-am-i)
        (flush-output)
        (match (read)
          [(? number? n) (cons who-am-i n)]
          [_ (loop)])))))

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

(define (tic-tac-toe-ai ai-matrix)
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
                 [spot-score 0]
                 #:result (cons who-am-i spot))
                ([i (in-naturals)]
                 [x (in-flvector output-vec)])
        (if (and (not (vector-ref board i))
                 (< spot-score x))
          (values i x)
          (values spot spot-score))))))

(define equal-ai-matrix
  (make-flvector (* 18 9) (/ 1.0 18.0)))
(define (random-ai-matrix)
  (define AI (make-flvector (* 18 9) 0.0))
  (define prng (current-pseudo-random-generator))
  (for ([i (in-naturals)] [x (in-flvector AI)])
    (flvector-set! AI i (flrandom prng)))
  AI)

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
      (eprintf "Generation ~a\n" gen)
      ;; XXX change to partial sort
      (define sorted-population (fitness population))
      (define-values (fit unfit) (split-at sorted-population retain-count))
      ;; XXX fuse shuffle-take
      (define surviving-unfit (take (shuffle unfit) survive-count))
      ;; XXX fuse shuffle split-at
      (define surviving (shuffle (append fit surviving-unfit)))
      (define-values (will-mutate wont-mutate) (split-at surviving mutate-count))
      (define mutated (map mutate will-mutate))
      (define parents (append mutated wont-mutate))
      (define children
        (for/list ([i (in-range number-of-children)])
          (define male-idx (random parent-count))
          (define female-idx (random-but-not male-idx parent-count))
          (define male (list-ref parents male-idx))
          (define female (list-ref parents female-idx))
          (breed male female)))
      (append parents children)))
  ;; XXX change to partial sort
  (first (fitness final-population)))

;; XXX doesn't seem like enough changes
(define (mutate-ai-matrix! B)
  (define prng (current-pseudo-random-generator))
  (flvector-set! B (random (* 18 9)) (flrandom prng))
  B)

(define (ai-matrix-breed B1 B2)
  (define B3 (make-flvector (* 18 9)))
  (for ([i (in-naturals)] [m (in-flvector B1)] [f (in-flvector B2)])
    (flvector-set! B3 i (if (zero? (random 2)) m f)))
  B3)

(define (tic-tac-toe/score P1d P2d)
  (define P1 (tic-tac-toe-ai P1d))
  (define P2 (tic-tac-toe-ai P2d))
  (+ (match (tic-tac-toe P1 P2)
       ['O 2]
       ['Tie 1]
       ['X 0])
     (match (tic-tac-toe P2 P1)
       ['O 0]
       ['Tie 1]
       ['X 2])))
(define ((tournament game) players)
  (map car
       (sort
        (let loop ([previous-players '()]
                   [players players])
          (match players
            ['() '()]
            [(cons P1 future-players)
             (define score
               (+ (for/sum ([p (in-list previous-players)]) (game P1 p))
                  (for/sum ([p (in-list future-players)]) (game P1 p))))
             (cons (cons P1 score)
                   (loop (cons P1 previous-players) future-players))]))
        >= #:key cdr)))

(module+ test
  (define evolved-ai-matrix
    (evolve 100 20
            0.2 0.05 0.01
            random-ai-matrix
            (tournament tic-tac-toe/score)
            mutate-ai-matrix! ai-matrix-breed))
  evolved-ai-matrix

  (tic-tac-toe (tic-tac-toe-ai evolved-ai-matrix)
               tic-tac-toe-interactive))
