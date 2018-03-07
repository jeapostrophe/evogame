#lang racket/base
(require racket/flonum
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
  (define input-vec (make-flvector 16))
  (define output-vec (make-flvector 8))
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

      ;; A     =  1 x 16
      ;;     B = 16 x 8
      ;; A * B =  1 x 8
      (vecmat-mult! 16 8 input-vec ai-matrix output-vec)

      (for/fold ([spot #f]
                 [spot-score 0]
                 #:return (cons who-am-i spot))
                ([i (in-naturals)]
                 [x (in-flvector output-vec)])
        (if (< spot-score x)
          (values i x)
          (values spot spot-score))))))

(module+ test
  (tic-tac-toe tic-tac-toe-interactive
               tic-tac-toe-interactive))
