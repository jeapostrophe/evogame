#lang racket/base
(require racket/match)

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
    (λ (board)
      (local-require raart)
      (define label-table
        (table (text-rows '((0 1 2) (3 4 5) (6 7 8)))))
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

(module+ test
  (tic-tac-toe tic-tac-toe-interactive
               tic-tac-toe-interactive))
