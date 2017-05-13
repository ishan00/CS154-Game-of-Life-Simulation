#lang racket
(require rsound)
(require racket/gui)
(require "main.rkt")
(require "test-cases-square.rkt")
(require "test-cases-hexagon.rkt")
(define mn (rs-read "hell.wav"))
(define xi 10)
(define yi 10)
(define size 16)
(define cols 50)
(define rows 40)

(define (larger-board)
  (begin
    (hex-to-square)
    (set! rows 160)
    (set! cols 200)
    (set! size 4)))

(define (smaller-board)
  (begin
    (hex-to-square)
    (set! rows 40)
    (set! cols 50)
    (set! size 16)))

(define (hexagonal-board)
  (begin
    (square-to-hex)
    (2d-vector-set! default-law 0 3 0)
    (2d-vector-set! default-law 1 2 0)
    (2d-vector-set! default-law 0 2 1)
    (2d-vector-set! default-law 1 3 1)
    (2d-vector-set! default-law 1 4 1)
    (set! matrix-initial (hex-matrix))
    (set! rows 30)
    (set! cols 17)
    (set! size 15)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SONG:::
(define play-state #f)
(define (play-now x)
  
  (if play-state (void) (begin (set! play-state #t) (play mn))))
(define (stop-now)
  
  (if play-state (begin (set! play-state #f) (stop)) (void) ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax for
  (syntax-rules (:)
    [(for init : condition : step : statements ...)
     (begin
       init
       (define (iter)
         (cond [condition (begin statements ... step (iter))]))
       (iter))]))
(define matrix-initial (make-hash))
(define (hex-matrix) (build-vector 30 (lambda(i) (build-vector 17 (lambda(j) (point i j 0))))))
(define main-canvas%
  (class canvas%
    (define/override (on-event event)
      (let ((x (if (eq? 'left-down (send event get-event-type)) (send event get-x) 0))
            (y (if (eq? 'left-down (send event get-event-type)) (send event get-y) 0)))
        (cond [(and (< x 463) (> x 165) (< y 247) (> y 209)) (begin (send main_frame show #f)
                                                                    (draw-matrix matrix-initial)
                                                                    (send simulation_frame show #t))]
              [(and (< x 489) (> x 119) (< y 312) (> y 274)) (begin (send main_frame show #f)
                                                                    (send instructions_frame show #t))]
              [(and (< x 425) (> x 192) (< y 386) (> y 349)) (begin (send main_frame show #f)
                                                                    (send settings_frame show #t))]
              [(and (< x 487) (> x 136) (< y 457) (> y 422)) (begin (send main_frame show #f)
                                                                    (send team_frame show #t))]
              [(and (< x 425) (> x 228) (< y 531) (> y 494)) (begin (send main_frame show #f)
                                                                    (send credits_frame show #t))])))
    (super-new)))

(define settings-canvas%
  (class canvas%
    (define/override (on-event event)
      (let ((x (if (eq? 'left-down (send event get-event-type)) (send event get-x) 0))
            (y (if (eq? 'left-down (send event get-event-type)) (send event get-y) 0)))
        (cond [(and (< x 465) (> x 303) (< y 208) (> y 151)) (begin (if square-cell (hexagonal-board) (begin
                                                                                                        (set! matrix-initial (make-hash))
                                                                                                        (smaller-board)))
                                                                    (send settings_canvas refresh))]
              [(and (< x 493) (> x 397) (< y 301) (> y 242)) (send law_dialog show #t)]
              [(and (< x 438) (> x 227) (< y 468) (> y 412)) (begin (send settings_frame show #f)
                                                                    (send main_frame show #t))])))
    (super-new)))

(define instructions-canvas%
  (class canvas%
    (define/override (on-event event)
      (let ((x (if (eq? 'left-down (send event get-event-type)) (send event get-x) 0))
            (y (if (eq? 'left-down (send event get-event-type)) (send event get-y) 0)))
        (cond [(and (< x 449) (> x 251) (< y 496) (> y 452)) (begin (send instructions_frame show #f)
                                                                    (send main_frame show #t))])))
    (super-new)))

(define simulation-canvas%
  (class canvas%
    (super-new)
    (define/override (on-event event)
      (cond [(eq?(send event get-event-type) 'left-down)
             (begin (cond [square-cell (let*[(a (quotient (- (send event get-x) xi) size))
                                      (b (quotient (- (send event get-y) yi) size))]
                                  (when (and (>= a 0) (< a cols) (>= b 0) (< b rows))
                                    (let* ((l (hash-ref matrix-initial b (lambda()(begin (hash-set! matrix-initial b '())
                                                                                         (hash-ref matrix-initial b))))))
                                      (if (belongs? a l) (hash-set! matrix-initial b (remove a l))
                                          (hash-set! matrix-initial b (cons a l))))))]
                   [else (let*[(x (send event get-x))
                               (y (send event get-y))
                               (a (inexact->exact (floor (/ (+ x -30 7.5) 45))))
                               (b (inexact->exact (floor (/ (+ y -30 (* (sqrt 3) 15)) (* (sqrt 3) 7.5)))))
                               (i (car (find a b x y)))
                               (j (cdr (find a b x y)))]
                           (when (and (>= i 0) (< i cols) (>= j 0) (< j rows))
                                    (if (equal? (point-state (2d-vector-ref matrix-initial j i)) 0)
                                        (begin
                                          (2d-vector-set! matrix-initial j i (point j i 1)))
                                        (begin
                                          (2d-vector-set! matrix-initial j i (point j i 0))))))])
                    (draw-matrix matrix-initial))]))))

(define team-canvas%
  (class canvas%
    (define/override (on-event event)
      (let ((x (if (eq? 'left-down (send event get-event-type)) (send event get-x) 0))
            (y (if (eq? 'left-down (send event get-event-type)) (send event get-y) 0)))
        (cond [(and (< x 367) (> x 200) (< y 416) (> y 363)) (begin (send team_frame show #f)
                                                                    (send main_frame show #t))])))
    (super-new)))

(define credits-canvas%
  (class canvas%
    (define/override (on-event event)
      (let ((x (if (eq? 'left-down (send event get-event-type)) (send event get-x) 0))
            (y (if (eq? 'left-down (send event get-event-type)) (send event get-y) 0)))
        (cond [(and (< x 413) (> x 333) (< y 449) (> y 417)) (begin (send credits_frame show #f)
                                                                    (send main_frame show #t))])))
    (super-new)))

(define (find a b x y)
  (cond [(in-hex a b x y) (cons a b)]
        [(in-hex (- a 1) b x y) (cons (- a 1) b)]
        [(in-hex a (- b 1) x y) (cons a (- b 1))]
        [(in-hex (- a 1) (- b 1) x y) (cons (- a 1) (- b 1))]
        [else (cons -1 -1)]))

(define (in-hex i j x y)
  (cond [(or (< i 0) (< j 0)) #f]
        [(= (remainder j 2) 0)
         (let* ([sq3 (sqrt 3)]
                [x1 (+ (* i 3 size) 30)]
                [y1 (+ (* j 0.5 sq3 size) 30)])
           (helper x y x1 y1))]
        [else (let* ([sq3 (sqrt 3)]
                     [x1 (+ (* i 3 size) (* 1.5 size) 30)]
                     [y1 (+ (* j 0.5 sq3 size) 30)])
                (helper x y x1 y1))]))
         
(define (helper x y x1 y1)
  (let* ([sq3 (sqrt 3)]
         [c1 (<= y y1)]
         [c2 (>= y (- y1 (* sq3 15)))]
         [c3 (<= (- y (* sq3 x)) (- y1 (* sq3 x1)))]
         [c4 (>= (+ y (* sq3 x)) (+ y1 (* sq3 x1) (* sq3 15 -1)))]
         [c5 (<= (+ y (* sq3 x)) (+ y1 (* sq3 x1) (* sq3 15)))]
         [c6 (>= (- y (* sq3 x)) (+ y1 (* sq3 x1 -1) (* sq3 30 -1)))])
    (and c1 c2 c3 c4 c5 c6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Frames ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define main_frame [new frame% [label "Game of Life"]
                        [width 647]
                        [height 600]
                        [x 350]
                        [y 50]])

(define simulation_frame [new frame% [label "Simulation"]
                              [width 1000]
                              [height 800]
                              [x 100]
                              [y 10]])

(define instructions_frame [new frame% [label "Instructions and History"]
                                [width 710]
                                [height 560]
                                [x 350]
                                [y 50]])

(define settings_frame [new frame% [label "Settings"]
                            [width 676]
                            [height 532]
                            [x 350]
                            [y 50]])
(define team_frame [new frame% [label "Team Details"]
                        [width 600]
                        [height 480]
                        [x 350]
                        [y 100]])
(define law_dialog (new dialog%
                               [label "Change Evolution Law"]
                               [width 400]
                               [height 600]
                               [parent settings_frame]))

(define credits_frame (new frame% [label "Credits"]
                           [width 780]
                           [height 520]
                           [x 300]
                           [y 100]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Panels ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define simulation_panel (new horizontal-panel%
                              [stretchable-width 0]
                              [stretchable-height 0]
                              [parent simulation_frame]))
(define board_panel (new vertical-panel%
                         [min-width 820]
                         [stretchable-width 0]
                         [min-height 650]
                         [parent simulation_panel]))
(define button_panel (new vertical-panel%
                          [min-height 500]
                          [alignment '(center center)]
                          [parent simulation_panel]))
(define statistics_panel (new horizontal-panel%
                          [alignment '(center center)]
                          [parent simulation_frame]))
(define settings_panel0 (new horizontal-panel%
                                 [alignment '(center center)]
                                 [parent law_dialog]))

(define settings_panel1 (new horizontal-panel%
                                 [alignment '(center center)]
                                 [parent law_dialog]))

(define settings_panel2 (new horizontal-panel%
                                 [alignment '(center center)]
                                 [parent law_dialog]))

(define settings_panel3 (new horizontal-panel%
                                 [alignment '(center center)]
                                 [parent law_dialog]))

(define settings_panel4 (new horizontal-panel%
                                 [alignment '(center center)]
                                 [parent law_dialog]))

(define settings_panel5 (new horizontal-panel%
                                 [alignment '(center center)]
                                 [parent law_dialog]))

(define settings_panel6 (new horizontal-panel%
                                 [alignment '(center center)]
                                 [parent law_dialog]))

(define settings_panel7 (new horizontal-panel%
                                 [alignment '(center center)]
                                 [parent law_dialog]))

(define settings_panel8 (new horizontal-panel%
                                 [alignment '(center center)]
                                 [parent law_dialog]))
(define settings_panel9 (new horizontal-panel%
                                 [alignment '(center center)]
                                 [parent law_dialog]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Canvas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define main_canvas [new main-canvas% [parent main_frame]
                         [paint-callback (λ (c d)
                                           (send d draw-bitmap (read-bitmap "background_main.png") 0 0))]])

(define instructions_canvas [new instructions-canvas% [parent instructions_frame]
                                 [paint-callback (λ (c d)
                                                   (send d draw-bitmap (read-bitmap "background_instructions.png") 0 0))]])

(define settings_canvas [new settings-canvas% [parent settings_frame]
                                 [paint-callback (λ (c d)
                                                   (if square-cell (send d draw-bitmap (read-bitmap "square.png") 0 0)
                                                       (send d draw-bitmap (read-bitmap "hex.png") 0 0)))]])

(define team_canvas [new team-canvas% [parent team_frame]
                         [paint-callback (lambda(c d)
                                           (send d draw-bitmap (read-bitmap "team_details.bmp") 0 0))]])

(define credits_canvas [new credits-canvas% [parent credits_frame]
                            [paint-callback (lambda(c d)
                                              (send d draw-bitmap (read-bitmap "credits.bmp") 0 0))]])
(define i -1)
(define j -1)
(define simulation_canvas [new simulation-canvas% [parent board_panel]
                               [paint-callback
                                (lambda (canvas dc) 
                                  (begin
                                   (paint dc)
                                    (cond (square-cell
                                           (begin
                                             (for (set! i 0) : (<= i rows) : (set! i (+ i 1)) :
                                               (send dc draw-line xi (+ yi (* i size)) (+ xi (* size cols)) (+ yi (* i size))))
                                             (for (set! j 0) : (<= j cols) : (set! j (+ j 1)) :
                                               (send dc draw-line (+ yi (* j size)) xi (+ yi (* j size)) (+ xi (* rows size))))))
                                          )
                                   ) )]])

(define bitmap-size 1000)

(define (paint dc) (send dc draw-bitmap face-bitmap 0 0))

(define face-bitmap (make-object bitmap% bitmap-size bitmap-size ))
(define bm-dc (make-object bitmap-dc% face-bitmap))
(define temp-bitmap (make-object bitmap% bitmap-size bitmap-size ))
(define dm-dc (make-object bitmap-dc% temp-bitmap))
(send bm-dc clear)

(define black-pen (make-object pen% "BLACK" 1 'solid))
(define white-pen (make-object pen% "WHITE" 1 'solid))
(define black-brush (make-object brush% "BLACK" 'solid))
(define white-brush (make-object brush% "WHITE" 'solid))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Dialogs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define square_input_dialog (new dialog%
                                 [label "Snandard Inputs"]
                                 [parent simulation_frame]))

(define hex_input_dialog (new dialog%
                              [label "Standard Inputs"]
                                 [parent simulation_frame]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Buttons   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define start_button (new button% [label "Start"]
                          [parent button_panel]
                          [callback (lambda(button event) (begin (play-now mn) (set! n 1000) (simulator 0)))]
                          [style '(border)]
                          [font normal-control-font]	 
                          [min-width 30]	 
                          [min-height 20]))
(define pause_button (new button% [label "Pause"]
                          [parent button_panel]
                          [callback (lambda(button event) (begin (stop-now) (draw-matrix matrix-initial) (set! n 0)
                                                                 ))]
                          [style '(border)]
                          [font normal-control-font]	 
                          [min-width 30]	 
                          [min-height 20]))
(define clear_button (new button% [label "Clear"]
                          [parent button_panel]
                          [callback (lambda(button event) (begin (stop-now) (set! n 0)
                                                                 (send alev refresher)
                                                                 (if square-cell (set! matrix-initial (make-hash))
                                                                     (set! matrix-initial (hex-matrix)))
                                                                 (draw-matrix matrix-initial)))]
                          [style '(border)]
                          [font normal-control-font]	 
                          [min-width 30]	 
                          [min-height 20]))
(define fast_button (new button% [label "Fast"]
                         [parent button_panel]
                         [callback (lambda(button event) (set! time (/ time 2)))]
                         [style '(border)]
                         [font normal-control-font]	 
                         [min-width 30]	 
                         [min-height 20]))
(define slow_button (new button% [label "Slow"]
                         [parent button_panel]
                         [callback (lambda(button event) (set! time (* time 2)))]
                         [style '(border)]
                         [font normal-control-font]	 
                         [min-width 30]	 
                         [min-height 20]))
(define input_button (new button% [label "Standard Inputs"]
                          [parent button_panel]
                          [callback (lambda(button event) (if square-cell
                                                              (send square_input_dialog show #t)
                                                              (send hex_input_dialog show #t)))]))
(define back_simulation_button (new button% [label "Back"]
                                    [parent statistics_panel]
                                    [callback (lambda (button event) (begin (stop-now) (if square-cell (set! matrix-initial (make-hash))
                                                                     (set! matrix-initial (hex-matrix)))
                                                                            (send alev refresher)    
                                                                            (send main_frame show #t)
                                                                            (send simulation_frame show #f)))]))
(define back_square_dialog_button (new button% [label "Back"]
                                       [parent law_dialog]
                                       [callback (lambda(button event) (begin (send settings_frame show #t)
                                                                              (send law_dialog show #f)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Choice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (f matrix board)
  (begin (board)
  (send simulation_canvas refresh)
  (set! matrix-initial (matrix))
  (draw-matrix matrix-initial)))

(define sample_cases_small (new choice%	 
                                [label "Std. Inputs for smaller board"]	 
                                [choices '("Empty" "Block" "Beehive" "Oscillator" "Pulsar"  "Gosper-Glider Gun" "Die Hard" "R-pentomino" "Acorn")]
                                [horiz-margin 20]
                                [parent square_input_dialog]
                                [callback (lambda (b e)
                                            (let ([event (send b get-selection)])
                                              (cond  [(= event 0) (f make-hash smaller-board)]
                                                     [(= event 1) (f block smaller-board)]
                                                     [(= event 2) (f beehive smaller-board)]
                                                     [(= event 3) (f oscillator smaller-board)]
                                                     [(= event 4) (f pulsar smaller-board)]
                                                     [(= event 5) (f gosper-glider-gun smaller-board)]
                                                     [(= event 6) (f die-hard smaller-board)]
                                                     [(= event 7) (f R-pentomino smaller-board)]
                                                     [(= event 8) (f acorn smaller-board)])))]))
(define sample_cases_large (new choice%	 
                                [label "Std. Inputs for larger board"]	 
                                [choices '("Empty" "Puffer Train" "Orthogonal" "Vacuum Cleaner")]
                                [horiz-margin 20]
                                [parent square_input_dialog]
                                [callback (lambda (b e)
                                            (let ([event (send b get-selection)])
                                              (cond [(= event 0) (f make-hash larger-board)]
                                                    [(= event 1) (f puffer-train larger-board)]
                                                    [(= event 2) (f orthogonal larger-board)]
                                                    [(= event 3) (f vacuum-cleaner larger-board)])))]))

(define sample_hex (new choice%	 
                                [label "Standard Inputs"]	 
                                [choices '("Empty" "Star" "Tri-Star" "Fan-star")]
                                [horiz-margin 20]
                                [parent hex_input_dialog]
                                [callback (lambda (b e)
                                            (let ([event (send b get-selection)])
                                              (cond [(= event 0) (begin (set! matrix-initial (hex-matrix))
                                                                        (draw-matrix matrix-initial))]
                                                    [(= event 1) (begin (set! matrix-initial (star))
                                                                        (draw-matrix matrix-initial))]
                                                    [(= event 2) (begin (set! matrix-initial (tri-star))
                                                                        (draw-matrix matrix-initial))]
                                                    [(= event 3) (begin (set! matrix-initial (fan-star))
                                                                        (draw-matrix matrix-initial))])))]))
(define dead_button (new button%
                         [label "Dead"]
                         [parent settings_panel0]))
(define alive_button (new button%
                         [label "Alive"]
                         [parent settings_panel0]))
(define dead_0 (new choice%
                    [label "0 Neighbours"]
                    [choices '("0" "1")]
                    [enabled #t]
                    [parent settings_panel1]
                    [callback (lambda (b e)
                                (let ([event (send b get-selection)])
                                  (cond [(= event 0) (2d-vector-set! default-law 0 0 0)]
                                        [(= event 1) (2d-vector-set! default-law 0 0 1)])))]))
(define dead_1 (new choice%
                    [label "1 Neighbours"]
                    [choices '("0" "1")]
                    [enabled #t]
                    [parent settings_panel2]
                    [callback (lambda (b e)
                                (let ([event (send b get-selection)])
                                  (cond [(= event 0) (2d-vector-set! default-law 0 1 0)]
                                        [(= event 1) (2d-vector-set! default-law 0 1 1)])))]))
(define dead_2 (new choice%
                    [label "2 Neighbours"]
                    [choices '("0" "1")]
                    [enabled #t]
                    [parent settings_panel3]
                    [callback (lambda (b e)
                                (let ([event (send b get-selection)])
                                  (cond [(= event 0) (2d-vector-set! default-law 0 2 0)]
                                        [(= event 1) (2d-vector-set! default-law 0 2 1)])))]))
(define dead_3 (new choice%
                    [label "3 Neighbours"]
                    [choices '("1" "0")]
                    [enabled #t]
                    [parent settings_panel4]
                    [callback (lambda (b e)
                                (let ([event (send b get-selection)])
                                  (cond [(= event 1) (2d-vector-set! default-law 0 3 0)]
                                        [(= event 0) (2d-vector-set! default-law 0 3 1)])))]))
(define dead_4 (new choice%
                    [label "4 Neighbours"]
                    [choices '("0" "1")]
                    [enabled #t]
                    [parent settings_panel5]
                    [callback (lambda (b e)
                                (let ([event (send b get-selection)])
                                  (cond [(= event 0) (2d-vector-set! default-law 0 4 0)]
                                        [(= event 1) (2d-vector-set! default-law 0 4 1)])))]))
(define dead_5 (new choice%
                    [label "5 Neighbours"]
                    [choices '("0" "1")]
                    [enabled #t]
                    [parent settings_panel6]
                    [callback (lambda (b e)
                                (let ([event (send b get-selection)])
                                  (cond [(= event 0) (2d-vector-set! default-law 0 5 0)]
                                        [(= event 1) (2d-vector-set! default-law 0 5 1)])))]))
(define dead_6 (new choice%
                    [label "6 Neighbours"]
                    [choices '("0" "1")]
                    [enabled #t]
                    [parent settings_panel7]
                    [callback (lambda (b e)
                                (let ([event (send b get-selection)])
                                  (cond [(= event 0) (2d-vector-set! default-law 0 6 0)]
                                        [(= event 1) (2d-vector-set! default-law 0 6 1)])))]))
(define dead_7 (new choice%
                    [label "7 Neighbours"]
                    [choices '("0" "1")]
                    [enabled #t]
                    [parent settings_panel8]
                    [callback (lambda (b e)
                                (let ([event (send b get-selection)])
                                  (cond [(and (= event 0) square-cell) (2d-vector-set! default-law 0 7 0)]
                                        [(and (= event 1) square-cell) (2d-vector-set! default-law 0 7 1)])))]))
(define dead_8 (new choice%
                    [label "8 Neighbours"]
                    [choices '("0" "1")]
                    [enabled #t]
                    [parent settings_panel9]
                    [callback (lambda (b e)
                                (let ([event (send b get-selection)])
                                  (cond [(and (= event 0) square-cell) (2d-vector-set! default-law 0 8 0)]
                                        [(and (= event 1) square-cell) (2d-vector-set! default-law 0 8 1)])))]))

(define alive_0 (new choice%
                     [label "0 Neighbours"]
                     [choices '("0" "1")]
                    [enabled #t]
                     [parent settings_panel1]
                     [callback (lambda (b e)
                                 (let ([event (send b get-selection)])
                                   (cond [(= event 0) (2d-vector-set! default-law 1 0 0)]
                                         [(= event 1) (2d-vector-set! default-law 1 0 1)])))]))
(define alive_1 (new choice%
                     [label "1 Neighbours"]
                     [choices '("0" "1")]
                    [enabled #t]
                     [parent settings_panel2]
                     [callback (lambda (b e)
                                 (let ([event (send b get-selection)])
                                   (cond [(= event 0) (2d-vector-set! default-law 1 1 0)]
                                         [(= event 1) (2d-vector-set! default-law 1 1 1)])))]))
(define alive_2 (new choice%
                     [label "2 Neighbours"]
                     [choices '("1" "0")]
                    [enabled #t]
                     [parent settings_panel3]
                     [callback (lambda (b e)
                                 (let ([event (send b get-selection)])
                                   (cond [(= event 1) (2d-vector-set! default-law 1 2 0)]
                                         [(= event 0) (2d-vector-set! default-law 1 2 1)])))]))
(define alive_3 (new choice%
                     [label "3 Neighbours"]
                     [choices '("1" "0")]
                    [enabled #t]
                     [parent settings_panel4]
                     [callback (lambda (b e)
                                 (let ([event (send b get-selection)])
                                   (cond [(= event 1) (2d-vector-set! default-law 1 3 0)]
                                         [(= event 0) (2d-vector-set! default-law 1 3 1)])))]))
(define alive_4 (new choice%
                     [label "4 Neighbours"]
                     [choices '("0" "1")]
                    [enabled #t]
                     [parent settings_panel5]
                     [callback (lambda (b e)
                                 (let ([event (send b get-selection)])
                                   (cond [(= event 0) (2d-vector-set! default-law 1 4 0)]
                                         [(= event 1) (2d-vector-set! default-law 1 4 1)])))]))
(define alive_5 (new choice%
                     [label "5 Neighbours"]
                     [choices '("0" "1")]
                    [enabled #t]
                     [parent settings_panel6]
                     [callback (lambda (b e)
                                 (let ([event (send b get-selection)])
                                   (cond [(= event 0) (2d-vector-set! default-law 1 5 0)]
                                         [(= event 1) (2d-vector-set! default-law 1 5 1)])))]))
(define alive_6 (new choice%
                     [label "6 Neighbours"]
                     [choices '("0" "1")]
                    [enabled #t]
                     [parent settings_panel7]
                     [callback (lambda (b e)
                                 (let ([event (send b get-selection)])
                                   (cond [(= event 0) (2d-vector-set! default-law 1 6 0)]
                                         [(= event 1) (2d-vector-set! default-law 1 6 1)])))]))
(define alive_7 (new choice%
                     [label "7 Neighbours"]
                     [choices '("0" "1")]
                    [enabled #t]
                     [parent settings_panel8]
                     [callback (lambda (b e)
                                 (let ([event (send b get-selection)])
                                   (cond [(and (= event 0) square-cell) (2d-vector-set! default-law 1 7 0)]
                                         [(and (= event 1) square-cell) (2d-vector-set! default-law 1 7 1)])))]))
(define alive_8 (new choice%
                     [label "8 Neighbours"]
                     [choices '("0" "1")]
                    [enabled #t]
                     [parent settings_panel9]
                     [callback (lambda (b e)
                                 (let ([event (send b get-selection)])
                                   (cond [(and (= event 0) square-cell) (2d-vector-set! default-law 1 8 0)]
                                         [(and (= event 1) square-cell) (2d-vector-set! default-law 1 8 1)])))]))
(define back_square_test_button (new button% [label "Back"]
                                       [parent square_input_dialog]
                                       [callback (lambda(button event) (begin (send simulation_frame show #t)
                                                                              (send square_input_dialog show #f)))]))
(define back_hex_test_button (new button% [label "Back"]
                                       [parent hex_input_dialog]
                                       [callback (lambda(button event) (begin (send simulation_frame show #t)
                                                                              (send hex_input_dialog show #f)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Simulator/ Update Matrix ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define n 1000)

(define (simulator i)
  (cond [(> i n) (display "Done")]
        [else (begin (if square-cell
                         
                         (set! matrix-initial (update-board matrix-initial default-law))
                         (begin (when (equal? matrix-initial (hex-matrix)) (set! n 0))
                         (set! matrix-initial (update-hex-board matrix-initial default-law))))
                     (draw-matrix matrix-initial)
                     (simulator (+ i 1)))]))

(define (draw-matrix matrix)
  (if square-cell
      (begin
        (send bm-dc clear)
        (send bm-dc set-brush black-brush)
        (send alev refresh)
        (map (lambda(i)(map (lambda(j)
                              (when (and (> j -1) (> i -1) (< j cols) (< i rows))
                              (begin
                                (send alev increment)
                                (send bm-dc draw-rectangle (+ (* j size) xi) (+ (* i size) yi) size size)))) 
(hash-ref matrix i))) (hash-keys matrix))
(send alev display-alive))
      (begin
        (send bm-dc clear)
        (send bm-dc set-brush black-brush)
        (send alev refresh)
        (map (lambda (l1) (map (lambda (x) (begin
                                             (if (= (point-state x) 0)
                                                 (begin (send bm-dc set-pen black-pen)
                                                        (send bm-dc set-brush white-brush))
                                                 (begin (send bm-dc set-pen white-pen)
                                                        (send alev increment)
                                                        (send bm-dc set-brush black-brush)))
                                             (if (= (remainder (point-i x) 2) 0)
                                                 (let* ([x1  (+ (* (point-j x) (* 3 size)) 30)]
                                                        [y1  (+ (* (point-i x) (* (/ (sqrt 3) 2) size)) 30)])
                                                   (send bm-dc draw-path (draw-hex x1 y1 size)))
                                                 (let* ([x1  (+ (* (point-j x) (* 3 size)) (* 1.5 size) 30)]
                                                        [y1  (+ (* (point-i x) (* (/ (sqrt 3) 2) size)) 30)])
                                                   (send bm-dc draw-path (draw-hex x1 y1 size)))))) l1))
             (2vector->list2 matrix))
        (send alev display-alive)
        ))
  (send simulation_canvas refresh)
  (sleep/yield time))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define
  total-alive%
  (class object%


    (init-field (alive 0))
    (init-field (alive-yet 0))
    (init-field (iter-count -2))
    (super-new)
    (define/public (display-alive) (send aliv-box set-value (~a alive)))
    (define/public refresher
      (λ()
        (begin
          (set! alive-yet 0)
          (send m restart-max)
          (send m restart-max)
          (set! iter-count -2)
          (get-average))))
    (define/public refresh
      (λ()
      (begin
        ;;;;call the maxima function
        ;(send chill set-value (~a alive))
        (send m change-max alive)
        (set! alive 0)
        ;(send chill set-value (~a alive))
        (set! iter-count (+ 1 iter-count))
        (get-average)
        )))

    (define/public (increment)
      (begin
        (set! alive (+ 1 alive))
        (set! alive-yet (+ 1 alive-yet))
;        (send chill set-value (~a alive))
                
        ;;;;and all else to do

        ))
    (define/public (get-average) 
      (define avg (if (= iter-count 0) 0 (exact->inexact(/ alive-yet iter-count))))
      (send avreg-box set-value (~a avg #:max-width 7)))
      
    ))
(define max%
  (class object%
    (init-field (max 0))
    (super-new)
    (define/public (get-max) max)
    (define/public (restart-max)
      (begin (set! max 0)
             (print "d")
          (send max-box set-value (~a max))));;;call for printing new max
      (define/public (change-max m)
      (if (> m max) (begin (set! max m)
                           (send max-box set-value (~a max)));;;call for printing new max
          (void)))))

(define alev (make-object total-alive%))
(define m (make-object max%))
(define aliv-box (new text-field%	 
   	 	[label "Alive"]	 
   	 	[parent button_panel]	 ))
(define avreg-box (new text-field%	 
   	 	[label "Average alive"]	 
   	 	[parent button_panel]	 ))
(define max-box (new text-field%	 
   	 	[label "Maxima reached"]	 
   	 	[parent button_panel]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-hex x y side)
  (let ([zee (new dc-path%)])
    (send zee move-to x y)
    (send zee line-to (+ x side) y)
    (send zee line-to (+ x (* 1.5 side)) (- y (* (/ (sqrt 3) 2) side)))
    (send zee line-to (+ x side) (- y (* (sqrt 3) side)))
    (send zee line-to x (- y (* (sqrt 3) side)))
    (send zee line-to (- x (/ side 2)) (- y (* (/ (sqrt 3) 2) side)))
    (send zee close)
    zee))

(define time 0.5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send main_frame show #t)
