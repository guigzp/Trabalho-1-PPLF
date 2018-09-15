#lang racket

(require racket/gui plot racket/draw)

(define frame (new frame% [label "Simulador de Ações"]
                   [width 300]
                            [height 300]))

(define msg (new message% [parent frame]
                 [label "Simulador de Ações"]))

(define panel (new horizontal-pane%
                   [parent frame]
                   [vert-margin 10]
                   [horiz-margin 10]
                   [alignment '(left bottom)]
                   [stretchable-width #t]
                   [stretchable-height #t]))

(define acoes (new radio-box%
                   [parent panel]
                   [label "Opções"]
                   [choices (list "Preços" "MMS" "MME" "RSI" "MACD")]
                   [style (list 'horizontal)]
                   [vert-margin 10]
                   [callback (lambda (control event)
                             (cond [(= 0 (send acoes get-selection)) (send acoes2 enable #t)]
                                   [else (send acoes2 enable #f)]))]))
(send acoes set-selection #f)


(define acoes2 (new radio-box%
                   [parent panel]
                   [label "Ações"]
                   [enabled #f]
                   [vert-margin 30]
                   [style (list 'horizontal)]
                   [choices (list "Microsoft" "Google" "Petrobras")]))

(send acoes2 set-selection #f)

(define botao (new button%
                   [parent panel]
                   [label "Gerar"]
                   [callback (lambda (button event)
                               (display (send acoes get-selection)))]))

(send frame show #f)

(define num 1)

(define f (new frame% [label "Test graph"]
               [width 300]
               [height 300]))

(define c (new canvas% [parent f]
               [paint-callback (lambda (c dc) 
(plot/dc (function sin  pi num)
           (send c get-dc)
           0 0 260 260 ))]))


(send f show #t)