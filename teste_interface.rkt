#lang racket

(require racket/gui plot racket/draw)

(define frame (new frame% [label "Simulador de Ações"]
                   [width 300]
                            [height 300]))

(define msg (new message% [parent frame]
                 [label "Simulador de Ações"]))

(define painel-principal (new panel% [parent frame]
                              [min-width 300]
                              [min-height 300]))

(define panel (new horizontal-panel%
                   [parent painel-principal]
                   [vert-margin 10]
                   [horiz-margin 0]
                   [alignment '(center top)]
                   [stretchable-width #f]
                   [stretchable-height #f]))

(define panel2 (new horizontal-panel%
                   [parent painel-principal]
                   [vert-margin 0]
                   [horiz-margin 0]
                   [alignment '(center top)]
                   [stretchable-width #f]
                   [stretchable-height #f]))

(define acoes (new radio-box%
                   [parent panel]
                   [label "Opções"]
                   [choices (list "Preços" "MMS" "MME" "RSI" "MACD")]
                   [style (list 'horizontal)]
                   [vert-margin 0]
                   [callback (lambda (control event)
                             (cond [(= 0 (send acoes get-selection)) (send acoes2 enable #t)]
                                   [else (send acoes2 enable #f)]))]))


(define acoes2 (new radio-box%
                   [parent panel2]
                   [label "Ações"]
                   [enabled #f]
                   [vert-margin 0]
                   [horiz-margin 0]
                   [style (list 'horizontal)]
                   [choices (list "Microsoft" "Google" "Petrobras")]))


(define botao (new button%
                   [parent frame]
                   [label "Gerar"]
                   [callback (lambda (button event)
                               (display (send acoes get-selection)))]))

(send frame show #t)

(define num 1)

(define f (new frame% [label "Test graph"]
               [width 300]
               [height 300]))

(define c (new canvas% [parent f]
               [paint-callback (lambda (c dc) 
(plot/dc (function sin  pi num)
           (send c get-dc)
           0 0 260 260 ))]))


(send f show #f)
