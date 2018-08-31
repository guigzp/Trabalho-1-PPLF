#lang racket

(require csv-reading)

(define (csvfile->list filename)
  (call-with-input-file filename
                        csv->list))

(define empresas (csvfile->list "dados.csv"))

;(struct dados_acoes (nome date open high low close adj volume))
(struct dados_acoes (nome date) #:transparent )

(define microsoft (list))

(define petrobras (list))

(define google (list))

(define (separa string)
  (string-split string ";" #:repeat? #t))

(define (addend x lst)
  (cond [(empty? lst) (list x)]
        [(empty? (rest lst)) (list (first lst) x)]
        [else
         (cons (first lst) (addend x (rest lst)))]))

(define teste (dados_acoes "Guilherme" "Zamberlam"))
(define teste2 (dados_acoes "Eduardo" "Torrezan"))

(separa (first (first empresas)))

(define (tenta lst)
  (cond [(empty? (rest lst) (first lst))]
        [else (dados_acoes 

