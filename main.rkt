#lang racket

(require csv-reading)

(define (csvfile->list filename)
  (call-with-input-file filename
                        csv->list))

(define empresas (csvfile->list "dados.csv"))

(struct dados_acoes (nome date open high low close adj volume))

