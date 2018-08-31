#lang racket

(require csv-reading)

; Arquivo csv -> Lista de listas
; Função que recebe um arquivo .csv e retorna uma lista de listas separando por quebra de linha, sendo cada linha uma lista.
(define (ler_arquivo nome_arquivo)
  (call-with-input-file nome_arquivo
                        csv->list))
; String -> Lista
; Função que recebe uma String e devolve uma Lista com os elementos da String original separados por ";"
(define (separa string)
  (string-split string ";" #:repeat? #t))

; Le o .csv para arquivo
(define arquivo (ler_arquivo "dados.csv"))

; Lista -> Lista
; Função para arrumar os dados gerados na leitura do .csv, como na leitura cada linha é uma lista de 1 elemento, sendo esse elemento uma String,
; é necessário separar cada um dos dados desta string, portanto, a função transforma utiliza a função separa para transformar cada lista de string
; em uma lista de várias strings sem os ; separadores.
(define (transforma lst)
  (cond [(empty? lst) empty]
        [ (cons (separa (first (first lst))) (transforma (rest lst)))]))

(define empresas (transforma arquivo))

;(struct dados_acoes (nome date open high low close adj volume))
(struct dados_acoes (nome date) #:transparent )

(define microsoft (list))

(define petrobras (list))

(define google (list))

(define teste (dados_acoes "Guilherme" "Zamberlam"))
(define teste2 (dados_acoes "Eduardo" "Torrezan"))

(define x (lambda (lst)
            (cond [(empty? lst) empty]
                  [( = "Google" (first lst)) add

