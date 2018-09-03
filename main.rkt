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

; Empresas é a constante que armazena os dados do arquivo .csv formatado, é uma lista de listas de strings.
(define empresas (transforma arquivo))

; Define a estrutura dos dados das ações
(struct dados_acoes (nome date open high low close adj volume) #:transparent)

; Lista -> dados_acoes
; Recebe uma lista de strings e devolve um dado_acoes com os valores das strings
(define (teste dados)
  (dados_acoes (first dados)  (second dados) (string->number (third dados)) (string->number (fourth dados))
               (string->number (fifth dados)) (string->number (sixth dados)) (string->number (seventh dados)) (string->number (eighth dados))))

; String, Lista de Listas de String -> Lista de Listas de dados_acoes
; Filtra a lista de todas as empresas pela string passada, retornando uma lista de listas com somente as empresas com o nome passado
(define (filtra_empresas nome lst)
  (cond [(empty? lst) empty]
        [(equal? nome (first (first lst))) (cons (teste (first lst)) (filtra_empresas nome (rest lst)))]
        [else (filtra_empresas nome (rest lst))]))

;Estrutura para armazenar os dados do Google
(define google (filtra_empresas "Google" empresas))

;Estrutura para armazenar os dados da Petrobras
(define petrobras (filtra_empresas "Petrobras" empresas))

;Estrutura para armazenar os dados da Microsoft
(define microsoft (filtra_empresas "Microsoft" empresas))

; Lista de dados_acoes -> Numero
; Devolve a soma da coluna de fechamento de uma lista de ações
(define (somacoluna lst)
  (cond [(empty? lst) 0]
        [else (+ (dados_acoes-close (first lst)) (somacoluna (rest lst)))]))

; Lista de dados_acoes -> Numero
; Devolve a soma das colunas de de fechamaento de 2 ações multiplicados
; (multiplica o valor da lista1 pela lista2 para todos os elementos enquanto os soma)
(define (somamultiplicado lst1 lst2)
  (cond [(empty? lst1) 0]
        [else (+ ( * (dados_acoes-close (first lst1)) (dados_acoes-close (first lst2)))  (somamultiplicado (rest lst1) (rest lst2)))]))


; Lista de dados_acoes, Lista de dados_acoes -> Numero
; Calcula o indice de correlaçao entre duas empresas
(define (correlacao acao1 acao2)
  (define x (somacoluna acao1))
  (define y (somacoluna acao2))
  (define xy (somamultiplicado acao1 acao2))
  (define xquadrado (somamultiplicado acao1 acao1))
  (define yquadrado (somamultiplicado acao2 acao2))
  (define m (length acao1))
  ( / (- xy (/ (* x y) m)) (sqrt (* ( - yquadrado (/ (* y y) m)) ( - xquadrado (/ (* x x) m))))))
