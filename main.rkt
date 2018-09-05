#lang racket

(require csv-reading)
(require rackunit)
(require rackunit/text-ui)

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
        [else (cons (separa (first (first lst))) (transforma (rest lst)))]))

; Empresas é a constante que armazena os dados do arquivo .csv formatado, é uma lista de listas de strings.
(define empresas (transforma arquivo))

; Define a estrutura dos dados das ações
(struct dados_acoes (nome date close) #:transparent)

; Lista de strings -> dados_acoes
; Recebe uma lista de strings e devolve um dado_acoes com os valores das strings
(define (constroi dados)
  (dados_acoes (first dados) (second dados) (string->number (sixth dados))))

; String, Lista de Listas de String -> Lista de Listas de dados_acoes
; Filtra a lista de todas as empresas pela string passada, retornando uma lista de listas com somente as empresas com o nome passado
(define (filtra_empresas nome lst)
  (cond [(empty? lst) empty]
        [(equal? nome (first (first lst))) (cons (constroi (first lst)) (filtra_empresas nome (rest lst)))]
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

(define google_desordenado (shuffle google))
(define petrobras_desordenado (shuffle petrobras))
(define microsoft_desordenado (shuffle microsoft))

; String -> String
; Inverte o ano com o dia de uma string de data do formato xx/xx/xxxx
(define (inverte string)
  (define x (reverse(string-split string "/" #:repeat? #t)))
  (string-append (first x) "/" (second x) "/" (third x)))

; dados_acoes -> dados_acoes
; Recebe um dados_acoes e devolve o mesmo com os campos da data com o ano e o dia trocados
(define (inverte_data_acao acao)
  (dados_acoes (dados_acoes-nome acao) (inverte(dados_acoes-date acao)) (dados_acoes-close acao)))

; Lista de dados_acoes -> lista de dados_acoes
; Recebe uma lista de dados_acoes e devolve a mesma lista mas com todos as datas invertidas seguindo a função inverte_data
(define (inverte_acoes lst)
  (cond [(empty? lst) empty]
        [else (cons (inverte_data_acao (first lst)) (inverte_acoes (rest lst)))]))

; Lista de dados_acoes -> Lista de dados_acoes
; Ordena uma lista de dados_acoes por data, primeiramente declara uma nova lista que é igual a passada como paramento mas com a data
; de todos os elementos da forma ANO/MES/DIA. Em seguida é ordenado a string data seguindo em ordem crescente de caracteres.
; Por último é feita novamente a inversão da data da lista ordenada para voltar para o formato DIA/MES/ANO
(define (ordena_data lst)
  (define x (inverte_acoes lst))
    (inverte_acoes (sort x string<? #:key dados_acoes-date)))


; Lista de dados_acoes, Numero -> Numero
; Devolve a soma dos Numero primeiro termos da lista
(define (soma_qtd acao qtd)
  (cond [(< (length acao) qtd) -1]
        [(= 0 qtd) 0]
        [else (+ (dados_acoes-close (first acao)) (soma_qtd (rest acao) (sub1 qtd)))]))

; Lista de dados_acoes, Numero -> Lista de Numeros
; Calcula Media Movel ao longo dos dias para um determinado numero de dias, devolve uma lista de medias moveis ao longo do tempo
(define (media_movel acao dias)
  (cond [(> dias (length acao)) empty]
        [(empty? acao) empty]
        [else (cons ( / (soma_qtd acao dias) dias) (media_movel (rest acao) dias))]))

; String, dados_acao -> String
; Devolve a proxima data válida
(define (proxima_data acao data)
  (cond [(empty? acao) "Data não pertencente ao banco de dados"]
        [(equal? (dados_acoes-date (first acao)) data)
         (cond [(empty? (rest acao)) "Não possui data próxima válida"]
               [else (dados_acoes-date (first (rest acao)))])]
        [else (proxima_data (rest acao) data)]))
        


(define ordenacao-tests
  (test-suite "Testes Ordenacao"
              (check-equal? (ordena_data google_desordenado) google)
              (check-equal? (ordena_data petrobras_desordenado) petrobras)
              (check-equal? (ordena_data microsoft_desordenado) microsoft)))

(define correlacao-tests
  (test-suite "Testes Correlacao"
              (check-equal? (correlacao google microsoft) 0.16036975115981003)
              (check-equal? (correlacao google petrobras) -0.07983751056182631)
              (check-equal? (correlacao microsoft petrobras) 0.6372067611547209)))

(define (executa-testes . testes)
(run-tests (test-suite "Todos os testes" testes))
(void))