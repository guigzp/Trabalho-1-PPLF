#lang racket

(require csv-reading)
(require rackunit)
(require rackunit/text-ui)
(require plot)
(require racket/gui/base)

; Arquivo csv -> Lista de listas
; Função que recebe um arquivo .csv e retorna uma lista de listas separando por quebra de linha, sendo cada linha uma lista.
(define (ler_arquivo nome_arquivo)
  (call-with-input-file nome_arquivo
                        csv->list))
; String -> Lista
; Função que recebe uma String e devolve uma Lista com os elementos da String original separados por ";"
(define (separa string)
  (string-split string "," #:repeat? #t))

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

; String -> String
; Inverte o ano com o dia de uma string de data do formato xx/xx/xxxx
(define (inverte string)
  (define x (reverse(string-split string "/" #:repeat? #t)))
  (string-append (first x) "/" (second x) "/" (third x)))

; Lista de strings -> dados_acoes
; Recebe uma lista de strings e devolve um dado_acoes com os valores das strings
(define (constroi dados)
  (dados_acoes (first dados) (inverte (second dados)) (string->number (sixth dados))))

; String, Lista de Listas de String -> Lista de Listas de dados_acoes
; Filtra a lista de todas as empresas pela string passada, retornando uma lista de listas com somente as empresas com o nome passado
(define (filtra_empresas nome lst)
  (cond [(empty? lst) empty]
        [(equal? nome (first (first lst))) (cons (constroi (first lst)) (filtra_empresas nome (rest lst)))]
        [else (filtra_empresas nome (rest lst))]))

;Estrutura para armazenar os dados do Google
(define google (filtra_empresas "Google" arquivo))

;Estrutura para armazenar os dados da Petrobras
(define petrobras (filtra_empresas "Petrobras" arquivo))

;Estrutura para armazenar os dados da Microsoft
(define microsoft (filtra_empresas "Microsoft" arquivo))

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

;(define (ordena_data lst)
;   (sort lst string<? #:key dados_acoes-date))

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

; Lista de dados_acoes, Numero, Numero -> Lista de Numeros
; Calcula Media Movel Exponencial
(define (media_movel_exponencial lst n media_anterior)
  (define k (/ 2 (+ n 1)))
  (cond [(empty? lst) empty]
        [else
         (define media_exponencial (+ (* k ( - (dados_acoes-close (first lst)) media_anterior)) media_anterior))
         (cons media_exponencial (media_movel_exponencial (rest lst) n media_exponencial))]))

; Avança lista
(define (avanca_lista acao qtd)
  (cond [(= qtd 0) acao]
        [else (avanca_lista (rest acao) (sub1 qtd))]))

; Chama o calculo da media exponencial
(define (media_exponencial acao n)
  (cond [(> n (length acao)) "Valor Inválido, superior ao tamanho do período"]
        [else
  (define primeira_media (first (media_movel acao n)))
  (cons primeira_media (media_movel_exponencial (avanca_lista acao n) n primeira_media))]))

; Diferença entre duas listas 
(define (lista1_menos_lista2 lst1 lst2)
  (cond [(empty? lst1) empty]
        [(empty? lst2) empty]
        [else (cons (- (first lst1) (first lst2)) (lista1_menos_lista2 (rest lst1) (rest lst2)))]))

; MACD = Media exponencial de 12 dias - Media exponencial de 26 dias
(define (macd acao)
  (lista1_menos_lista2 (media_exponencial acao 12) (media_exponencial acao 26)))


; Primeira Soma Média
(define (primeira_soma_media tipo lista quantidade)
  (cond [(= 0 quantidade) 0]
        [(tipo (dados_acoes-close (second lista)) (dados_acoes-close (first lista)))
               (define valor (abs (- (dados_acoes-close (second lista)) (dados_acoes-close (first lista)))))
               (+ valor (primeira_soma_media tipo (rest lista) (sub1 quantidade)))]
        [else (primeira_soma_media tipo (rest lista) (sub1 quantidade))]))

; Primeira Perda Média
(define (primeira_perda_media lista quantidade)
  (/ (primeira_soma_media < lista (sub1 quantidade)) quantidade))

; Primeiro Ganho Médio
(define (primeiro_ganho_medio lista quantidade)
  (/ (primeira_soma_media > lista (sub1 quantidade)) quantidade))


; Força Relativa
(define (forca_relativa acao quantidade)
  (/ (primeiro_ganho_medio acao quantidade) (primeira_perda_media acao quantidade)))

; RSI
(define (calculo_rsi acao original quantidade)
  (cond [(empty? acao) empty]
        [else
         (define valor (- 100 (/ 100(+ 1 (forca_relativa original quantidade)))))
         (cons valor (calculo_rsi (rest acao) (rest original) quantidade))]))

(define (rsi acao quantidade)
  (calculo_rsi (avanca_lista acao quantidade) acao quantidade))
                              
; String, dados_acao -> String
; Devolve a proxima data válida
(define (proxima_data acao data opcao)
  (cond [(empty? acao) "Não existe data válida no banco de dados."]
        [(opcao data (dados_acoes-date (first acao))) (dados_acoes-date (first acao))]
        [(string=? data (dados_acoes-date (first acao)))
         (cond [(empty? (rest acao)) "Não existe data válida no banco de dados."]
               [else (dados_acoes-date (first (rest acao)))])]
        [else (proxima_data (rest acao ) data opcao)]))

; Chama a proxima_data
(define (proxima_data_valida acao data)
  (inverte (proxima_data (inverte_acoes acao) (inverte data)  string<?) ))

; Chama a anterior data
(define (anterior_data_valida acao data)
  (inverte (proxima_data (reverse (inverte_acoes acao)) (inverte data)  string>?) ))
        
(define ordenacao-tests
  (test-suite "Testes Ordenacao"
              (check-equal? (ordena_data google_desordenado) google)
              (check-equal? (ordena_data petrobras_desordenado) petrobras)
              (check-equal? (ordena_data microsoft_desordenado) microsoft)))

(define correlacao-tests
  (test-suite "Testes Correlacao"
              (check-equal? (correlacao google microsoft) 0.1603697511597682)
              (check-equal? (correlacao google petrobras) -0.07983751056172986)
              (check-equal? (correlacao microsoft petrobras) 0.6372067611546479)))

(define (executa-testes . testes)
(run-tests (test-suite "Todos os testes" testes))
(void))

(define ys (macd google ))
(define (gera valor)
  (cond [(= valor 0) empty]
        [else (cons valor (gera (sub1 valor)))]))
(define xs (reverse (gera (length ys))))

(define (preco acao)
  (cond [(empty? acao) empty]
        [else (cons (dados_acoes-close (first acao)) (preco (rest acao)))]))

(define x (preco google))
(define y (reverse (gera (length x))))

;(plot (discrete-histogram (map vector xs ys) #:color 'red #:gap 0.5) #:width 2000 #:y-max 15)
;(plot (lines (map vector xs ys)) #:out-file "teste.png" #:out-kind 'png)
;(plot (lines (map vector y x)#:y-min 1000) )

(define frame (new frame% [label "Simulador de Ações"]))

(define msg (new message% [parent frame]
                 [label "Simulador de Ações"]))

(define opcoes (new radio-box%
                   [parent frame]
                   [label "Opções: "]
                   [choices (list "Preços" "MMS" "MME" "RSI" "MACD")]
                   [style (list 'horizontal)]
                   [vert-margin 10]
                   [callback (lambda (control event)
                             (cond [(= 0 (send opcoes get-selection)) (send texto-periodo enable #f)]
                                   [(= 4 (send opcoes get-selection)) (send texto-periodo enable #f)]
                                   [else (send texto-periodo enable #t)]))]))
;(send opcoes set-selection #f)

(define frame-opcao-invalida (new frame% [label "Erro"][width 100] [height 100]))

(define msg-opcao-invalida (new message% [label "Opção Inválida!"] [parent frame-opcao-invalida]))

(define botao-opcao-invalida (new button% [label "OK"] [parent frame-opcao-invalida]))

(define acoes (new radio-box%
                   [parent frame]
                   [label "Ação: "]
                   [vert-margin 30]
                   [style (list 'horizontal)]
                   [choices (list "Google" "Microsoft" "Petrobras")]))

(define texto-periodo (new text-field%
                   [parent frame]
                   [label "Período"]
                   [enabled #f]
                   [vert-margin 30]))

(define botao (new button%
                  [parent frame]
                  [label "Gerar"]
                  [vert-margin 10]
                  [horiz-margin 10]
                  [callback (lambda (button event)
                              (define opcao-escolhida (send opcoes get-selection))
                              (define acao-escolhida (send acoes get-selection))
                              (cond [(false? opcao-escolhida) (send frame-opcao-invalida show #t)]
                                    [(= 0 opcao-escolhida) (geraGrafico opcao-escolhida acao-escolhida 0)]
                                    [(= 1 opcao-escolhida) (geraGrafico opcao-escolhida acao-escolhida (string->number (send texto-periodo get-value)))]
                                    [(= 2 opcao-escolhida) (geraGrafico opcao-escolhida acao-escolhida (string->number (send texto-periodo get-value)))]
                                    [(= 3 opcao-escolhida) (geraGrafico opcao-escolhida acao-escolhida (string->number (send texto-periodo get-value)))]
                                    [(= 4 opcao-escolhida) (geraGrafico opcao-escolhida acao-escolhida 0)]))]))

(define (geraGrafico opcao acao periodo)
  (define precos 0)
  
  (cond [(= opcao 0)
         (cond [(= 0 acao) (set! precos (preco google)) ]
               [(= 1 acao) (set! precos (preco microsoft))]
               [(= 2 acao) (set! precos (preco petrobras))])]
        [(= opcao 1)
         (cond [(= acao 0) (set! precos (media_movel google periodo))]
               [(= acao 1) (set! precos (media_movel microsoft periodo))]
               [(= acao 2) (set! precos (media_movel petrobras periodo))])]
        [(= opcao 2)
         (cond [(= acao 0) (set! precos (media_exponencial google periodo))]
               [(= acao 1) (set! precos (media_exponencial microsoft periodo))]
               [(= acao 2) (set! precos (media_exponencial petrobras periodo))])]
        [(= opcao 3)
         (cond [(= acao 0) (set! precos (rsi google periodo))]
               [(= acao 1) (set! precos (rsi microsoft periodo))]
               [(= acao 2) (set! precos (rsi petrobras periodo))])]
        [(= opcao 4)
         (cond [(= acao 0) (set! precos (macd google))]
               [(= acao 1) (set! precos (macd microsoft))]
               [(= acao 2) (set! precos (macd petrobras))])])

  (define  periodos (reverse (gera (length precos))))
  (define min (argmin sqr precos))
  (define max (argmax sqr precos))

  (define frame-grafico (new frame% [label "Gráfico"]
               [width 300]
               [height 300]))
  
  (define c (new canvas% [parent frame-grafico]
               [paint-callback (lambda (c dc) 
                                 (plot/dc (lines (map vector periodos precos)#:y-min min #:y-max max )
                                          (send c get-dc)
                                          0 0 260 260 #:x-label "Período" #:y-label "Preço"))]))

  (send frame-grafico show #t))

 
(send frame show #t)