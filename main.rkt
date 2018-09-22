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

; Chama o cálculo do RSI
(define (rsi acao quantidade)
  (calculo_rsi (avanca_lista acao quantidade) acao quantidade))

; Lista de dacos_acoes -> Lista de Strings
; Gera uma lista com todas as datas válidas
(define (gera_datas acao)
  (cond [(empty? acao) empty]
        [else (cons (dados_acoes-date (first acao)) (gera_datas (rest acao)))]))

; Estrutura de lista que armazena todas as dastas válidas
(define datas (gera_datas google))

; lista de strings -> lista de strings
; Recebe a lista de datas e retorna a mesma mas com no formato ANO/MES/DIA
(define (inverte_datas lista_datas)
  (cond [(empty? lista_datas) empty]
        [else (cons (inverte (first lista_datas)) (inverte_datas (rest lista_datas)))]))
                              
; String, dados_acao -> String
; Devolve a proxima data válida
(define (proxima_data lista_datas data opcao)
  (cond [(empty? lista_datas) empty]
        [(opcao data  (first lista_datas)) (first lista_datas)]
        [(string=? data  (first lista_datas))
         (cond [(empty? (rest lista_datas)) empty]
               [else  (first (rest lista_datas))])]
        [else (proxima_data (rest lista_datas ) data opcao)]))

; String -> String
; Chama a anterior data para calcular a data posterior da passada e a devolve
(define (posterior_data_valida data)
  (define nova_data (proxima_data (inverte_datas datas) (inverte data)  string<?))
  (cond [(empty? nova_data) empty]
        [else (inverte nova_data)]))

; String -> String
; Chama a anterior data para calcular a data anterior da passada e a devolve
(define (anterior_data_valida data)
  (define nova_data (proxima_data (reverse (inverte_datas datas)) (inverte data)  string>?) )
  (cond [(empty? nova_data) empty]
        [else (inverte nova_data)]))

; Lista de acoes, String -> Numero
; Devolve o valor de fechemato da ação no dia passado
(define (valor_acao_dia acao data)
  (cond [(equal? data (dados_acoes-date (first acao))) (dados_acoes-close (first acao))]
        [else (valor_acao_dia (rest acao) data)]))
        
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

(define data-tests
  (test-suite "Testes Data"
              (check-equal? (posterior_data_valida "02/01/2018") "03/01/2018")
              (check-equal? (posterior_data_valida "30/04/2018") "01/05/2018")
              (check-equal? (anterior_data_valida "03/01/2018") "02/01/2018")
              (check-equal? (anterior_data_valida "01/05/2018") "30/04/2018")))

(define media_movel-tests
  (test-suite "Teste Media Movel Simples"
              (check-equal? (first (media_movel google 10)) 1100.145996)
              (check-equal? (first (media_movel microsoft 10)) 87.79499960000001)
              (check-equal? (first (media_movel petrobras 10)) 11.171000000000001)))

(define media_movel_exponencial-tests
  (test-suite "Teste Media Movel Exponencial"
              (check-equal? (first (media_exponencial google 14)) 1111.1821462857145)
              (check-equal? (first (media_exponencial microsoft 14)) 88.55714242857142)
              (check-equal? (first (media_exponencial petrobras 14)) 11.454999999999998)))

(define rsi-tests
  (test-suite "Teste RSI"
              (check-equal? (first (rsi google 14)) 93.30480987894174)
              (check-equal? (first (rsi microsoft 14)) 80.23506410256401)
              (check-equal? (first (rsi petrobras 14)) 91.00529100529101)))

(define macd-tests
  (test-suite "Teste MACD"
              (check-equal? (first (macd google)) -18.221595237179372)
              (check-equal? (first (macd microsoft)) -2.069808198717922)
              (check-equal? (first (macd petrobras)) -0.9373076923076944)))

; Função para executar os testes
(define (executa-testes . testes)
(run-tests (test-suite "Todos os testes" testes))
(void))


; Função para chamar todos os testes
(define (executa_todos_testes)
  (executa-testes correlacao-tests data-tests ordenacao-tests media_movel-tests media_movel_exponencial-tests macd-tests rsi-tests))

; Numero -> Lista de numeros
; Gera uma lista de numeros de 1 até o valor passado
(define (gera valor)
  (cond [(= valor 0) empty]
        [else (cons valor (gera (sub1 valor)))]))

; lista de acoes -> lista de numeros
; Constroi uma lista somente com os valores de fechamento da ação passada
(define (preco acao)
  (cond [(empty? acao) empty]
        [else (cons (dados_acoes-close (first acao)) (preco (rest acao)))]))

(define frame_gera_grafico (new frame% [label "Gráficos das Ações"]))

(define msg (new message% [parent frame_gera_grafico]
                 [label "Gráficos das Ações"]))

(define opcoes (new radio-box%
                   [parent frame_gera_grafico]
                   [label "Opções: "]
                   [choices (list "Preços" "MMS" "MME" "RSI" "MACD")]
                   [style (list 'horizontal)]
                   [vert-margin 10]
                   [callback (lambda (control event)
                             (cond [(= 0 (send opcoes get-selection)) (send texto-periodo enable #f)]
                                   [(= 4 (send opcoes get-selection)) (send texto-periodo enable #f)]
                                   [else (send texto-periodo enable #t)]))]))

(define frame-opcao-invalida (new frame% [label "Erro"][width 100] [height 100]))

(define msg-opcao-invalida (new message% [label "Algo está errado!"] [parent frame-opcao-invalida]))

(define botao-opcao-invalida (new button% [label "OK"] [parent frame-opcao-invalida]
                                  [callback (lambda (button event) (send frame-opcao-invalida show #f))]))

(define acoes (new radio-box%
                   [parent frame_gera_grafico]
                   [label "Ação: "]
                   [vert-margin 30]
                   [style (list 'horizontal)]
                   [choices (list "Google" "Microsoft" "Petrobras")]))

(define texto-periodo (new text-field%
                   [parent frame_gera_grafico]
                   [label "Período"]
                   [enabled #f]
                   [vert-margin 30]))

(define botao (new button%
                  [parent frame_gera_grafico]
                  [label "Gerar"]
                  [vert-margin 10]
                  [horiz-margin 10]
                  [callback (lambda (button event)
                              (define opcao-escolhida (send opcoes get-selection))
                              (define acao-escolhida (send acoes get-selection))
                              (cond 
                                    [(or (= 1 opcao-escolhida) (= 2 opcao-escolhida) (= 3 opcao-escolhida))
                                     (cond [(equal? "" (send texto-periodo get-value))
                                            (send frame-opcao-invalida show #t)]
                                           [else (geraGrafico opcao-escolhida acao-escolhida (string->number (send texto-periodo get-value)))])]
                                    [else (geraGrafico opcao-escolhida acao-escolhida 0)]))]))


; Função para plotar o gráfico para um frame de acordo com a opção e o período passado
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
               [width 600]
               [height 600]))
  
  (define c (new canvas% [parent frame-grafico]
               [paint-callback (lambda (c dc) 
                                 (plot/dc (lines (map vector periodos precos)#:y-min min #:y-max max )
                                          (send c get-dc)
                                          0 0 500 500 #:x-label "Período" #:y-label "Preço"))]))

  (send frame-grafico show #t))

(define frame_compra_venda (new frame% [label "Simulador de Compra e Venda"]
                                [width 500]
                                [height 500]))

(define mensagem_data (new message% [label "" ]
                          [parent frame_compra_venda]
                          [horiz-margin 0]
                          [auto-resize #t]))

(define preco_google (new message% [label "" ]
                          [parent frame_compra_venda]
                          [horiz-margin 0]
                          [auto-resize #t]))

(define preco_petrobras (new message% [label "" ]
                          [parent frame_compra_venda]
                          [horiz-margin 0]
                          [auto-resize #t]))

(define preco_microsoft (new message% [label "" ]
                          [parent frame_compra_venda]
                          [horiz-margin 0]
                          [auto-resize #t]))

(define compradas_google (new message% [label "Ações Google: 0"]
                              [parent frame_compra_venda]
                              [horiz-margin 0]
                              [auto-resize #t]))

(define compradas_microsoft (new message% [label "Ações Microsoft: 0"]
                              [parent frame_compra_venda]
                              [horiz-margin 0]
                              [auto-resize #t]))

(define compradas_petrobras (new message% [label "Ações Petrobras: 0"]
                              [parent frame_compra_venda]
                              [horiz-margin 0]
                              [auto-resize #t]))

(define total_gasto (new message% [label "Total Gasto: 0"]
                              [parent frame_compra_venda]
                              [horiz-margin 0]
                              [auto-resize #t]))

(define total_vendido (new message% [label "Total Vendido: 0"]
                              [parent frame_compra_venda]
                              [horiz-margin 0]
                              [auto-resize #t]))

(define acoes_opcoes (new radio-box%
                   [parent frame_compra_venda]
                   [label "Ação: "]
                   [style (list 'horizontal)]
                   [choices (list "Google" "Microsoft" "Petrobras")]))

(define text_quantidade (new text-field%
                   [parent frame_compra_venda]
                   [label "Quantidade: "]))

; Atualiza o frame conforme a data passada
(define (atualiza nova_data)
  (cond [(empty? nova_data) (terminar)]
        [else
         (send mensagem_data set-label nova_data)
         (send preco_google set-label (string-append "Preço Google: " (number->string (valor_acao_dia google nova_data))))
         (send preco_microsoft set-label (string-append "Preço Microsoft: " (number->string (valor_acao_dia microsoft nova_data))))
         (send preco_petrobras set-label (string-append "Preço Petrobras: " (number->string (valor_acao_dia petrobras nova_data))))
         ]))

; String da forma "Ações aaaa: numero" -> numero
; Retira somente o numero de uma string
(define (pegar_valor_mensagem msg)
  (string->number (first (string-split (second (string-split msg ":"))))))

; Reinicia Simulação Compra e Venda para o primeiro dia
(define (reinicia)
  (atualiza "02/01/2018")
  (send total_gasto set-label "Total Gasto: 0")
  (send total_vendido set-label "Total Vendido: 0")
  (send compradas_google set-label "Ações Google: 0")
  (send compradas_microsoft set-label "Ações Microsoft: 0")
  (send compradas_petrobras set-label "Ações Petrobras: 0")
  )


; Numero -> altera as labels das mensagens
; Função para o callback do botao de comprar ação
(define (comprar opcao)
  (define qtd (string->number (send text_quantidade get-value)))
  (define valor_g (valor_acao_dia google (send mensagem_data get-label)))
  (define valor_p (valor_acao_dia petrobras (send mensagem_data get-label)))
  (define valor_m (valor_acao_dia microsoft (send mensagem_data get-label)))
  (define total_atual (pegar_valor_mensagem (send total_gasto get-label)))
  (cond [(or (not (number? qtd)) (> 1 qtd)) (send frame-opcao-invalida show #t)]
        [else (cond [(= 0 opcao) (send compradas_google set-label
                                       (string-append "Ações Google: " (number->string (+ qtd (pegar_valor_mensagem (send compradas_google get-label))))))
                                 (send total_gasto set-label (string-append "Total Gasto: " (number->string (+ (* qtd valor_g) total_atual))))]
                    
                    [(= 1 opcao) (send compradas_microsoft set-label
                                       (string-append "Ações Microsoft: " (number->string (+ qtd (pegar_valor_mensagem (send compradas_microsoft get-label))))))
                                 (send total_gasto set-label (string-append "Total Gasto: " (number->string (+ (* qtd valor_m) total_atual))))]
                    
                    [(= 2 opcao) (send compradas_petrobras set-label
                                       (string-append "Ações Petrobras: " (number->string (+ qtd (pegar_valor_mensagem (send compradas_petrobras get-label))))))
                                 (send total_gasto set-label (string-append "Total Gasto: " (number->string (+ (* qtd valor_p) total_atual))))]
                    )]))


(define frame_final_simulacao (new frame% [label "Simulação Compra e Venda"]))

(define mensagem_final_simulacao (new message% [label ""]
                                      [parent frame_final_simulacao]
                                      [auto-resize #t]))

(define botao_nova_simulacao (new button% [label "Nova Simulação"]
                                  [parent frame_final_simulacao]
                                  [callback (lambda (button event)
                                              (send frame_final_simulacao show #f)
                                              (send frame_pede_periodo show #t))]))

; Função para pegar todas as ações compradas e vender todas
(define (vender_tudo)
  (define qtd_g (*(pegar_valor_mensagem (send compradas_google get-label)) (valor_acao_dia google (send mensagem_data get-label))))
  (define qtd_m (*(pegar_valor_mensagem (send compradas_microsoft get-label)) (valor_acao_dia microsoft (send mensagem_data get-label))))
  (define qtd_p (*(pegar_valor_mensagem (send compradas_petrobras get-label)) (valor_acao_dia petrobras (send mensagem_data get-label))))
  (+ qtd_g qtd_m qtd_p))

; Termina a simulação calculando o lucro ou prejuízo e reiniciando a simulação para o dia 1
(define (terminar)
  (define gasto (pegar_valor_mensagem (send total_gasto get-label)))
  (define vendido (+ (pegar_valor_mensagem (send total_vendido get-label)) (vender_tudo)))
  (define final (- vendido gasto))
  (cond [(< final 0) (send mensagem_final_simulacao set-label (string-append "Você teve prejuízo de: " (number->string (abs final))))]
        [else (send mensagem_final_simulacao set-label (string-append "Você teve lucro de: " (number->string (abs final))))])
  (reinicia)
  (send frame_compra_venda show #f)
  (send frame_final_simulacao show #t))


; Callback botão vender
(define (vender opcao)
  (define qtd (string->number (send text_quantidade get-value)))
  (define disponivel_google (pegar_valor_mensagem (send compradas_google get-label)))
  (define disponivel_petrobras (pegar_valor_mensagem (send compradas_petrobras get-label)))
  (define disponivel_microsoft (pegar_valor_mensagem (send compradas_microsoft get-label)))
  (define vendido (pegar_valor_mensagem (send total_vendido get-label)))
  (cond [(or (not (number? qtd)) (> 1 qtd)) (send frame-opcao-invalida show #t)]
        [else (cond [(= 0 opcao)
                     (cond [(< disponivel_google qtd) (send frame-opcao-invalida show #t)]
                           [else (send compradas_google set-label (string-append "Ações Google: " (number->string (- disponivel_google qtd))))
                                 (send total_vendido set-label (string-append "Total Vendido: "
                                                                              (number->string (+ vendido (* qtd (valor_acao_dia google
                                                                                                                                (send mensagem_data get-label)))))))])]
                    [(= 1 opcao)
                     (cond [(< disponivel_microsoft qtd) (send frame-opcao-invalida show #t)]
                           [else (send compradas_microsoft set-label (string-append "Ações Microsoft: " (number->string (- disponivel_microsoft qtd))))
                                 (send total_vendido set-label (string-append "Total Vendido: "
                                                                              (number->string (+ vendido (* qtd (valor_acao_dia microsoft
                                                                                                                                (send mensagem_data get-label)))))))])]           
                    [(= 2 opcao)
                     (cond [(< disponivel_petrobras qtd) (send frame-opcao-invalida show #t)]
                           [else (send compradas_petrobras set-label (string-append "Ações Petrobras: " (number->string (- disponivel_petrobras qtd))))
                                 (send total_vendido set-label (string-append "Total Vendido: "
                                                                              (number->string (+ vendido (* qtd (valor_acao_dia petrobras
                                                                                                                                (send mensagem_data get-label)))))))])]
                    )]))

(define botao_proximo (new button% [label "Encerrar o dia"]
                           [parent frame_compra_venda]
                           [callback (lambda (button event)
                                       (atualiza (posterior_data_valida (send mensagem_data get-label))))]))

(define botao_comprar (new button% [label "Comprar"]
                           [parent frame_compra_venda]
                           [callback (lambda (button event)
                                       (comprar (send acoes_opcoes get-selection)))]))

(define botao_vender (new button% [label "Vender"]
                          [parent frame_compra_venda]
                          [callback (lambda (button event)
                                      (vender (send acoes_opcoes get-selection)))]))

(define botao_terminar (new button% [label "Terminar Simulação"]
                           [parent frame_compra_venda]
                           [callback (lambda (button event)
                                       (terminar))]))

(define frame_principal (new frame%
                             [label "Trabalho PPLF - Guilherme"]
                             [width 500]
                             [height 500]))

(define frame_pede_periodo (new frame%
                                [label "Período da Simulação"]
                                [height 50]
                                [width 50]
                                ))

(define pede_periodo (new message%
                          [parent frame_pede_periodo]
                          [label "Digite uma data (DD/MM/AAAA) para iniciar a simulação ou deixe em branco para iniciar do começo"]
                          ))

(define pede_data (new text-field%
                       [parent frame_pede_periodo]
                       [label "Data: "]))

(define botao_inicia_simulacao (new button%
                                    [label "Iniciar"]
                                    [parent frame_pede_periodo]
                                    [callback (lambda (button event)
                                                (define data_digitada (send pede_data get-value))
                                                 (cond [(not (number? data_digitada)) (atualiza "02/01/2018")]
                                                       [(number? (index-of datas data_digitada)) (atualiza data_digitada)]
                                                       [(not (empty? (anterior_data_valida data_digitada))) (atualiza (anterior_data_valida data_digitada))]
                                                       [(not (empty? (posterior_data_valida data_digitada))) (atualiza (posterior_data_valida data_digitada))])
                                                (send frame_pede_periodo show #f)
                                                (send frame_compra_venda show #t))]))

(define mensagem1 (new message% [parent frame_principal]
                       [label "Preços Google"]))

(define painel_precos_google (new horizontal-panel%
                                  [parent frame_principal]
                                  [alignment '(center center)]))

(define canvas_precos_google (new editor-canvas%
                                  [parent painel_precos_google]
                                  [style '(transparent)]
                                 ))

(define texto_precos_google (new text%))

(define mensagem2 (new message% [parent frame_principal]
                       [label "Preços Microsoft"]))

(define painel_precos_microsoft (new horizontal-panel%
                                  [parent frame_principal]
                                  [alignment '(center center)]))

(define canvas_precos_microsoft (new editor-canvas%
                                  [parent painel_precos_microsoft]
                                  [style '(transparent)]
                                 ))

(define texto_precos_microsoft (new text%))

(define mensagem3 (new message% [parent frame_principal]
                       [label "Petrobras"]))

(define painel_precos_petrobras (new horizontal-panel%
                                  [parent frame_principal]
                                  [alignment '(center center)]))

(define canvas_precos_petrobras (new editor-canvas%
                                  [parent painel_precos_petrobras]
                                  [style '(transparent)]
                                 ))

(define texto_precos_petrobras (new text%))

(define (cria_data_valores lst string)
  (cond [(empty? lst) string]
        [else (set! string (string-append string "Data: " (dados_acoes-date (first lst)) " Fechamento: " (number->string (dados_acoes-close (first lst))) "\n"))
              (cria_data_valores (rest lst) string) ]))

(send texto_precos_google insert (cria_data_valores google ""))

(send canvas_precos_google set-editor texto_precos_google)

(send texto_precos_microsoft insert (cria_data_valores microsoft ""))

(send canvas_precos_microsoft set-editor texto_precos_microsoft)

(send texto_precos_petrobras insert (cria_data_valores petrobras ""))

(send canvas_precos_petrobras set-editor texto_precos_petrobras)

(define botao_graficos (new button%
                            [label "Gerar Gráficos"]
                            [parent frame_principal]
                            [callback (lambda (button event)
                                        (send frame_gera_grafico show #t))]))

(define botao_simulacao (new button%
                            [label "Iniciar Simulações"]
                            [parent frame_principal]
                            [callback (lambda (button event)
                                        (send frame_pede_periodo show #t))]))

(send frame_principal show #t)