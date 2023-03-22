;Trabalho de Linguagens de Programacao
;Alunos : Thiago de Castro G. Pereira / Eduardo Cardoso Lourenzo
;20/12/2015




;###########  Funcoes da polonesa reversa  ##############

;converte um operador em um numero para facilitar a procura de um valor na tabela de decisoes
(define conversor 
  (lambda (c)
    (cond
      ((equal? c '$) 1)
      ((equal? c '+) 2)
      ((equal? c '-) 3)
      ((equal? c '*) 4)
      ((equal? c '/) 5)
      ((equal? c '<) 6)
      ((equal? c '>) 7))))

;Tabela de decisoes
(define tabelaDecisoes '(
                         (4 1 1 1 1 1 5)
                         (2 2 2 1 1 1 2)
                         (2 2 2 1 1 1 2)
                         (2 2 2 2 2 1 2)
                         (2 2 2 2 2 1 2)
                         (5 1 1 1 1 1 3)
                         ))

;funcao que auxilia a funcao valorMatriz para pegar o valor na matriz (tabelaDecisoes)
(define posicao
  (lambda(x L)
    (if (> x 1)
        (posicao (- x 1) (cdr L))
        (car L))))

;funcao que retorna um valor dado a linha, a coluna e a matriz
(define valorMatriz
  (lambda(i j L)
    (posicao j (posicao i L))))

;funcao que modifica e retorna a lista Texas de acordo com o valor obtido da tabela de decisao
(define decisaoTexas
  (lambda (a Texas)
    (if (operador? a)
        (cond
          ((= (valorMatriz (conversor (car Texas)) (conversor a) tabelaDecisoes) 1) (cons a Texas))
          ((= (valorMatriz (conversor (car Texas)) (conversor a) tabelaDecisoes) 2) (cdr Texas))
          ((= (valorMatriz (conversor (car Texas)) (conversor a) tabelaDecisoes) 3) (cdr Texas))
          ((= (valorMatriz (conversor (car Texas)) (conversor a) tabelaDecisoes) 4) Texas)
          )
        Texas)))

;funcao que modifica e retorna a lista California de acordo com o valor obtido da tabela de decisao
(define decisaoCalifornia
  (lambda (a California Texas)
    (if (operador? a)
        (cond
          ((= (valorMatriz (conversor (car Texas)) (conversor a) tabelaDecisoes) 1) California)
          ((= (valorMatriz (conversor (car Texas)) (conversor a) tabelaDecisoes) 2) (append California (cons (car Texas) '())))
          ((= (valorMatriz (conversor (car Texas)) (conversor a) tabelaDecisoes) 3) California)
          ((= (valorMatriz (conversor (car Texas)) (conversor a) tabelaDecisoes) 4) California)
          )
        (append California (cons a '())))))

;funcao que modifica e retorna a lista NovaIorque de acordo com o valor obtido da tabela de decisao
(define decisaoNovaIorque
  (lambda (a NovaIorque Texas)
    (if (operador? a)
        (if (= (valorMatriz (conversor (car Texas)) (conversor a) tabelaDecisoes) 2)
            NovaIorque
            (cdr NovaIorque))
        (cdr NovaIorque))))

;se for um operador usado na tabela de decisao retorna verdadeiro senao retorna falso
(define operador?
  (lambda (c)
    (cond
      ((equal? c '$) #t)
      ((equal? c '+) #t)
      ((equal? c '-) #t)
      ((equal? c '*) #t)
      ((equal? c '/) #t)
      ((equal? c '<) #t)
      ((equal? c '>) #t)
      (else #f))))

;funcao recursiva que recebe uma expressao e mantem 2 listas (Texas e California) para usar
;o metodo da polonesa reversa e transformar a expressao para a forma pos-fixa
(define tranformaExpr
  (lambda (NovaIorque Texas California)
    (if (null? NovaIorque)
        (if (equal? (car Texas) '$)
            California
            (tranformaExpr '() 
                           (decisaoTexas '$ Texas) 
                           (decisaoCalifornia '$ California Texas)))
        (tranformaExpr (decisaoNovaIorque (car NovaIorque) NovaIorque Texas)
                       (decisaoTexas (car NovaIorque) Texas) 
                       (decisaoCalifornia (car NovaIorque) California Texas)))))

;funcao que recebe uma expressao e retorna essa nesma expressao na forma pos-fixa
(define polonesa 
  (lambda (Expr) 
    (tranformaExpr (append Expr '($)) '($) '() )))






;###########  Funcoes para manipulacao de variaveis  ##############

;Retorna o valor de uma dada variavel, buscando da lista de variaveis
(define pegarVar
  (lambda (a L)
    (if (equal? a (car (car L)))
        (cadar L)
        (pegarVar a (cdr L)))))

;Adiciona uma nova variavel na lista de variaveis a partir de uma atribuicao
;retorna a nova lista de variaveis
(define adicionarVar
  (lambda (L1 L2)
    (if (number? (caddr L1))
        (cons(cons (car L1) (cons (caddr L1) '())) L2)
        (cons(cons (car L1) (cons (pegarVar (caddr L1) L2) '())) L2))))





;###########  Funcoes para resolucao de expressoes  ##############

;Funcao que verifica de o parametro passado e um numero ou uma variavel
;caso seja numero retorna o mesmo
;caso seja uma variavel retorna seu valor de acordo com a lista de variaveis
(define numOuVar
  (lambda (n Variaveis)
    (if (number? n)
        n
        (pegarVar n Variaveis))))

;Funcao que retorna o resultado de uma operacao dado o operador e dois termos
(define resultado
  (lambda (operador n1 n2 Variaveis)
    (cond
      ((equal? operador '+) (+ (numOuVar n1 Variaveis) (numOuVar n2 Variaveis)))
      ((equal? operador '-) (- (numOuVar n1 Variaveis) (numOuVar n2 Variaveis)))
      ((equal? operador '*) (* (numOuVar n1 Variaveis) (numOuVar n2 Variaveis)))
      ((equal? operador '/) (/ (numOuVar n1 Variaveis) (numOuVar n2 Variaveis))))))

;Funcao executa uma expressao pos-fixa e retorna o resultado
(define executa
  (lambda (L1 L2 Variaveis L3)
    (if (null? (cdr L1))
        (NumOuVar (car L1) Variaveis)
        (if (operador? (car L2))
            (executa 
             (append (cddr L3) (cons (resultado (car L2) (cadr L3) (car L3) Variaveis) (cdr L2))) 
             (append (cddr L3) (cons (resultado (car L2) (cadr L3) (car L3) Variaveis) (cdr L2)))
             Variaveis '())
            (executa L1 (cdr L2) Variaveis (cons (car L2) L3))))))
  
;Funcao que imprime o valor retornado da funcao executa
;retorna a lista de variaveis
(define expressao
  (lambda (L Variaveis)
    (display(executa (polonesa L) (polonesa L) Variaveis '()))
    (newline)
    Variaveis))





;###########  Funcoes de interpretacao da lista (Programa)  ##############

;Funcao que seleciona se a lista e uma expressao ou uma atribuicao
(define seleciona
  (lambda (L Variaveis)
    (if (equal? (car L) '@)
        (expressao (cdr L) Variaveis)
        (adicionarVar L Variaveis))))

;Funcao recursiva chamada pela funcao avaliar que contem a lista de variaveis e percorre a lista (programa)
;executando as linhas (sublistas)
(define auxAvaliar 
  (lambda (Programa Variaveis) 
    (if (null? Programa)
        (display "")
        (auxAvaliar (cdr Programa) (seleciona (car Programa) Variaveis)))))

;Funcao principal que recebe o programa (lista) e executa
(define avaliar
  (lambda (Programa)
    (auxAvaliar Programa '())))


;EXECUTANDO UM EXEMPLO DA FUNCAO AVALIAR
(avaliar '(
           (A = 1)
           (B = 2.5) 
           (@ A)
           (@ A + B) 
           (C = B)
           (@ A + < B * 2 > / C) 
          )
)


