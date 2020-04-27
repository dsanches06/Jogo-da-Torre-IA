;;;; puzzle.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Autor:

;;------------------------------------------------------------------------------------------------------
;;  VARIAVEIS GLOBAIS
;;------------------------------------------------------------------------------------------------------
;;contador para contabilizar ocorrencia = 100
(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))

;;------------------------------------------------------------------------------------------------------
;;   TABULEIRO 7x7
;;------------------------------------------------------------------------------------------------------
(defun teste()
 "Função para para tabuleiro formatado"   
 (mostrar-tabuleiro (tabuleiro-inicial)))

(defun tabuleiro-inicial ()
  "Função para criar tabuleiro inicial já com inserido na posição a1 e j1"  
  (substituir 1 1 (gerar-tabuleiro-aux) T))

(defun gerar-tabuleiro-aux ()
 "Função auxiliar para gerar e baralhar o tabuleiro com casas de 0 a 77"
 (tabuleiro-aleatorio (baralhar (gerar-lista-sem-numero-089))))

(defun tabuleiro-aleatorio (lista &optional(n 7))
  "Função que recebe uma é numa lista e criar sublistas de n elementos recorrendo à função 
   subseq que tem um comportamento semelhante ao substring para strings"
  (cond
      ((null lista) nil)
      (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))))

(defun linha (i tabuleiro)
  "Função que recebe um índice e o tabuleiro e retorna uma lista que representa esta linha"
   (cond ((or (< i 0) (>= i (length tabuleiro))) NIL)
        (t (nth i tabuleiro))))
	
(defun celula (i j tabuleiro)
  "Função que recebe dois índices e o tabuleiro e retorna o valor presente nessa célula do tabuleiro"
   (linha i (coluna j tabuleiro)))

(defun coluna (j tabuleiro)
  "Função que recebe um índice e o tabuleiro e retorna uma lista que representa esta coluna"
  (mapcar #'(lambda(linha) 
             (cond ((or (< j 0) (>= j (length linha))) NIL)
                   (t (nth j linha)))) tabuleiro))

;------------------------------------------------------------------------------------------------------
;;   FUNÇÕES AUXILIARES
;;------------------------------------------------------------------------------------------------------
(defun baralhar (lista)
  "Função que recebe uma lista e irá mudar aleatoriamente os seus números. 
   Faça uma função recursiva e utilize a função nth, a função random e a função 
   remover-se em que a função deve remover o número da lista igual ao encontrado aleatoriamente"
   (let ((n (nth (random (length lista)) lista)))
         (cond 
             ((= (funcall *fn*) (length lista)) lista) ;;condição de paragem se for 99
             (t(baralhar (setf lista (cons n (remover-se #'(lambda (x) (= x n)) lista))))))))


(defun primeiro-digito (n)
 "Função que recebe um numero e retorna o primeiro digito"
   (cond
       ((< n 10) n) 
        (t (primeiro-digito (floor (/ n 10))))))

(defun ultimo-digito (n)
 "Função que recebe um numero e retorna o ultimo digito"
  (mod n 10))

(defun simetrico-aux(x y)
 "Função que recebe 2 numero e retorna o seu simetrico"
     (let* ((x (write-to-string  x)) 
            (y (write-to-string y)))
           (cond
               ((let ((z (concatenate 'string y x))) (parse-integer z)))
               (T NIL))))

(defun remover-se (pred lista)
  "Função que recebe uma função e uma lista para remover os numeros duplicados"
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (t (cons (car lista) (remover-se pred (cdr lista))))))


(defun substituir-posicao-aux (i tabuleiro &optional (valor 'NIL))
  "Função que recebe um índice, uma lista e um valor (por default o valor é NIL) e substitui pelo valor pretendido nessa posição."
  (cond
        ((= i 0) (cons valor (cdr tabuleiro)))
        (t (cons (car tabuleiro) (substituir-posicao-aux (- i 1) (cdr tabuleiro) valor)))))

(defun posicao-aux (casa tabuleiro &optional (inicio 0))     
  (cond ((null tabuleiro) nil)
        ((atom (first tabuleiro))
         (if (eql casa (first tabuleiro))
             (list inicio)
             (posicao-aux casa (rest tabuleiro) (1+ inicio))))
        (t (let ((posicao (posicao-aux casa (first tabuleiro) 0)))
             (if posicao
                 (cons inicio posicao)
                 (posicao-aux casa (rest tabuleiro) (1+ inicio)))))))

(defun posicao-destino-cavalo ()
 "Função para retorna a casa do destino da peça para aplicação de regras"

)

;;------------------------------------------------------------------------------------------------------
;;   GERAR LISTA SEM NUMEROS  0, 8  e 9
;;------------------------------------------------------------------------------------------------------
(defun numero-com-089 (n)
 "Função que recebe um numero e verifica se existe numero com 0, 8 e 9 e retorn t ou nil"
    (let ((x (ultimo-digito n)))
          (cond
              ((or (= x 0) (= x 8) (= x 9)) T)
             (T NIL))))

(defun gerar-lista-sem-numero-089 ()
 "Função que gerar e cria uma lista de numeros entre 11 até 77 com remoção os numeros que terminam com 0, 8 e 9"
   (loop for n from 11 to 77
	if (equal (numero-com-089 n) 'NIL)
      collect n))

;;------------------------------------------------------------------------------------------------------
;;   REGRAS DE NUMEROS-DUPLOS
;;------------------------------------------------------------------------------------------------------
(defun numero-duplo (n)
 "Função que recebe um numero e verifica se existe os 2 numeros são igual"
    (let* ((x (primeiro-digito n)) 
           (y (ultimo-digito n)))
          (cond
             ((= x y) n)
             (T NIL))))

(defun regra-numero-duplo (tabuleiro)
 "Se uma Torre for colocado numa casa com um número duplo,
  por exemplo 66, então deve ser removido o número duplo de maior valor"
    ;;obter posicao inicial da peça
  ;;obter a posicao final da peça (obter o valor da casa antes de movimentar)
  (let* ((posicao (obter-posicao-peca-ou-casa tabuleiro))
         (valor (obter-peca-ou-casa (first posicao) (second posicao) tabuleiro)))
        (format t "p: ~A, valor: ~A, p0: ~A, p1: ~A ~%" posicao valor (first posicao) (second posicao))
  
))

;;------------------------------------------------------------------------------------------------------
;;   REGRAS DE SIMETRICOS
;;------------------------------------------------------------------------------------------------------
(defun simetrico (n)
 "Função que recebe um numero e verifica se existe os 2 numeros são igual"
    (let* ((x (primeiro-digito n)) 
           (y (ultimo-digito n)))
          (cond
              ((let ((z (simetrico-aux x y))) z))
              (T NIL))))


(defun regra-simetrico (tabuleiro) 
 "Se a casa escolhida tiver um número com dois dígitos diferentes,
  por exemplo 57, então, em consequência, o número simétrico 75 é apagado do tabuleiro"
  ;;obter posicao inicial da peça
  ;;obter a posicao final da peça (obter o valor da casa antes de movimentar)
  (let* ((posicao (obter-posicao-peca-ou-casa tabuleiro))
         (valor (obter-peca-ou-casa (first posicao) (second posicao) tabuleiro)))
        (format t "p: ~A, valor: ~A, p0: ~A, p1: ~A ~%" posicao valor (first posicao) (second posicao))
  
))

;;------------------------------------------------------------------------------------------------------
;;   PEÇA CAVALO ("T")
;;------------------------------------------------------------------------------------------------------
(defun obter-posicao-peca-ou-casa (tabuleiro &optional (casa 'T))
  "Função que recebe o elemento e tabuleiro e ainda devolve a posição (i j) em que se encontra o cavalo. 
  Caso o cavalo não se encontre no tabuleiro deverá ser retornado NIL"
  (posicao-aux casa tabuleiro))

(defun obter-peca-ou-casa (i j tabuleiro)
 "Função que procura a posicao de cavalo no tabuleiro e retorna o seu valor"
  (celula i j tabuleiro))

(defun mover-peca (x y dx dy tabuleiro)
  "Função que irá mover o cavalo"
  (let ((novo-tabuleiro (copy-tree tabuleiro))
        (novo-x (+ x dx))
        (novo-y (+ y dy))
        (peca (nth x (nth y tabuleiro))))
    (setf (nth x (nth y novo-tabuleiro)) NIL
          (nth novo-x (nth novo-y novo-tabuleiro)) peca) novo-tabuleiro))

;;------------------------------------------------------------------------------------------------------
;;   VALIDAÇÃO DE CASAS E MOVIMENTOS
;;------------------------------------------------------------------------------------------------------
(defun validar-casap (x y tabuleiro)
  "Função que recebe dois indices e tabuleiro para validar se a casa destino está disponivel"
  (and (<= 0 x 6)
       (<= 0 y 6)
       (null (nth x (nth y tabuleiro)))))

(defun casa-vaziap (x y tabuleiro)
"Retorna o valor presente nessa célula do tabuleiro"	
	(let ((vazio (celula x y tabuleiro)))
		(cond 
		    ((equal vazio 'NIL) T)
		     (T NIL))))

(defun validar-movimento (x y board)
  "Função que recebe dois indices e tabuleiro para validar o movimento "
  (if (and (<= 0 x 6)
           (<= 0 y 6)
           (= (apply '+
                     (mapcar (lambda (L)
                               (if (or (not (equal (nth 0 L) x))
                                       (not (equal (nth 1 L) y)))
                                   0
                                   1))
                             board))
              0) )
      0
      nil)) 

;;------------------------------------------------------------------------------------------------------
;;   OPERAÇÃO SUBSTITUIR PEÇA NO TABULEIRO
;;------------------------------------------------------------------------------------------------------
(defun substituir (i j tabuleiro &optional (valor 'NIL))
  "Função que recebe dois índices, o tabuleiro e um valor (por default o valor é NIL).
   A função deverá retornar o tabuleiro com a célula substituída pelo valor pretendido. 
   Utilize a função substituir-posicao definida anteriormente" 
  (substituir-posicao-aux j tabuleiro (substituir-posicao-aux i (linha j tabuleiro) valor)))


;;------------------------------------------------------------------------------------------------------
;;   CONSTRUTOR
;;------------------------------------------------------------------------------------------------------
(defun criar-no (estado pecas &optional (profundidade 0) (heuristica 0) (f 0) (pai NIL))
 "Cria uma lista que representa um no que tem um estado. Este no pode tambem ter 
  uma profunidade, heuristica, f e um no que o gerou"
  (list estado pecas profundidade heuristica f pai))

;;------------------------------------------------------------------------------------------------------
;;   METODOS SELECTORES (GETTERS)
;;------------------------------------------------------------------------------------------------------
(defun no-estado (no)
 "Função que retorna o estado de no"
  (first no))

(defun no-peca (no)
 "Função que retorna o numero de pontos da peça de no"
  (second no))

(defun no-profundidade (no)
  "Funcao que retorna a profundidade de um no"
  (third no))

(defun no-heuristica (no)
  "Funcao que retorna a heuristica de um no"
  (fourth no))

(defun no-f (no)
  "Funcao que retorna o f de um no"
  (fifth no))

(defun no-pai (no)
  "Funcao que retorna o no gerador de um determinado no"
  (sixth no))

;;------------------------------------------------------------------------------------------------------
;;   OPERADORES
;;------------------------------------------------------------------------------------------------------
(defun CIMA-ESQUERDA (x y tabuleiro) ;; mov cima esquerda
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 40"
   (cond
       ((equal (validar-movimento (- x 1) (- y 2) tabuleiro) 0) 
        (mover-peca x y -1 -2 tabuleiro))
       (T nil)))

(defun CIMA-DIREITA (x y tabuleiro) ;; mov cima direita
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 30"
   (cond
       ((equal (validar-movimento (+ x 1) (- y 2) tabuleiro) 0) 
        (mover-peca x y 1 -2 tabuleiro))
       (T nil)))

(defun DIREITA-CIMA (x y tabuleiro) ;; mov direita cima
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 70"
   (cond
       ((equal (validar-movimento (+ x 2) (- y 1) tabuleiro) 0) 
	(mover-peca x y 2 -1 tabuleiro))
       (T nil)))

(defun DIREITA-BAIXO (x y tabuleiro) ;; mov direita baixo
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 80"
   (cond
       ((equal (validar-movimento (+ x 1) (+ y 1) tabuleiro) 0) 
	(mover-peca x y 2 1 tabuleiro))
       (T nil)))

(defun BAIXO-DIREITA (x y tabuleiro) ;; mov baixo direita
"Função que recebe o tabuleiro e movimenta o cavalo para a posição 2 linhas abaixo e uma coluna ao lado direito, ou seja, o movimento que leva o cavalo para a casa de valor 10"
  (cond
      ((equal (validar-movimento (+ x 1) (+ y 2) tabuleiro) 0)
       (mover-peca x y 1 2 tabuleiro))
     (T nil)))

(defun BAIXO-ESQUERDA (x y tabuleiro) ;; mov baixo esquerda
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 20"
   (cond
       ((equal (validar-movimento (- x 1) (+ y 2) tabuleiro) 0) 
        (mover-peca x y -1 2 tabuleiro))
       (T nil)))

(defun ESQUERDA-BAIXO (x y tabuleiro) ;; mov esquerda baixo
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 50"
   (cond 
       ((equal (validar-movimento (- x 2) (+ y 1) tabuleiro) 0) 
	(mover-peca x y -2 1 tabuleiro))
       (T nil)))

(defun ESQUERDA-CIMA (x y tabuleiro) ;;mov esquerda cima
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 60"
   (cond
       ((equal (validar-movimento (- x 2) (- y 1) tabuleiro) 0) 
	(mover-peca x y -2 -1 tabuleiro))
       (T nil)))

(defun operadores ()
  "Funcao que lista todos os operadores existentes no dominio do problema"
  (list 'CIMA-ESQUERDA 'CIMA-DIREITA 'DIREITA-CIMA 'DIREITA-BAIXO 'BAIXO-DIREITA 'BAIXO-ESQUERDA 'ESQUERDA-BAIXO 'ESQUERDA-CIMA))

;;------------------------------------------------------------------------------------------------------
;;  SUCESSORES
;;------------------------------------------------------------------------------------------------------
(defun gerar-sucessores (no operadores algoritmo heuristica &optional (profundidade NIL))
  "Funcao que devolve a lista de todos os sucessores de um determinado no passado como argumento"
  (cond
      ((equal algoritmo 'dfs)
          (cond
              ;So se aplica os sucessores caso a profundidade do no seja inferior a profundidade maxima
              ((>= (no-profundidade no) profundidade) NIL)  
              (T (apply #'append (mapcar #'(lambda (operador)
                                                   (aplicar-operador-no no operador heuristica)) operadores)))))
   (T (apply #'append (mapcar #'(lambda (operador)
                                        (aplicar-operador-no no operador heuristica)) operadores)))))


(defun aplicar-operador-no (no operador heuristica)
  "Funcao que aplica apenas um operador a um determinado no. Consoante o operador passado por argumento esta 
   funcao ira determinar as jogadas possiveis para esse operador e ira criar um no 
   (um sucessor) para cada uma dessas jogadas possives"
   (format t "operador: ~A~%" operador)
  (let ((jogadas-possiveis 
	                   (cond
                              ((or (equal operador 'CIMA-ESQUERDA)
                                   (equal operador 'CIMA-DIREITA)
                                   (equal operador 'DIREITA-CIMA)
                                   (equal operador 'DIREITA-BAIXO)
                                   (equal operador 'BAIXO-DIREITA)
                                   (equal operador 'BAIXO-ESQUERDA)
                                   (equal operador 'ESQUERDA-BAIXO)
                                   (equal operador 'ESQUERDA-CIMA))
                               (jogadas-possiveis (no-estado no))))))
      (mapcar #'(lambda (jogada)
                     (let* ((estado (funcall operador (first jogada) (second jogada) (no-estado no)))
                            (pecas-novas NIL)
                            (heuristica-novo-no (funcall heuristica estado pecas-novas))
                            (profundidade-novo-no (1+ (no-profundidade no)))
                            (custo-novo-no (+ profundidade-novo-no heuristica-novo-no)))
                        (criar-no estado pecas-novas profundidade-novo-no heuristica-novo-no custo-novo-no no)
					)) jogadas-possiveis)))

(defun jogadas-possiveis (tabuleiro)
  "Funcao que determina todas as jogadas possiveis para um tipo de 
   peca num determinado tabuleiro, devolvendo-as numa lista"
   ;;obter posicao do cavalo e obter o valor celula  (obter-peca-tabuleiro tabuleiro)
   ;; verificar se existe casas vazias ou não
   ;; determinar as possiveis jogadas
  (let* ((posicao (obter-posicao-peca-ou-casa tabuleiro))
         (valor (obter-peca-ou-casa (first posicao) (second posicao) tabuleiro)))
     (format t "p: ~A, valor: ~A, p0: ~A, p1: ~A ~%" posicao valor (first posicao) (second posicao))

))

(defun jogada-in-jogadas-possiveis-p (jogada tabuleiro)
  "Funcao que verifica se uma determinada jogada existe na lista de jogadas possiveis para a peca passada por argumento"
  (let ((jogadas-possiveis (funcall #'jogadas-possiveis tabuleiro)))
        (eval (cons 'or (mapcar #'(lambda (jogada-possivel) 
                                         (cond
                                             ((equal jogada-possivel jogada) T)
                                             (T nil))) jogadas-possiveis)))))
;;------------------------------------------------------------------------------------------------------
;;   FUNÇÕES DE CÁLCULO DE HEURISTICAS
;;------------------------------------------------------------------------------------------------------
(defun heuristica (tabuleiro &optional (peca NIL))
 "Calcula a heuristica usando as funções auxiliares"

)
