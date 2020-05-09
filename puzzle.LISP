;;;; puzzle.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Autor:

;;------------------------------------------------------------------------------------------------------
;;  VARIAVEIS GLOBAIS
;;------------------------------------------------------------------------------------------------------
;;contador para incrementar ocorrencia = 77
(defparameter *fn* (let ((count 0)) #'(lambda () (incf count))))

;;------------------------------------------------------------------------------------------------------
;;   TABULEIRO 7x7
;;------------------------------------------------------------------------------------------------------

(defun board ()
 '((64 35 74 26 21 57 12) 
   (43 15 47 53 24 56 42) 
   (51 41 71 31 17 45 55) 
   (67 66 22 T 54 75 32) 
   (13 11 16 23 25 27 33) 
   (34 36 37 44 46 52 61) 
   (62 63 65 72 73 76 77)))

(defun tabuleiro-inicial ()
"Função para criar tabuleiro inicial já com inserido na posição a1 e g1 no array (0 6)"  
(substituir 3 3 (gerar-tabuleiro) 'T))

(defun gerar-tabuleiro ()
"Função auxiliar para gerar e baralhar o tabuleiro com casas de 0 a 77
 sem numero com 0, 8 , 9 e ainda sem peça colocada"
(tabuleiro-aleatorio (baralhar (lista-sem-numero-089))))

(defun linha (x tabuleiro)
"Função que recebe um índice e o tabuleiro e retorna uma lista que representa esta linha"
(cond ((or (not (numberp x)) (< x 0) (>= x (length tabuleiro))) NIL)
     (T (nth x tabuleiro))))

(defun coluna (y tabuleiro)
"Função que recebe um índice e o tabuleiro e retorna uma lista que representa esta coluna"
  (cond ((or (not (numberp y)) (< y 0)) NIL)
        (t (mapcar #'(lambda (linha &aux (n-coluna (nth y linha))) n-coluna) tabuleiro))))
	
(defun celula (x y tabuleiro)
"Função que recebe dois índices e o tabuleiro e retorna o valor presente nessa célula do tabuleiro"
  (cond ((or (not (numberp x)) (not (numberp y))) nil)
        ((or (< x 0) (< y 0)) nil)
        (t (nth x (linha y tabuleiro)))))

;------------------------------------------------------------------------------------------------------
;;   FUNÇÕES AUXILIARES
;;------------------------------------------------------------------------------------------------------
(defun matriz-transposta (tabuleiro)
 "Função que faz a transposta do tabuleiro"
  (apply #'mapcar #'list tabuleiro))

(defun tabuleiro-aleatorio (lista &optional(n 7))
"Função que recebe uma é numa lista e criar sublistas de n elementos recorrendo à função 
subseq que tem um comportamento semelhante ao substring para strings"
(cond
   ((null lista) NIL)
   (T (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))))

(defun baralhar (lista)
"Função que recebe uma lista e irá mudar aleatoriamente os seus números. 
Faça uma função recursiva e utilize a função nth, a função random e a função 
remover-se em que a função deve remover o número da lista igual ao encontrado aleatoriamente"
(let ((n (nth (random (length lista)) lista)))
      (cond 
          ((= (funcall *fn*) (length lista)) lista) ;;condição de paragem se for 99
          (T (baralhar (setf lista (cons n (remover-se #'(lambda (x) (= x n)) lista))))))))

(defun primeiro-digito (n)
"Função que recebe um numero e retorna o primeiro digito"
(cond
    ((< n 10) n) 
     (T (primeiro-digito (floor (/ n 10))))))

(defun ultimo-digito (n)
"Função que recebe um numero e retorna o ultimo digito"
(mod n 10))

(defun digitos-iguais (n)
"Função que recebe um numero e verifica se os 2 digitos são iguais"
 (let* ((x (primeiro-digito n)) 
        (y (ultimo-digito n)))
       (cond
          ((= x y) T)
          (T NIL)))) 
			 			 		 
(defun simetrico (n)
"Função que recebe 2 numero e retorna o seu simetrico"
  (let* ((x (primeiro-digito n)) 
         (y (ultimo-digito n))
	    (w (write-to-string  x)) 
         (z (write-to-string y)))
        (cond
            ((let ((str (concatenate 'string z w))) (parse-integer str)))
            (T NIL))))

(defun maior-numero-lista (tabuleiro)
 "Função que retorna o numero maior numa lista"
  (reduce #'max tabuleiro))

(defun remover-se (pred lista)
"Função que recebe uma função e uma lista para remover os numeros duplicados"
(cond ((null lista) NIL) 
     ((funcall pred (car lista)) (remover-se pred (cdr lista)))
     (T (cons (car lista) (remover-se pred (cdr lista))))))

(defun posicao-aux (casa tabuleiro &optional (inicio 0))     
(cond ((null tabuleiro) nil)
     ((atom (first tabuleiro))
      (if (eql casa (first tabuleiro))
          (list inicio)
          (posicao-aux casa (rest tabuleiro) (1+ inicio))))
     (T (let ((posicao (posicao-aux casa (first tabuleiro) 0)))
          (if posicao
              (cons inicio posicao)
              (posicao-aux casa (rest tabuleiro) (1+ inicio)))))))

;;------------------------------------------------------------------------------------------------------
;;   GERAR LISTA SEM NUMEROS COM  0, 8  e 9
;;------------------------------------------------------------------------------------------------------
(defun numero-com-089 (n)
"Função que recebe um numero e verifica se existe numero com 0, 8 e 9 e retorna t ou nil"
 (let ((x (ultimo-digito n)))
       (cond
           ((or (= x 0) (= x 8) (= x 9)) T)
          (T NIL))))

(defun lista-sem-numero-089 ()
"Função que gerar e cria uma lista de numeros entre 11 até 77 com remoção os numeros que terminam com 0, 8 e 9"
(loop for n from 11 to 77
	if (equal (numero-com-089 n) 'NIL)
   collect n))

;;------------------------------------------------------------------------------------------------------
;;   REGRAS DE NUMEROS-DUPLOS
;;------------------------------------------------------------------------------------------------------
(defun regra-numero-duplo (peca pontos tabuleiro)
"Se uma Torre for colocado numa casa com um número duplo,
por exemplo 66, então deve ser removido o número duplo de maior valor"
  ;; (format t "~@T~@T~@T~@TPeça ~A -> Casa ~A - Regra de Duplo~%" peca pontos)
   (cond 
         ((= pontos 0) tabuleiro)
         (T (let* ((lista-nova (apply #'append tabuleiro));;converter o tabuleiro 7x7 em uma única lista
                   (lista-max (loop for n in lista-nova ;;percorrer a nova lista
                   if (and (numberp n)(equal (digitos-iguais n) 'T) (> n pontos))
                             collect n)))
            ;;(format t "~@T~@T~@T~@TLista Duplos Max: ~A~%" lista-max)
              (cond 
                  ((equal lista-max 'NIL) tabuleiro)
                  (T (let* ((duplo-max (maior-numero-lista lista-max))
                            (posicao-duplo-max (obter-posicao tabuleiro duplo-max)))
          ;;(format t "~@T~@T~@T~@TDuplo Max: ~A~%" duplo-max)
         ;; (format t "~@T~@T~@T~@TPosicao ~A: ~A~%" duplo-max posicao-duplo-max)

          (substituir (first posicao-duplo-max) (second posicao-duplo-max) tabuleiro 'NIL)))))

)))

;;------------------------------------------------------------------------------------------------------
;;   REGRAS DE SIMETRICOS
;;------------------------------------------------------------------------------------------------------
(defun regra-simetrico (peca pontos tabuleiro) 
"Se a casa escolhida tiver um número com dois dígitos diferentes,
por exemplo 57, então, em consequência, o número simétrico 75 é apagado do tabuleiro"
   ;;(format t "~@T~@T~@T~@TPeça ~A -> Casa ~A - Regra de Simetrico~%" peca pontos)
  (cond 
       ((= pontos 0) tabuleiro)
      (T (let* ((simetrico-pontos-destino (simetrico pontos));;numero simetricos de pontos
                (posicao-simetrico (obter-posicao tabuleiro simetrico-pontos-destino)));;obter a posicao do numero simetrico
                ;; (format t "~@T~@T~@T~@TPosicão ~A: ~A~%" simetrico-pontos-destino posicao-simetrico)
            (cond
                ((or (= simetrico-pontos-destino 0) (equal posicao-simetrico 'NIL)) tabuleiro);; for nula, retorna o tabuleiro
                 (T (substituir (first posicao-simetrico) (second posicao-simetrico) tabuleiro 'NIL)))))))

;;------------------------------------------------------------------------------------------------------
;;   CONSTRUTOR PRINCIPAL
;;------------------------------------------------------------------------------------------------------
(defun criar-no (estado pecas &optional (profundidade 0) (heuristica 0) (f 0) (pai NIL))
"Cria uma lista que representa um no que tem um estado. Este no pode tambem ter 
uma profundidade, heuristica, f e um no que o gerou"
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
"Funcao que retorna o no pai"
(sixth no))

;;------------------------------------------------------------------------------------------------------
;;   CONSTRUTOR NOVA PEÇA - METODOS SELECTORES(GETTERS) -  MODIFICADRES (SETTERS)
;;------------------------------------------------------------------------------------------------------
(defun set-pontos-peca (peca ponto)
"Função que recebe um no e valor de pontos, obtem a peca e retorna nova lista
  com pontos atualizados"
  (list (first peca) (+ (second peca) ponto)))

;;------------------------------------------------------------------------------------------------------
;;   OPERAÇÃO SUBSTITUIR PEÇA NO TABULEIRO
;;------------------------------------------------------------------------------------------------------
(defun substituir (x y tabuleiro &optional (valor 'NIL))
"Função que recebe dois índices, o tabuleiro e um valor (por default o valor é NIL).
A função deverá retornar o tabuleiro com a célula substituída pelo valor pretendido. 
Utilize a função substituir-posicao definida anteriormente" 
(cond   
    ((equal (posicao-validap x y) 'NIL) NIL)
    (T (substituir-posicao-aux x tabuleiro (substituir-posicao-aux y (linha x tabuleiro) valor)))))

(defun substituir-posicao-aux (y tabuleiro &optional (valor 'NIL))
"Função que recebe um índice, uma lista e um valor (por default o valor é NIL) e substitui pelo valor pretendido nessa posição."
(cond
     ((= y 0) (cons valor (cdr tabuleiro)))
     (T (cons (car tabuleiro) (substituir-posicao-aux (- y 1) (cdr tabuleiro) valor)))))

;;------------------------------------------------------------------------------------------------------
;;   PEÇA CAVALO ("T")
;;------------------------------------------------------------------------------------------------------
(defun obter-posicao (tabuleiro &optional (casa 'T))
"Função que recebe o elemento e tabuleiro devolvendo a posição (i j) 
caso não se encontre no tabuleiro deverá ser retornado NIL"
;(let ((posicao (posicao-aux casa tabuleiro)))
 ;     (cond   
  ;        ((and (equal casa 'T) (equal posicao 'NIL));;se for peca T e não houver peça
   ;        (substituir 1 1 tabuleiro 'NIL) ;;sera colocada na A1-G1
    ;       (list 1 1));;retorna a sua posicao definida 
     ;     (T (posicao-aux casa tabuleiro)))))
(posicao-aux casa tabuleiro))

(defun mover-peca (x y dx dy tabuleiro)
  (let* ((novo-tabuleiro (copy-tree tabuleiro))
         (novo-x (+ x dx))
         (novo-y (+ y dy))
         (peca (nth x (nth y tabuleiro)))
         (pontos-destino (nth novo-x (nth novo-y tabuleiro))))
         (when (posicao-validap novo-x novo-y)
               (setf (nth x (nth y novo-tabuleiro)) NIL
                     (nth novo-x (nth novo-y novo-tabuleiro)) peca)
               (if (equal (digitos-iguais pontos-destino) 'T);;se for digitos iguais
                   (regra-numero-duplo peca pontos-destino novo-tabuleiro);;regra duplo
                   (regra-simetrico peca pontos-destino novo-tabuleiro)))));;regra simetrico

;;------------------------------------------------------------------------------------------------------
;;   VALIDAÇÃO DE CASAS E MOVIMENTOS
;;------------------------------------------------------------------------------------------------------
(defun casa-disponivel (x y tabuleiro)
"Retorna o valor presente nessa célula do tabuleiro"	
(let ((disponivel (celula x y tabuleiro)))
		 (cond 
		    ((numberp disponivel) T)
		     (T NIL))))

(defun movimento-valido (x y tabuleiro)
"Função que recebe dois indices e tabuleiro para validar o movimento "
(if (and (<= 0 x 6)
        (<= 0 y 6)
        (= (apply '+
                  (mapcar (lambda (L)
                            (if (or (not (equal (nth 0 L) x))
                                    (not (equal (nth 1 L) y))) 
                                0
                                1))
                          tabuleiro)) 0)) T NIL)) 
 
(defun movimentos-solucao (no solucoes) 
"Função que retorna os movimentos necessários para chegar à solução"
(cond
((null (no-pai no)) solucoes)))
;;(T  (movimentos-solucao (no-pai no) (cons (no-movimento no) solucoes)))))
             
;;------------------------------------------------------------------------------------------------------
;;   OPERADORES
;;------------------------------------------------------------------------------------------------------
(defun HORIZONTAL (x y n movimento tabuleiro) 
 "Função que faz o movimento da Torre no sentido horizontal"
   (cond
        ((and (equal movimento 'ESQUERDA) 
              (equal (posicao-validap (- x n) (+ y 0)) 'T)
              (equal (casa-disponivel (- x n) (+ y 0) tabuleiro) 'T))
              (mover-peca x y (- n) 0 tabuleiro))
       
        ((and (equal movimento 'DIREITA) 
              (equal (posicao-validap (+ x n) (+ y 0)) 'T)
              (equal (casa-disponivel (+ x n) (+ y 0) tabuleiro) 'T))
              (mover-peca x y n 0 tabuleiro))
        (T NIL)))

(defun VERTICAL (x y n movimento tabuleiro) 
 "Função que faz o movimento da Torre no sentido vertical"
   (cond
        ((and (equal movimento 'CIMA) 
              (equal (posicao-validap (+ x 0) (- y n)) 'T)
              (equal (casa-disponivel (+ x 0) (- y n) tabuleiro) 'T))
              (mover-peca x y 0 (- n) tabuleiro))
       
        ((and (equal movimento 'BAIXO) 
              (equal (posicao-validap (+ x 0) (+ y n)) 'T)
              (equal (casa-disponivel (+ x 0) (+ y n) tabuleiro) 'T))
              (mover-peca x y 0 n tabuleiro))
        (T NIL)))

(defun operadores ()
"Funcao que lista todos os operadores existentes no dominio do problema"
 (list 'HORIZONTAL 'VERTICAL))

(defun posicao-validap (x y)
"Funçao para verificar se as coordenadas são validas"
;(format t "posicao valida: x:~A, y:~A~%" x y)
(and (>= x 0) (>= y 0) (< x 7) (< y 7)))

(defun movimentos ()
"Funcao que lista dos movimentos"
 (list 'CIMA 'BAIXO 'ESQUERDA 'DIREITA))

(defun jogadas-possiveis (x y tabuleiro) 
 "Funcao que determina todas as jogadas possiveis para um tipo de 
  peca num determinado tabuleiro, devolvendo-as numa lista"
  (let (result)
    (dolist (mov '(CIMA BAIXO ESQUERDA DIREITA) result)
      (dotimes (n 6)
        (when (or (not (equal (HORIZONTAL x y n mov tabuleiro) 'NIL));;se movimento for valido na horizontal
                  (not (equal (VERTICAL x y n mov tabuleiro) 'NIL)));se movimento for valido na vertical
          (push (list n mov) result))))));;cria uma lista de jogadas
;;------------------------------------------------------------------------------------------------------
;;  SUCESSORES
;;------------------------------------------------------------------------------------------------------
(defun gerar-sucessores (no operadores algoritmo &optional (heuristica NIL) (profundidade NIL))
"Funcao que devolve a lista de todos os sucessores de um determinado no passado como argumento"
  
         (format t "~%~@T~@T~@T~@T--- Tabuleiro inicial ---~%")
         (mostrar-tabuleiro (no-estado no))
(cond
    ((equal algoritmo 'dfs)
 (cond
  ((>= (no-profundidade no) profundidade) NIL) ;So se aplica os sucessores caso a profundidade do no seja inferior a profundidade maxima
  (T (remover-jogadas-invalidas (apply #'append (mapcar #'(lambda (operador)
                                            (aplicar-operador-no no operador heuristica)) operadores))))))
(T (remover-jogadas-invalidas (apply #'append (mapcar #'(lambda (operador)
                                            (aplicar-operador-no no operador heuristica)) operadores))))))

(defun aplicar-operador-no (no operador heuristica)
"Funcao que aplica apenas um operador a um determinado no. Consoante o operador passado por argumento esta funcao ira 
 determinar as jogadas possiveis para esse operador e ira criar um no (um sucessor) para cada uma dessas jogadas possives"
   (let* ((posicao-peca (obter-posicao (no-estado no)))
          (jogadas-possiveis (jogadas-possiveis (first posicao-peca) (second posicao-peca) (no-estado no))))
         (cond
              ((=(length jogadas-possiveis) 0) NIL);; se não houver jogada sai
              (T (mapcar #'(lambda (jogada) (format t "~%Jogada ~A~%" jogada)
                            (let* (
                                   (posicao-destino (posicao-destino posicao-peca jogada))
                                   (pontos-destino (pontos-destino posicao-destino (no-estado no)))
                                   (estado (funcall operador (first posicao-peca) (second posicao-peca) (first jogada) (second jogada) (no-estado no)))
                                   (pecas (set-pontos-peca (no-peca no) pontos-destino))
                                   (heuristica (funcall heuristica estado (no-peca no)))
                                   (profundidade (1+ (no-profundidade no)))
                                   (custo (+ profundidade heuristica)))
             (format t "~@T~@T~@T~@TJogada Destino: ~A~%" posicao-destino)
           (format t "~@T~@T~@T~@TPontos jogada destino: ~A~%" pontos-destino)
                              (criar-no estado pecas profundidade heuristica custo no))) jogadas-possiveis)))))

(defun posicao-destino (peca jogada)
 "Função que recebe uma jogada e retorna a posicao de destino"
  (cond 
      ((equal (second jogada) 'CIMA) (list (+ (first peca) 0) (- (second peca) (first jogada))))
      ((equal (second jogada) 'BAIXO) (list (+ (first peca) 0) (+ (second peca) (first jogada))))
      ((equal (second jogada) 'ESQUERDA) (list (- (first peca) (first jogada)) (+ (second peca) 0)))
      ((equal (second jogada) 'DIREITA) (list (+ (first peca) (first jogada)) (+ (second peca) 0)))
      (T NIL)))

(defun pontos-destino (destino tabuleiro)
 "Função que recebe uma jogada e retorna o numero de pontos da casa destino"
  (cond 
      ((null destino) 0);;jogada destino nula, 0 ponto
      (T (celula (first destino) (second destino) tabuleiro))))

(defun remover-jogadas-invalidas (sucessores)
"Remove todas as jogadas inválidas da lista de sucessores"
  (cond
      ((null sucessores) ())
      ((null (first (first sucessores))) (remover-jogadas-invalidas (cdr sucessores)))
      (T (cons (first sucessores) (remover-jogadas-invalidas (cdr sucessores))))))


;;------------------------------------------------------------------------------------------------------
;;   SOLUÇAO, OBJECTIVO e CALCULAR PONTOS
;;------------------------------------------------------------------------------------------------------
(defun no-solucao-p (no)
"Funcao que determina se um no e no objetivo ou nao, ou seja, se um no ja nao tem mais pecas para jogar ou se um no ja nao tem jogadas possiveis"
(let* ((tabuleiro (no-estado no))
      (pecas-no (no-peca no)))

 (cond
     ;;((and (not (null (jogadas-possiveis tabuleiro))) (> (first pecas-no) 0)) nil)
     (T T))))

(defun solucao-nao-encontrada () ;função que mostra uma mensagem de solução não encontrada
(format T "SOLUÇÃO NÃO ENCONTRADA"))	
										 
;;------------------------------------------------------------------------------------------------------
;;   FUNÇÕES DE CÁLCULO DE HEURISTICAS
;;------------------------------------------------------------------------------------------------------
(defun sem-heuristica (tabuleiro &optional (peca NIL)) ;uma função com uma heurística sem significado para uso nas pesquisas cegas
0
)

(defun heuristica1 (tabuleiro &optional (peca NIL))
"Calcula a heuristica h(x) = o(x)/m(x) em que
m(x) é a média por casa dos pontos que constam no tabuleiro x,
o(x) é o número de pontos que faltam para atingir o valor definido como objetivo."
;;(/ (pontos-atingir-objectivo peca) (ponto-medio-casa-tabuleiro tabuleiro)))
(/ 4 2))


(defun heuristica2 (tabuleiro &optional (peca NIL))
"Calcula a heuristica usando as funções auxiliares"

)