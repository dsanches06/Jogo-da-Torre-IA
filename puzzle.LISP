;;contador para contabilizar ocorrencia = 100
(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))

;;; FUNÇÕES DO TABULEIRO

(defun teste()
 "Função para para tabuleiro formatado"
 (imprime-tabuleiro (baralhar (inteiros-ate-77))))

(defun tabuleiro-inicial ()
  "Função para criar tabuleiro inicial já com inserido na posição a1 e j1"     
  (substituir 4 0 (gerar-tabuleiro-aux) T))

(defun gerar-tabuleiro-aux ()
 "Função auxiliar para gerar e baralhar o tabuleiro com casas de 0 a 77"
 (tabuleiro-aleatorio (baralhar (inteiros-ate-77))))
 
(defun tabuleiro-aleatorio (lista &optional(n 7))
  "Função que recebe uma é numa lista e criar sublistas de n elementos recorrendo à 
   função subseq que tem um comportamento semelhante ao substring para strings"
  (cond
   ((null lista) nil)
   (t (cons (subseq lista 0 n) 
      (tabuleiro-aleatorio (subseq lista n) n)))))

(defun baralhar (lista)
  "Função que recebe uma lista e irá mudar aleatoriamente os seus números. 
   Faça uma função recursiva e utilize a função nth, a função random e a função remover-se em que 
   a função deve remover o número da lista igual ao encontrado aleatoriamente"
   (let ((n (nth (random (length lista)) lista)))
         (cond 
             ((= (funcall *fn*) (length lista)) lista) ;;condição de paragem se for 77
             (t (baralhar (setf lista (cons n (remover-se #'(lambda (x) (= x n)) lista))))))))


(defun remover-numero-089 (n)
 "Função que recebe um numero e verifica se existe numero com 0, 8 e 9 e retorn t ou nil"
    (let ((p (primeiro-digito n)) 
          (u (ultimo-digito n)))
          (cond
             ((or (<= n 10) (= u 0)) T)
             ((or (= p 8) (= u 8)) T)
             ((or (= p 9) (= u 9)) T)
             (T NIL))))

(defun primeiro-digito (n)
 "Função que recebe um numero e retorna o primeiro digito"
   (cond
       ;; menor que zero, retorna n
      ((< n 10) n) 
       ;; maior ou igual que 10, n divide por 10
      (t (primeiro-digito (floor (/ n 10))))));;


(defun ultimo-digito (n)
 "Função que recebe um numero e retorna o ultimo digito"
  (mod n 10))
                                                                                           
(defun inteiros-ate-77 (&optional(n 77))
  "Função que recebe um número positivo n e cria uma lista com todos os números entre 0 
  (inclusivé) e o número passado como argumento (exclusivé). Por default o n é 77"
  (do* ((i 0 (+ i 1));; começa em 0
        (resultado (list i) (cons i resultado)))
       ((= i n) resultado)));; até n = 77


(defun remover-se (pred lista)
  "Função que recebe uma função e uma lista para remover os numeros duplicados"
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (t (cons (car lista) (remover-se pred (cdr lista))))))
  
(defun linha (i tabuleiro)
  "Função que recebe um índice e o tabuleiro e retorna uma lista que representa esta linha"
  (nth i tabuleiro))
	
(defun celula (i j tabuleiro)
  "Função que recebe dois índices e o tabuleiro e retorna o valor presente nessa célula do tabuleiro"
  (nth j (linha i tabuleiro)))

(defun coluna (j tabuleiro)
  "Função que recebe um índice e o tabuleiro e retorna uma lista que representa esta coluna"
  (mapcar #'(lambda (linha &aux (n-column (nth j linha))) n-column) tabuleiro))	

(defun substituir (i j tabuleiro &optional (valor 'NIL))
  "Função que recebe dois índices, o tabuleiro e um valor (por default o valor é NIL). A função deverá retornar o tabuleiro com a célula substituída pelo valor pretendido. Utilize a função substituir-posicao definida anteriormente" 
  (substituir-posicao-aux j tabuleiro (substituir-posicao-aux i (linha j tabuleiro) valor))) 
	
(defun substituir-posicao-aux (i tabuleiro &optional (valor 'NIL))                     
  "Função que recebe um índice, uma lista e um valor (por default o valor é NIL) e substitui pelo valor pretendido nessa posição."
  (cond
        ((= i 0) (cons valor (cdr tabuleiro)))
        (t (cons (car tabuleiro) (substituir-posicao-aux (- i 1) (cdr tabuleiro) valor)))))

(defun posicao-cavalo (tabuleiro)
  "Função que recebe o tabuleiro e devolve a posição (i j) em que se encontra o cavalo. Caso o cavalo não se encontre no tabuleiro deverá ser retornado NIL"
  (posicao-cavalo-aux T tabuleiro))

(defun posicao-cavalo-aux (cavalo tabuleiro &optional (inicio 0))
  "" 
  (cond ((null tabuleiro) nil)
        ((atom (first tabuleiro))
         (if (eql cavalo (first tabuleiro))
             (list inicio)
             (posicao-cavalo-aux cavalo (rest tabuleiro) (1+ inicio))))
        (t (let ((posicao (posicao-cavalo-aux cavalo (first tabuleiro) 0)))
             (if posicao
                 (cons inicio posicao)
                 (posicao-cavalo-aux cavalo (rest tabuleiro) (1+ inicio)))))))

(defun mover-cavalo (x y dx dy tabuleiro)
  "Função que irá mover o cavalo"
  (let ((novo-tabuleiro (copy-tree tabuleiro))
        (novo-x (+ x dx))
        (novo-y (+ y dy))
        (cavalo (nth x (nth y tabuleiro))))
    (setf (nth x (nth y novo-tabuleiro)) NIL
          (nth novo-x (nth novo-y novo-tabuleiro)) cavalo) novo-tabuleiro))

(defun validar-casap (x y tabuleiro)
  "Função que recebe dois indices e tabuleiro para validar se a casa destino está disponivel"
  (and (<= 0 x 9)
       (<= 0 y 9)
       (null (nth x (nth y tabuleiro)))))


(defun validar-movimento (x y board)
  "Função que recebe dois indices e tabuleiro para validar o movimento "
  (if (and (<= 0 x 9)
           (<= 0 y 9)
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
	  
;;; OPERADORES
(defun operadores ()
"Cria uma lista com todos os operadores do problema do jogo de cavalo"
  (list 'BAIXO-DIREITA 'BAIXO-ESQUERDA 'CIMA-DIREITA 'CIMA-ESQUERDA 'ESQUERDA-BAIXO 'ESQUERDA-CIMA 'DIREITA-CIMA 'DIREITA-BAIXO))

(defun BAIXO-DIREITA (x y tabuleiro) ;; mov baixo direita
"Função que recebe o tabuleiro e movimenta o cavalo para a posição 2 linhas abaixo e uma coluna ao lado direito, ou seja, o movimento que leva o cavalo para a casa de valor 10"
  (cond
      ((equal (validar-movimento (+ x 1) (+ y 2) tabuleiro) 0)
       (mover-cavalo x y 1 2 tabuleiro))
     (T nil)))

(defun BAIXO-ESQUERDA (x y tabuleiro) ;; mov baixo esquerda
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 20"
   (cond
       ((equal (validar-movimento (- x 1) (+ y 2) tabuleiro) 0) 
        (mover-cavalo x y -1 2 tabuleiro))
       (T nil)))

(defun CIMA-DIREITA (x y tabuleiro) ;; mov cima direita
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 30"
   (cond
       ((equal (validar-movimento (+ x 1) (- y 2) tabuleiro) 0) 
        (mover-cavalo x y 1 -2 tabuleiro))
       (T nil)))

(defun CIMA-ESQUERDA (x y tabuleiro) ;; mov cima esquerda
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 40"
   (cond
       ((equal (validar-movimento (- x 1) (- y 2) tabuleiro) 0) 
        (mover-cavalo x y -1 -2 tabuleiro))
       (T nil)))

(defun ESQUERDA-BAIXO (x y tabuleiro) ;; mov esquerda baixo
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 50"
   (cond 
       ((equal (validar-movimento (- x 2) (+ y 1) tabuleiro) 0) 
	(mover-cavalo x y -2 1 tabuleiro))
       (T nil)))

(defun ESQUERDA-CIMA (x y tabuleiro) ;;mov esquerda cima
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 60"
   (cond
       ((equal (validar-movimento (- x 2) (- y 1) tabuleiro) 0) 
	(mover-cavalo x y -2 -1 tabuleiro))
       (T nil)))

(defun DIREITA-CIMA (x y tabuleiro) ;; mov direita cima
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 70"
   (cond
       ((equal (validar-movimento (+ x 2) (- y 1) tabuleiro) 0) 
	(mover-cavalo x y 2 -1 tabuleiro))
       (T nil)))

(defun DIREITA-BAIXO (x y tabuleiro) ;; mov direita baixo
"Função que recebe o tabuleiro e movimenta o cavalo para posição
, ou seja, o movimento que leva o cavalo para a casa de valor 80"
   (cond
       ((equal (validar-movimento (+ x 1) (+ y 1) tabuleiro) 0) 
	(mover-cavalo x y 2 1 tabuleiro))
       (T nil)))



; FUNÇÕES DE CÁLCULO DE HEURISTICAS
(defun calcular-heuristica (tabuleiroActual tabuleiroSolucao tipoHeuristica)
""	
   (cond
       ((equal tipoHeuristica 'heuristica1) 
        (processar-jogo (tabuleiro1) (tabuleiro2)))
       (T 0)))
	   

(defun processar-jogo (tabuleiroActual tabuleiroSolucao)
"Função para processar o jogo"

)
	   
;;; NODE
(defun construir-no (mov x y tabuleiro noPai tipoHeuristica)
	(cond
		( (null tabuleiro) (list nil) )
		( T 
			(list 
				tabuleiro 
				(nth 1 noPai)
				(+ 1 (nth 2 noPai)) 
				(calcular-heuristica tabuleiro (nth 1 noPai) tipoHeuristica)
				(append (nth 10 noPai) (list (list mov x y)))
			)
		) 
	)
)

;;; MOVIMENTO
(defun movimento (mov no tipoHeuristica)
"Função movimento"
	(mapcar #'
		(lambda (x) 
			(construir-no 
				mov 
				(car x) 
				(car (cdr x)) 
				(funcall mov (car x) (car (cdr x)) (car no)) 
				no 
				tipoHeuristica))
			(posicao-cavalo no)))

;;;  SUCESSORES
(defun gerar-sucessores (no tipoHeuristica)
"Função que recebe o nó e tipo de heuristica"
  (apply #'append
		(mapcar #' 
			(lambda (x) (funcall 'movimento x no tipoHeuristica))
			'(operador-1 operador-2 operador-3 operador-4 operador-5 operador-6 operador-7 operador-8))))





