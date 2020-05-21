;;;; puzzle.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Autor:
;===========================================================================
;       FUNÇÕES PARA PROCURA NO ESPAÇO
;===========================================================================
(defun bfs (no-inicial algoritmo funcao-solucao funcao-sucessores heuristica &optional (abertos (list no-inicial)) (fechados nil) (tempo-inicial (get-universal-time)) (nos-expandidos 0)(nos-gerados 0))
  "Funcao que implementa o algoritmo de procura em largura"

  (cond
   ((null abertos) nil) ;Se a lista de abertos estiver vazia, falha
   (T (let* ((no-atual (first abertos))
             (abertos-sem-no-inicial (rest abertos))
             (fechados-com-no-inicial (cons no-atual fechados))
             (sucessores (remover-nos-repetidos (funcall funcao-sucessores no-atual 'bfs heuristica) abertos-sem-no-inicial fechados-com-no-inicial))
             (abertos-com-sucessores (abertos-bfs abertos-sem-no-inicial sucessores)))
        
        (format t "~%----------------BFS------------------------------~%") 
        (format t "~%No-Atual:")
        (mostrar-tabuleiro  (first no-atual))
        (format t "~%Abertos-sem-inicial: ~A ~%" abertos-sem-no-inicial)
        (format t "~%Fechados-com-no-inicial:")
        (mostrar-tabuleiro (first (first fechados-com-no-inicial)))
        (format t "~%Sucessores: ~A ~%" sucessores)
        (format t "~%Abertos-com-sucessores: ~A ~%" (first abertos-com-sucessores))
        (format t "~%---------------------------------------------~%~%")

        (cond
         ;Se existir uma solucao nos sucessores, vamos a procura dessa solucao
         ((existe-solucaop sucessores funcao-solucao) 
          (list (procura-no-objetivo sucessores funcao-solucao) sucessores algoritmo NIL tempo-inicial (1+ nos-expandidos) (+ nos-gerados (length sucessores))))

         (T (bfs (first abertos-com-sucessores) algoritmo funcao-solucao funcao-sucessores heuristica abertos-com-sucessores fechados-com-no-inicial tempo-inicial (1+ nos-expandidos) (+ nos-gerados (length sucessores)))))))))



;; dfs
(defun dfs (no-inicial algoritmo funcao-solucao funcao-sucessores profundidade heuristica &optional (abertos (list no-inicial)) (fechados nil) (tempo-inicial (get-universal-time)) (nos-expandidos 0) (nos-gerados 0))
  "Funcao que implementa o algoritmo de procura em profundidade"
  (cond
   ((null abertos) nil) ;Se abertos vazia falha
   (T (let* ((no-atual (first abertos))
             (abertos-sem-no-inicial (rest abertos))
             (fechados-com-no-inicial (cons no-atual fechados))) 
        
        ;;(format t "~%----------------DFS------------------------------~%") 
        ;;(format t "~%No-Atual:")
        ;;(mostrar-tabuleiro  (first no-atual))
        ;;(format t "~%Abertos-sem-inicial: ~A ~%" abertos-sem-no-inicial)
        ;;      (format t "~%Fechados-com-no-inicial:")
        ;;(mostrar-tabuleiro (first fechados-com-no-inicial))
        
        (cond
         ;Se a profundidade do no atual ultrapassar a profundidade maxima passa-se para o no seguinte
         ((> (no-profundidade no-atual) profundidade) 
          (dfs (first abertos-sem-no-inicial) funcao-solucao funcao-sucessores profundidade heuristica abertos-sem-no-inicial fechados-com-no-inicial tempo-inicial nos-expandidos nos-gerados))
         (T (let* ((sucessores (remover-nos-repetidos (funcall funcao-sucessores no-atual 'dfs heuristica profundidade) abertos-sem-no-inicial fechados-com-no-inicial))
                   (abertos-com-sucessores (abertos-dfs abertos-sem-no-inicial sucessores)))
              
              ;; (format t "~%Sucessores: ~A ~%" sucessores)
              ;;(format t "~%Abertos-com-sucessores: ~A ~%" abertos-com-sucessores)
              ;;(format t "~%---------------------------------------------~%~%")
              
              (cond
               ((existe-solucaop sucessores funcao-solucao)
                (list (procura-no-objetivo sucessores funcao-solucao) sucessores algoritmo profundidade tempo-inicial (1+ nos-expandidos) (+ nos-gerados (length sucessores))))
               (T (dfs (first abertos-com-sucessores) algoritmo funcao-solucao funcao-sucessores profundidade heuristica abertos-com-sucessores fechados-com-no-inicial tempo-inicial (1+ nos-expandidos) (+ nos-gerados (length sucessores))))))))))))


;;REMOVER-NOS-REPETIDOS which is referenced by BFS
;; remover-nos-repetidos
(defun remover-nos-repetidos (sucessores abertos fechados)
  "Funcao que retira sucessores que ja existam em abertos ou fechados"
  (apply #'append (mapcar #'(lambda (no)
                              (cond
                               ((or (no-existep no abertos) (no-existep no fechados)) nil)
                               (T (list no)))) sucessores)))


;; no-existep
(defun no-existep (no lista)
  "Funcao que verifica se um determinado no ja existe numa lista ou nao"
  (cond
   ((null lista) NIL)
   ((and 
     (equal (no-estado no) (no-estado (first lista))) ;Se o estado do no passado por argumento for igual ao estado do no que estamos a iterar neste momento
     (equal (no-peca no) (no-peca (first lista)))) T) ;Se as pecas do no passado por argumento forem iguais as pecas do no que estamos a iterar neste momento
   (T (no-existep no (rest lista)))))

;;EXISTE-SOLUCAOP which is referenced by BFS
(defun existe-solucaop (sucessores funcao-solucao)
  "Funcao que verifica se existe algum no objetivo numa determinada lista de sucessores"
  (eval (cons 'or (mapcar funcao-solucao sucessores))))

;;ABERTOS-BFS which is referenced by BFS
(defun abertos-bfs (abertos sucessores)
  "Funcao que constroi a lista de ABERTOS segundo o algoritmo bfs"
  (append abertos sucessores))

;; abertos-dfs
(defun abertos-dfs (abertos sucessores)
  "Funcao que constroi a lista de ABERTOS segundo o algoritmo dfs"
  (append sucessores abertos))

;;PROCURA-NO-OBJETIVO which is referenced by BFS
(defun procura-no-objetivo (sucessores funcao-solucao)
  "Funcao que procura o primeiro no objetivo de uma lista de sucessores"
  (cond
   ((null sucessores) nil)
   ((funcall funcao-solucao (first sucessores)) (first sucessores))
   (T (procura-no-objetivo (rest sucessores) funcao-solucao))))









