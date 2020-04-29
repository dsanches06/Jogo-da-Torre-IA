 
(defun procura-generica (no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores f-heuristica &optional (abertos (list no-inicial)) (fechados nil) (tempo-inicial (get-universal-time)))
"Permite procurar a solucao de um problema usando a procura no espaÃ§o de estados. A partir de um estado inicial,
 de uma funcao que gera os sucessores e de um dado algoritmo. De acordo com o algoritmo pode ser usada um limite
 de profundidade, uma heuristica e um algoritmo de ordenacao
"
	(cond
		((null abertos) nil); nao existe solucao ao problema
		((funcall f-solucao (car abertos))  (list (car abertos) (- (get-universal-time) tempo-inicial))); se o primeiro dos abertos e solucao este no e devolvido com o tempo de exe
		(( existep (first abertos) fechados f-algoritmo) (procura-generica no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores (cdr abertos) fechados)); se o no ja existe nos fechados e ignorado
		(T 
			(let* ((lista-sucessores (funcall f-sucessores (first abertos)  lista-operadores f-algoritmo prof-max f-heuristica))
			      (solucao (existe-solucao lista-sucessores f-solucao f-algoritmo)));verifica se existe uma solucao nos sucessores para o dfs
		          (cond
		            (solucao (list solucao (- (get-universal-time) tempo-inicial))); devolve a solucao, com o tempo de execucao
					(T (procura-generica no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores f-heuristica (funcall f-algoritmo (rest abertos) lista-sucessores) (cons (car abertos) fechados))); expande a arvore se o primeiro dos abertos nao for solucao
					)
			)
		)
	)
)

(defun existep (abertos fechados algoritmo)

)

;; existe-solucao
(defun existe-solucao (lista f-solucao f-algoritmo)
"Verifica se existe uma solucao ao problema numa lista de sucessores para o algoritmo dfs"
  (cond
	 ((not (eql f-algoritmo 'dfs)) nil)
     ((null lista) nil)
     ((funcall f-solucao (car lista)) (car lista))
     (T (existe-solucao (cdr lista) f-solucao f-algoritmo)))
)

(defun no-visitadop (no nosFechados)
 "Corre todos os nós fechados até encontrar um que aceite as condições, senão chega ao fim e devolve nil"
	(cond
		; Se não existir nós na lista de fechados retorna nil
		( (null nosFechados) nil )
		
		; Se o tabuleiro for igual e a profundidade maior ou igual então já foi visitado (caso o nó já tenha sido expandido mas não tenha atingigo um g maior não se considera visitado)
		( (and (igual? (car no) (caar nosFechados))  (>= (nth 9 no) (cadar nosFechados))) T )
		
		(T (no-visitadop no (cdr nosFechados)))
	)
)


(defun largura (nosSucessores restantesNosAbertos)
" Coloca os nós sucessores no inicio da lista de abertos"
	(list (append (car restantesNosAbertos) nosSucessores)))

(defun profundidade (nosSucessores restantesNosAbertos)
"Coloca os nós sucessores no fim da lista de abertos"
	(list (append nosSucessores (car restantesNosAbertos))))

(defun A* (nosSucessores restantesNosAbertos)
"Coloca os nós sucessores na lista de abertos ordenados"
  (nosSucessores restantesNosAbertos))

 
(defun procuraIDA* (sucessores heuristica limiar proximoLimiar &optional (tempo (get-universal-time)) (nosGerados 0) (nosExpandidos 0) (nosDescartados 0))
"Função auxiliar da pesquisa IDA* (percorre a arvore do problema recurcivamente )"
)

(defun IDA* (no heuristica limiarBase proximoLimiar &optional (tempo (get-universal-time)) (nosGerados 0) (nosExpandidos 0) (nosDescartados 0))
"Função de procura em IDA"
)