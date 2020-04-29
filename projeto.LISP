 ;===========================================================================
;       FUNÇÕES PARA CRIAR E MOSTRA OS DADOS DO TABULEIRO
;===========================================================================

;;; Função para mostrar o tabuleiro
(defun mostrar-tabuleiro (tabuleiro)
  "Mostra o tabuleiro"
  (terpri)
  (mapcar (lambda (x) (format T "~@T~@T~@T~@T~A ~%" x)) tabuleiro)
  (format NIL ""))

(defun mostrar-movimento (tabuleiro)
  "Mostra o tabuleiro"
  (terpri)
  (mapcar (lambda (x) (format T "~@T~@T~@T~@T~A ~%" x)) tabuleiro)
  (format NIL ""))

;;; Função para imprimir tabuleiro no ficheiro
(defun mostrar-tabuleiro-ficheiro (tabuleiro file)
  "permite imprimir tabuleiro no ficheiro"
  (format file "~%")
  (mapcar (lambda (x) (format file "	~A ~%" x)) tabuleiro)
  (format NIL ""))


(defun processar-dados (algoritmo problema estado pecas &optional (profundidade 0) (heuristica 0) (f 0) (pai NIL))
  ""                                 
 (let* (;;(no-heuristica (funcall heuristica problema pecas))
        (no (criar-no  estado 
                       pecas
                       profundidade
                       heuristica
                       f ;f do no raiz, que vai ser igual a heuristica porque g=0
                       pai)))

      (mostrar-tabuleiro no)
         (cond
           ((equal algoritmo-escolhido 'dfs)
            (mostrar-resultado (no problema algoritmo heuristica 1 profundidade)))
          (T (mostrar-resultado (no problema algoritmo heuristica 1 profundidade)))
     ;; (escrever-resultados (dfs no 'no-objetivo-p 'sucessores (operadores) profundidade-escolhida heuristica-escolhida) caminho))
     ;;(T (escrever-resultados (funcall algoritmo-escolhido no 'no-objetivo-p 'sucessores (operadores) heuristica-escolhida) caminho))
))) 


;===========================================================================
;       FUNÇÕES DE LEITURA E ESCRITA
;===========================================================================

;;; Função para definir o caminho para os ficheiros do projeto apartir da raiz
(defun diretoria-atual ()
  "Define o caminho para os ficheiros do projeto a partir da raiz"
  (let ((path "C:\\Users\\Toshiba\\Downloads\\Dados\\IA\\2019_2020\\ProjetoIA\\Fase I\\"))
    path))

;;; Função para ler problema apartir do ficheiro
(defun ler-problema (ficheiro)
 "Permite ler o problema apartir do ficheiro"
  (with-open-file (f ficheiro
                     :direction :input
                     :if-exists NIL
                     :if-does-not-exist :error)
    (read f)))


;;; Função que devolve um determinado problema
(defun obter-problema(tabuleiro)
  "permite obter um determinado problema"
  (nth (- tabuleiro 1) (ler-problema (concatenate 'string (diretoria-atual) "problemas.dat"))))           

;;; Função que mostra os resultados de um determinado problema
(defun mostrar-resultado (no problema algoritmo heuristica tempo limite &optional (nosGerados 0) (nosExpandidos 0) (nosDescartados 0))
  "mostra os resultados de um determinado problema"
  (terpri)	
  (format T "~%~@T~@T~@T~@T   +===========================+~%")
  (format T "~@T~@T~@T~@T   |    ESTATISTICAS GERAIS    |~%")
  (format T "~@T~@T~@T~@T   +===========================+~%")
  (mostrar-objetivo problema T)
  (format T "	~A" (mostrar-tabuleiro no))
  (format T "~%      --- TABULEIRO INICIAL ---~%" )
  (format T "	~A" (mostrar-tabuleiro (tabuleiro-inicial)))
  (format T "~%      --- MOVIMENTOS ---~%" )
 ;;mostrar movimento
  (format T "~%      --- TABULEIRO FINAL ---~%" )
  (format T "	~A" (mostrar-tabuleiro no))
  (format T "~%      --- RESULTADO ---~%")
  (format T "        Problema: ~A ~%" problema) 
  (format T "        Algoritmo: ~A ~%" algoritmo) 
  (format T "        Heuristica: ~A ~%" heuristica)
  (format T "	Limite/Limiar: ~A ~%" limite)
  (format T "	Nos Gerados: ~A ~%" (1- nosGerados))                            
  (format T "	Nos Descartados: ~A ~%" nosDescartados)
  (format T "	Nos Expandidos: ~A ~%" nosExpandidos)                                
 ;; (format T "	Penetrancia: ~A" (float (/ (nth 2 tabuleiro)(1- nosGerados)))) ;; terminar depois de obter sucessores
  (format T "	Tempo em Segundos (Aproximado): ~A ~%~%" ( * (float ( / (- (get-universal-time) tempo) internal-time-units-per-second)) 1000) )
  (format T "~@T~@T~@T~@T   +===========================+~%")
  (format T "~@T~@T~@T~@T   |     FIM DA ESTATISTICA    |~%")
  (format T "~@T~@T~@T~@T   +===========================+~%~%")
 
  (with-open-file (file (concatenate 'string (diretoria-atual) "resultados.dat")  
		:direction :output
		:if-exists :append
		:if-does-not-exist :create)
                (format file "~%~@T~@T~@T~@T   +===========================+~%")
                (format file "~@T~@T~@T~@T   |    ESTATISTICAS GERAIS    |~%")
                (format file "~@T~@T~@T~@T   +===========================+~%")
                (mostrar-objetivo problema file)
                (format file "	~A" (mostrar-tabuleiro-ficheiro no file))
                (format file "~%      --- TABULEIRO INICIAL ---~%" )
                (format file "	~A" (mostrar-tabuleiro-ficheiro no file))
                (format file "~%      --- MOVIMENTOS ---~%" )
                ;;mostrar movimento
                (format file "~%      --- TABULEIRO FINAL ---~%" )
                (format file "	~A" (mostrar-tabuleiro-ficheiro no file))
                (format file "~%      --- RESULTADO ---~%")
                (format file "        Problema: ~A ~%" problema) 
                (format file "        Algoritmo: ~A ~%" algoritmo) 
                (format file "        Heuristica: ~A ~%" heuristica)
                (format file "	    Limite/Limiar: ~A ~%" limite)
                (format file "	    Nos Gerados: ~A ~%" (1- nosGerados))                  
                (format file "        Nos Descartados: ~A ~%" nosDescartados)
                (format file "	    Nos Expandidos: ~A ~%" nosExpandidos)                                
             ;; (format file "	Penetrancia: ~A" (float (/ (nth 2 tabuleiro)(1- nosGerados)))) ;; terminar depois de obter sucessores
                (format file "        Tempo em Segundos (Aproximado): ~A ~%~%" ( * (float ( / (- (get-universal-time) tempo) internal-time-units-per-second)) 1000) )
                (format file "~@T~@T~@T~@T   +===========================+~%")
                (format file "~@T~@T~@T~@T   |     FIM DA ESTATISTICA    |~%")
                (format file "~@T~@T~@T~@T   +===========================+~%~%")
		(format file "================================================================================================================~%~%")))
             
;===========================================================================
;       FUNÇÕES PARA INTER-ACÇÃO DO UTILIZADOR
;===========================================================================
(defun ler-caminho (&optional (path (diretoria-atual))) 
  "Função para ler o diretorio onde se encontra os ficheiros do projeto" 
   (cond 
       ((null path) (format T "~@T~@T~@T~@T Introduza o caminho ate ao diretorio do projeto:~%")     
	   (ler-caminho (read-line)))
	   (T path)))




(defun iniciar ()
  "Permite para iniciar o programa"
   (let ((path (ler-caminho)))
        (carregar-ficheiros path)))

(defun carregar-ficheiros (path)
  "Permite que compila e carregar os ficheiros do projeto para ser executados"
  (progn 
	;(compile-file (concatenate 'string path "puzzle.lisp"))
        ;(compile-file (concatenate 'string path "procura.lisp"))
	(load (concatenate 'string path "puzzle.ofasl"))
        (load (concatenate 'string path "procura.ofasl"))
        (menu-principal)))

;;; Função menu principal
(defun menu-principal ()
  "Apresenta o menu iniciar com as opções"
  (terpri)
  (format T "~@T~@T~@T~@T +============================+~%")
  (format T "~@T~@T~@T~@T |         PROJECTO IA        |~%")
  (format T "~@T~@T~@T~@T +============================+~%")
  (format T "~@T~@T~@T~@T |     1. Resolver Puzzle     |~%")
  (format T "~@T~@T~@T~@T |     2. Regras do Puzzle    |~%")   
  (format T "~@T~@T~@T~@T |     0. Sair                |~%")
  (format T "~@T~@T~@T~@T +============================+~%")
  (format T "~@T~@T~@T~@T Escolha: ")
  (let ((opcao (read)))
    (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
    (cond
     ((equal opcao '1) (resolver-problema))
     ((equal opcao '2) (mostrar-regras))
     ((equal opcao '0) (menu-sair) NIL)
     (T (format T "~%~@T~@T~@T~@T *ATENÇÃO*")
        (format T "~%~@T~@T~@T~@T Opção Inválida: Introduza um numéro válido")
        (terpri) 
        (menu-principal))))) 


;; Função que apresenta a escolha do problema a resolver
(defun resolver-problema ()
  "Permite a escolha dos problemas a resolver"
  (terpri)
  (format T "~@T~@T~@T~@T +================================+~%")
  (format T "~@T~@T~@T~@T |        Lista de Problemas      |~%")
  (format T "~@T~@T~@T~@T +================================+~%")
  (format T "~@T~@T~@T~@T |         1. Tabuleiro A         |~%")
  (format T "~@T~@T~@T~@T |         2. Tabuleiro B         |~%")
  (format T "~@T~@T~@T~@T |         3. Tabuleiro C         |~%")
  (format T "~@T~@T~@T~@T |         4. Tabuleiro D         |~%")
  (format T "~@T~@T~@T~@T |         5. Tabuleiro E         |~%")
  (format T "~@T~@T~@T~@T |         6. Tabuleiro F         |~%")
  (format T "~@T~@T~@T~@T |         7. Tabuleiro G         |~%")
  (format T "~@T~@T~@T~@T |         0. Sair                |~%")
  (format T "~@T~@T~@T~@T +================================+~%")
  (format T "~@T~@T~@T~@T Escolha: ")
  (let ((opcao (read)))
    (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
    (cond
     ((equal opcao '1) (escolher-algoritmo (obter-problema 1) 'A))
     ((equal opcao '2) (escolher-algoritmo (obter-problema 2) 'B))
     ((equal opcao '3) (escolher-algoritmo (obter-problema 3) 'C))
     ((equal opcao '4) (escolher-algoritmo (obter-problema 4) 'D))
     ((equal opcao '5) (escolher-algoritmo (obter-problema 5) 'E))
     ((equal opcao '6) (escolher-algoritmo (obter-problema 6) 'F))
     ((equal opcao '7) (escolher-algoritmo (obter-problema 7) 'G))
     ((equal opcao '0) (menu-principal))
     (T (format T "~%~@T~@T~@T~@T *ATENÇÃO*")
        (format T "~%~@T~@T~@T~@T Opção Inválida: Introduza um numéro válido")
        (terpri)
        (resolver-problema)))))

;;; Função que apresenta o objetivo do problema escolhido
(defun mostrar-objetivo (problema tipo) 
  "Permite mostrar o objetivo do problema para escrita em consola e file"
  (cond
     ((equal problema 'A) (format tipo "~%      --- PROBLEMA ~A: Objectivo 100 pontos ---~%" problema))
     ((equal problema 'B) (format tipo "~%      --- PROBLEMA ~A: Objectivo 200 pontos ---~%" problema))
     ((equal problema 'C) (format tipo "~%      --- PROBLEMA ~A: Objectivo 300 pontos ---~%" problema))
     ((equal problema 'D) (format tipo "~%      --- PROBLEMA ~A: Objectivo 425 pontos ---~%" problema))
     ((equal problema 'E) (format tipo "~%      --- PROBLEMA ~A: Objectivo 525 pontos ---~%" problema))
     ((equal problema 'F) (format tipo "~%      --- PROBLEMA ~A: Objectivo 1000 pontos ---~%" problema))))		
		
;;; Função que apresenta a escolha do algoritimo a usar
(defun escolher-algoritmo (tabuleiro problema)
  "Permite escolher o algoritimo para resolver o problema"
  (mostrar-objetivo problema T)
  (format T "~A" (mostrar-tabuleiro tabuleiro))
  (terpri)
  (format T "~@T~@T~@T~@T +===============================+~%")
  (format T "~@T~@T~@T~@T |        Lista de Algoritmos    |~%")
  (format T "~@T~@T~@T~@T +===============================+~%")
  (format T "~@T~@T~@T~@T |         1. BFS                |~%")
  (format T "~@T~@T~@T~@T |         2. DFS                |~%")
  (format T "~@T~@T~@T~@T |         3. A*                 |~%")
  (format T "~@T~@T~@T~@T |         4. IDA*               |~%")
  (format T "~@T~@T~@T~@T |         0. Sair               |~%")
  (format T "~@T~@T~@T~@T +===============================+~%")
  (format T "~@T~@T~@T~@T Escolha: ")
  (let ((opcao (read)))
    (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
    (cond
     ((equal opcao '1) (busca-largura tabuleiro 'BFS problema))
     ((equal opcao '2) (busca-profundidade tabuleiro 'DFS problema))
     ((equal opcao '3) (escolher-heuristica tabuleiro 'A* problema))
     ((equal opcao '4) (escolher-heuristica tabuleiro 'IDA* problema))
     ((equal opcao '0) (resolver-problema))
     (T (format T "~%~@T~@T~@T~@T *ATENÇÃO*")
        (format T "~%~@T~@T~@T~@T Opção Inválida: Introduza um numéro válido") 
        (terpri)
        (escolher-algoritmo tabuleiro problema))))) 

;;; Função para busca em largura
(defun busca-largura (tabuleiro algoritmo problema)
  "para busca em largura"
  (format T "~%~@T~@T~@T~@T Procura Generica - Largura")
  (processar-dados algoritmo problema tabuleiro 'T)
  (iniciar))

;;; Função para inserir a profundidade
(defun busca-profundidade (tabuleiro algoritmo problema)
  "para inserir a profundidade para busca em profundidade"
  (terpri)
  (format T "~@T~@T~@T~@T Até que profundidade: ")
  (let ((profundidade (read)))
    (cond
     ((numberp profundidade)
      (cond
       ((> profundidade 0) 
        (processar-dados algoritmo problema tabuleiro 'T profundidade)
        (iniciar))
       (T (busca-profundidade tabuleiro algoritmo problema))))
     (T (busca-profundidade tabuleiro algoritmo problema)))))


;;; Função para escolher heuristica
(defun escolher-heuristica (tabuleiro algoritmo problema)
  "Permite escolher a heuristica"
  (terpri)
  (format T "~@T~@T~@T~@T +=============================+~%")
  (format T "~@T~@T~@T~@T |     Lista de Heuristica     |~%")
  (format T "~@T~@T~@T~@T +=============================+~%")
  (format T "~@T~@T~@T~@T |      1. Heurística Dada     |~%")
  (format T "~@T~@T~@T~@T |      2. Heurística Criada   |~%")
  (format T "~@T~@T~@T~@T |      0. Sair                |~%")
  (format T "~@T~@T~@T~@T +=============================+~%")
  (format T "~@T~@T~@T~@T Escolha: ")
  (let ((opcao (read)))
    (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
    (cond
     ((equal opcao '1)
      (cond
       ((equal algoritmo 'IDA*)                              
        (processar-dados algoritmo problema tabuleiro 'T 0 'heuristica1)
        (iniciar))
       ((equal algoritmo 'A*)                            
        (processar-dados algoritmo problema tabuleiro 'T 0 'heuristica1)
        (iniciar))))
     ((equal opcao '2)
      (cond
       ((equal algoritmo 'IDA*)                                               
        (processar-dados algoritmo problema tabuleiro 'T 0 'heuristica2)
        (iniciar))
       ((equal algoritmo 'A*)                            
        (processar-dados algoritmo problema tabuleiro 'T 0 'heuristica1)
        (iniciar))))
     ((equal opcao '0) 
      (terpri) 
      (resolver-problema))
     (T (format T "~%~@T~@T~@T~@T *ATENÇÃO*")
        (format T "~%~@T~@T~@T~@T Opção Inválida: Introduza um numéro válido")
        (terpri)
        (escolher-heuristica tabuleiro algoritmo problema)))))



;; Função para mostrar as regras do puzzle
(defun mostrar-regras ()
  "Mostra as regras do puzzle" 
  (terpri)
  (format T "
    +========================= Regras do Puzzle ===========================================+
    |                                                                                      |
    |                                                                                      |
    |                                                                                      |
    +======================================================================================+~%")
  (terpri) 
  ;;apresenta o menu principal
  (menu-principal))

;; Função para terminar o programa
(defun menu-sair ()
  "Permite mostrar mensagem quando o programa termina"
  (terpri)	
  (format T "~@T~@T~@T~@T+============================+~%")
  (format T "~@T~@T~@T~@T|     PROGRAMA TERMINADO     |~%")
  (format T "~@T~@T~@T~@T+============================+~%"))


 