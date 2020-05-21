;;;; puzzle.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Autor:
;===========================================================================
;       FUNÇÕES PARA CRIAR E MOSTRA OS DADOS DO TABULEIRO
;===========================================================================
;;; Função para mostrar o tabuleiro
(defun mostrar-tabuleiro (tabuleiro)
  "Mostra o tabuleiro"
  (format t "~%")
  (mapcar (lambda (x) (format T "~@T~@T~@T~@T~@T ~A ~%" x)) tabuleiro)
  (format NIL ""))

(defun mostrar-movimento (sucessores)
  "Mostra os movimentos"
  (mapcar #'(lambda (x) (format t "~A" (mostrar-tabuleiro (first x)))) sucessores))

;;; Função para imprimir tabuleiro no ficheiro
(defun mostrar-tabuleiro-ficheiro (tabuleiro file)
  "permite imprimir tabuleiro no ficheiro"
  (format file "~%")
  (mapcar (lambda (x) (format file "	~A ~%" x)) tabuleiro)
  (format NIL ""))

;===========================================================================
;       FUNÇÕES DE LEITURA E ESCRITA
;===========================================================================
(defun diretoria-atual ()
  "Define o caminho para os ficheiros do projeto a partir da raiz"
  (let ((path "C:\\Users\\Toshiba\\Downloads\\Dados\\IA\\2019_2020\\ProjetoIA\\Fase I\\"))
    path))

(defun resultado-aux (resultado)
 (format t "~%No-objetivo:~A~%" (first resultado))
 (format t "~%No-Pai:~A~%"  (no-pai (first resultado)))

 (format t "~%Sucessores:~A~%" (second resultado))
 (format t "~%Algoritmo:~A~%" (third resultado))
 (format t "~%Profundidade:~A~%" (fourth resultado))
 (format t "~%Tempo:~A~%" (fifth resultado))
 (format t "~%No-expandido:~A~%" (sixth resultado))
 (format t "~%No-gerado:~A~%" (seventh resultado))
)



(defun escrever-resultados (resultado)
  "Funcao que ira imprimir o resultado para o ecra de uma forma mais clara bem como escrever as estatisticas num ficheiro estatisticas.dat"
  
  (format t "Resultados:~%")
 (format t "~A~%" resultado)
 (resultado-aux resultado)
  ;,(format t "~%No-Objetivo:~%")
 ;; (format t "~A~%" (first resultado))
  ;;(format t "~%Sucessores:~%")
 ;, (format t "~A~%" (second resultado))
 ;; (mostrar-movimento (second resultado))
  
  (cond
   ((not (null resultado))
    (let* ((no-objetivo (first resultado))
           (estado-tabuleiro (first (first no-objetivo)))
           (pecas (no-peca no-objetivo))
           (heuristica (no-heuristica no-objetivo))
           (custo-uniforme (no-f no-objetivo))
           (pai (no-pai no-objetivo))
           (problema (second (first pai)))
           (objetivo (third (first pai)))
           (sucessores (second resultado))
           (algoritmo (third resultado))
           (profundidade (fourth resultado))
           (tempo (fifth resultado))
           (nos-expandidos (sixth resultado))
           (nos-gerados (seventh resultado)))
      (progn
        (format T "~%~@T~@T~@T~@T   +===========================+~%")
        (format T "~@T~@T~@T~@T   |    ESTATISTICAS GERAIS    |~%")
        (format T "~@T~@T~@T~@T   +===========================+~%")
        (format T "~%~@T~@T~@T~@T --- NÓ INICIAL ---~%")
        (mostrar-tabuleiro (first pai))
        (format T "~%~@T~@T~@T~@T --- MOVIMENTOS ---~%")
        (mostrar-movimento sucessores)
        (format T "~%~@T~@T~@T~@T --- NÓ OBJETIVO ---~%")
        (mostrar-tabuleiro estado-tabuleiro)
        (format T "~%~@T~@T~@T~@T --- RESULTADOS ---~%")
        (format T "~@T~@T~@T~@T   Algoritmo: ~A ~%" algoritmo)
        (format T "~@T~@T~@T~@T   Problema: ~A ~%" problema) 
        (format T "~@T~@T~@T~@T   Objetivo: ~A ~%" objetivo) 
        (format T "~@T~@T~@T~@T   Peça: ~A ~%" (first pecas))
        (format T "~@T~@T~@T~@T   Pontuação: ~A ~%" (second pecas))
        (format T "~@T~@T~@T~@T   Profundidade: ~A ~%" profundidade)
        (format T "~@T~@T~@T~@T   Heuristica: ~A ~%" heuristica)
        (format T "~@T~@T~@T~@T   Custo Uniforme: ~A ~%" custo-uniforme)
        (format T "~@T~@T~@T~@T   Nós Gerados: ~A ~%" nos-gerados)    
        (format T "~@T~@T~@T~@T   Nós Expandidos: ~A ~%" nos-expandidos)  
        ;;(format T "~@T~@T~@T~@T   Penetrancia: ~A" (penetrancia profundidade nos-gerados))
        ;;(format T "~@T~@T~@T~@T   Fator de Ramificação: ~A ~%" (fator-ramificacao profundidade nos-gerados))
        (format T "~@T~@T~@T~@T   Tempo em Segundos: ~A ~%" (- (get-universal-time) tempo))
        (format T "~@T~@T~@T~@T   No Pai:")
        (mostrar-tabuleiro (first pai))
        (format T "~%~@T~@T~@T~@T   +===========================+~%")
        (format T "~@T~@T~@T~@T   |     FIM DA ESTATISTICA    |~%")
        (format T "~@T~@T~@T~@T   +===========================+~%~%")
        
        ;;(escrever-estatisticas-ficheiro path tabuleiro pecas profundidade heuristica-resultado custo-resultado tempo-inicial nos-expandidos nos-gerados)
        ))) 
   (T NIL)))

;;;	(format file "================================================================================================================~%~%"))

;===========================================================================
;       FUNÇÕES PARA INTER-ACÇÃO DO UTILIZADOR
;===========================================================================
(defun iniciar ()
  "Permite para iniciar o programa"
  (let ((path (ler-path)))
    (carregar-ficheiros path)))

(defun ler-path (&optional (path (diretoria-atual))) 
  "Função para ler o diretorio onde se encontra os ficheiros do projeto" 
  (cond 
   ((null path) (format T "~@T~@T~@T~@T Introduza o caminho ate ao diretorio do projeto:~%")     
    (ler-path (read-line)))
   (T path)))

(defun carregar-ficheiros (path)
  "Permite que compila e carregar os ficheiros do projeto para ser executados"
  (progn 
    ;(compile-file (concatenate 'string path "puzzle.lisp"))
    ;(compile-file (concatenate 'string path "procura.lisp"))
    (load (concatenate 'string path "puzzle.ofasl"))
    (load (concatenate 'string path "procura.ofasl"))
    (menu-principal path)))

;;; Função menu principal
(defun menu-principal (path)
  "Apresenta o menu iniciar com as opções"
  (progn
    (format T "~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T |         PROJECTO IA         |~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T |     1 - Resolver Puzzle     |~%")
    (format T "~@T~@T~@T~@T |     2 - Regras do Puzzle    |~%")   
    (format T "~@T~@T~@T~@T |     0 - Sair                |~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let ((opcao (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
      (cond
       ((not (numberp opcao)) (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0, 1, 2] ~%") (menu-principal path))
       ((= opcao 1) (resolver-puzzle path))
       ((= opcao 2) (mostrar-regras path))
       ((= opcao 0) (menu-sair) NIL)
       (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0, 1, 2] ~%") (menu-principal path)))))) 


(defun resolver-puzzle (path)
  "Funcao que ira iniciar a aplicacao, pedindo ao utilizador para que escolha o tabuleiro que pretenda que seja o no inicial, o algoritmo que pretende utilizar e a profundidade maxima que pretende para o algoritmo dfs"
  (let* ((tabuleiro (ler-tabuleiro path))
         (algoritmo (ler-algoritmo (first tabuleiro) path (second tabuleiro)))
         (profundidade (cond
                        ((equal algoritmo 'dfs) (ler-profundidade))
                        (T NIL)))
         (heuristica (cond
                      ((or (equal algoritmo 'a*) (equal algoritmo 'ida*)) (ler-heuristica path))
                      (T 'sem-heuristica)))
         (pecas-no-raiz (list 'BRANCA 0))
         (heuristica-no-raiz (funcall heuristica tabuleiro pecas-no-raiz))
         (no (criar-no tabuleiro pecas-no-raiz 0 heuristica-no-raiz heuristica-no-raiz NIL)))
    (cond
     ((equal algoritmo 'dfs)
      (escrever-resultados (dfs no algoritmo 'no-solucao-p 'gerar-sucessores profundidade heuristica)) (iniciar))
     (T (escrever-resultados (funcall algoritmo no algoritmo 'no-solucao-p 'gerar-sucessores heuristica))
        (iniciar)))))

;; Função que apresenta a escolha do problema a resolver
(defun ler-tabuleiro (path)
  "Permite a escolha dos problemas a resolver"
  (progn
    (format T "~%")
    (format T "~@T~@T~@T~@T +===================================+~%")
    (format T "~@T~@T~@T~@T |          Lista de Problemas       |~%")
    (format T "~@T~@T~@T~@T +===================================+~%")
    (format T "~@T~@T~@T~@T |         1 - Tabuleiro A [100]     |~%")
    (format T "~@T~@T~@T~@T |         2 - Tabuleiro B [200]     |~%")
    (format T "~@T~@T~@T~@T |         3 - Tabuleiro C [300      |~%")
    (format T "~@T~@T~@T~@T |         4 - Tabuleiro D [425]     |~%")
    (format T "~@T~@T~@T~@T |         5 - Tabuleiro E [525]     |~%")
    (format T "~@T~@T~@T~@T |         6 - Tabuleiro F [1000]    |~%")
    (format T "~@T~@T~@T~@T |         7 - Tabuleiro G [ORAL]    |~%")
    (format T "~@T~@T~@T~@T +===================================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let ((opcao (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
      (with-open-file (file (concatenate 'string path "problemas.dat") :direction :input)
        (let ((tabuleiroA (list (ler-tabuleiro-ficheiro file) 'A 100))
              (tabuleiroB (list (ler-tabuleiro-ficheiro file) 'B 200))
              (tabuleiroC (list (ler-tabuleiro-ficheiro file) 'C 300))
              (tabuleiroD (list (ler-tabuleiro-ficheiro file) 'D 400))
              (tabuleiroE (list (ler-tabuleiro-ficheiro file) 'E 500))
              ;;(tabuleiroF (list (ler-tabuleiro-ficheiro file) 'F 1000))
              ;;(tabuleiroG (list (ler-tabuleiro-ficheiro file) 'G 'NIL')) ;Problema G da avaliacao oral
              
              )
          (cond
           ((not (numberp opcao)) (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0, 1, 2, 3, 4, 5, 6, 7] ~%") 
            (ler-tabuleiro path))
           
           ((= opcao 1) tabuleiroA)
           ((= opcao 2) tabuleiroB)
           ((= opcao 3) tabuleiroC)
           ((= opcao 4) tabuleiroD)
           ((= opcao 5) tabuleiroE)
           ;;((= opcao 6) tabuleiroF)
           ;((= opcao 7) tabuleiroG)
           (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0, 1, 2, 3, 4, 5, 6, 7] ~%") (ler-tabuleiro path))))))))	

;; ler-tabuleiro-ficheiro
(defun ler-tabuleiro-ficheiro (file)
  "Funcao que permite ler um tabuleiro de um ficheiro"
  (let ((linha (read file nil :fim)))
    (cond
     ((equal linha 'end) (close file) NIL)
     ((not (equal linha 'break)) (cons linha (ler-tabuleiro-ficheiro file)))
     ((equal linha 'break) NIL))))		

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
(defun ler-algoritmo (tabuleiro path problema)
  "Permite escolher o algoritimo para resolver o problema"
  (progn
    (mostrar-objetivo problema T)
    (mostrar-tabuleiro tabuleiro)
    (format T "~%")
    (format T "~@T~@T~@T~@T +=========================================+~%")
    (format T "~@T~@T~@T~@T |            Lista de Algoritmos          |~%")
    (format T "~@T~@T~@T~@T +=========================================+~%")
    (format T "~@T~@T~@T~@T |     1 - Busca em Largura (bfs)          |~%")
    (format T "~@T~@T~@T~@T |     2 - Busca em Profundidade (dfs)     |~%")
    (format T "~@T~@T~@T~@T |     3 - A*                              |~%")
    (format T "~@T~@T~@T~@T |     4 - IDA*                            |~%")
    (format T "~@T~@T~@T~@T +=========================================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let ((opcao (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
      (cond
       ((not (numberp opcao)) (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0, 1, 2, 3, 4] ~%") 
        (ler-algoritmo tabuleiro path problema))
       ((= opcao 1) 'bfs)
       ((= opcao 2) 'dfs)
       ((= opcao 3) 'a*)
       ((= opcao 4) 'ida*)
       (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0, 1, 2, 3, 4] ~%") 
          (ler-algoritmo tabuleiro path problema))))))

;; ler-profundidade
(defun ler-profundidade (&optional (resposta NIL))
  "Funcao que ira questionar o utilizador sobre qual a profundidade maxima que pretende para o algortimo Busca em profundidade (dfs)"
  (cond
   ((null resposta) 
    (format T "~%~@T~@T~@T~@T Insira a profundidade maxima que deseja para o algoritmo: ") 
    (ler-profundidade (read)))
   ((not (numberp resposta)) 
    (format T "~%~@T~@T~@T~@T Por favor insira um numero maior do que 0 ~%") (ler-profundidade))
   ((<= resposta 0) 
    (format T "~%~@T~@T~@T~@T Por favor insira um numero maior do que 0 ~%") (ler-profundidade))
   (T resposta)))


;;; Função para escolher heuristica
(defun ler-heuristica (path)
  "Permite escolher a heuristica"
  (progn
    (format T "~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T |     Lista de Heuristica     |~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T |      1 - Heurística1        |~%")
    (format T "~@T~@T~@T~@T |      2 - Heurística2        |~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let ((opcao (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
      (cond
       ((not (numberp opcao)) (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0, 1, 2] ~%") (ler-heuristica path))
       ((= opcao 1) 'heuristica1)
       ((= opcao 2) 'heuristica2)
       (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0,1, 2] ~%") 
          (ler-heuristica path))))))

;; Função para mostrar as regras do puzzle
(defun mostrar-regras (path)
  "Mostra as regras do puzzle" 
  (format T "~%")
  (format T "
    +========================= Regras do Puzzle ===========================================+
    |                                                                                      |
    |                                                                                      |
    |                                                                                      |
    +======================================================================================+~%")
  (format T "~%")
  (menu-principal path))

;; Função para terminar o programa
(defun menu-sair ()
  "Permite mostrar mensagem quando o programa termina"
  (format T "~%")	
  (format T "~@T~@T~@T~@T+============================+~%")
  (format T "~@T~@T~@T~@T|     PROGRAMA TERMINADO     |~%")
  (format T "~@T~@T~@T~@T+============================+~%"))


