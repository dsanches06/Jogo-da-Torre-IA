(defun tab0 ()
  '((T NIL NIL 55 NIL NIL 77)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (66 NIL NIL NIL NIL NIL 22)))

(defun tab1 ()
  '((55 NIL NIL T NIL NIL 77)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (66 NIL NIL NIL NIL NIL 22)))

(defun tab2 ()
  '((55 NIL NIL T NIL NIL 77)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL 74 75 76 57 47 NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (66 NIL NIL NIL NIL NIL 22)))

(defun tab3 ()
  '((NIL NIL NIL T NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL 27 64 46 NIL NIL)
    (NIL NIL 75 76 57 NIL NIL)
    (NIL NIL 71 67 17 NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)))

(defun tab4 ()
  '((NIL NIL NIL T NIL NIL NIL)
    (NIL NIL 77 NIL 66 NIL NIL)
    (NIL 72 27 64 46 55 NIL)
    (NIL NIL 75 76 57 NIL NIL)
    (NIL 11 71 67 17 44 NIL)
    (NIL NIL 22 NIL 33 NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL)))

(defun tab5 ()
  '((55 NIL NIL T NIL NIL 77)
    (NIL 72 NIL NIL NIL 73 NIL)
    (NIL NIL 65 64 63 NIL NIL)
    (NIL 74 75 76 57 47 NIL)
    (NIL NIL 66 67 69 NIL NIL)
    (NIL 70 NIL NIL NIL 71 NIL)
    (66 NIL NIL NIL NIL NIL 22)))

(defun tab6 ()
  '((55 NIL NIL NIL NIL NIL NIL) 
      (NIL 72 NIL NIL NIL 73 NIL) 
      (NIL NIL 65 64 63 NIL NIL) 
      (NIL 74 75 76 57 47 NIL) 
      (NIL NIL NIL 67 69 NIL NIL) 
      (NIL 70 NIL NIL NIL 71 NIL) 
      (NIL NIL NIL NIL NIL NIL T)))

(defun teste (&optional (tabuleiro (tab5)))
  ;;imprimir tabuleiro
  (format t "~%------- tabuleiro inicial ---")
  (mostrar-tabuleiro tabuleiro)
 
    ;;obter posicao da peça
 (let* ((posicao (obter-posicao tabuleiro)))
      
       (format t "~%posicao da peça: x:~A, y:~A~%" (first posicao) (second posicao))
       ;;obter jogadas
       (format t "~%Jogadas 1 da peça na posicao x:~A, y:~A" (first posicao) (second posicao))
       (mostrar-tabuleiro (jogadas-possiveis (first posicao) (second posicao) tabuleiro))
       
        ;;movimento horizontal 3 casa - mov direita 
       (let* ((novo-tabuleiro (HORIZONTAL (first posicao) (second posicao) 3 'DIREITA tabuleiro))
              (nova-posicao (obter-posicao novo-tabuleiro)))
              (format t "~%------- MOVIMENTO HORIZONTAL => (3 DIREITA)  ---")
              (mostrar-tabuleiro novo-tabuleiro)
          
             ;;obter nova posicao da peça   
             (format t "~%Nova posicao da peça: x:~A, y:~A~%" (first nova-posicao) (second nova-posicao))
            ;;obter novas jogadas
            (format t "~%Jogadas 2 da peça na posicao x:~A, y:~A" (first nova-posicao) (second nova-posicao))
            (mostrar-tabuleiro (jogadas-possiveis (first nova-posicao) (second nova-posicao) novo-tabuleiro))

      ;;movimento vertical 2 casa - mov baixo 
       (let* ((novo-tabuleiro1 (VERTICAL (first nova-posicao) (second nova-posicao) 6 'BAIXO novo-tabuleiro))
              (nova-posicao1 (obter-posicao novo-tabuleiro1)))
              (format t "~%------- MOVIMENTO VERTICAL => (6 BAIXO)  ---")
              (mostrar-tabuleiro novo-tabuleiro1)
          
             ;;obter nova posicao da peça   
             (format t "~%Nova posicao da peça: x:~A, y:~A~%" (first nova-posicao1) (second nova-posicao1))
            ;;obter novas jogadas
            (format t "~%Jogadas 3 da peça na posicao x:~A, y~A" (first nova-posicao1) (second nova-posicao1))
            (mostrar-tabuleiro (jogadas-possiveis (first nova-posicao1) (second nova-posicao1) novo-tabuleiro1))

        )


        )



)





)
