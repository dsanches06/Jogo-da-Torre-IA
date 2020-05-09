

(defun teste (&optional (tabuleiro (board)))
  ;;imprimir tabuleiro
  (format t "~%------- tabuleiro inicial ---")
  (mostrar-tabuleiro tabuleiro)
 
    ;;obter posicao da peça
 (let* ((posicao (obter-posicao tabuleiro)))
      
       (format t "~%posicao da peça: x:~A, y:~A~%" (first posicao) (second posicao))
       ;;obter jogadas
       (format t "~%Jogadas 1 da peça na posicao x:~A, y~A" (first posicao) (second posicao))
       (mostrar-tabuleiro (jogadas-possiveis (first posicao) (second posicao) tabuleiro))
       
        ;;movimento horizontal 3 casa - mov direita 
       (let* ((novo-tabuleiro (VERTICAL (first posicao) (second posicao) 3 'cima tabuleiro))
              (nova-posicao (obter-posicao novo-tabuleiro)))
              (format t "~%------- MOVIMENTO VERTICAL => (3 CIMA)  ---")
              (mostrar-tabuleiro novo-tabuleiro)
          
             ;;obter nova posicao da peça   
             (format t "~%Nova posicao da peça: x:~A, y:~A~%" (first nova-posicao) (second nova-posicao))
            ;;obter novas jogadas
            (format t "~%Jogadas 2 da peça na posicao x:~A, y~A" (first nova-posicao) (second nova-posicao))
            (mostrar-tabuleiro (jogadas-possiveis (first nova-posicao) (second nova-posicao) novo-tabuleiro))

      ;;movimento vertical 2 casa - mov baixo 
       (let* ((novo-tabuleiro1 (VERTICAL (first nova-posicao) (second nova-posicao) 2 'BAIXO novo-tabuleiro))
              (nova-posicao1 (obter-posicao novo-tabuleiro1)))
              (format t "~%------- MOVIMENTO HORIZONTAL => (1 DIREITA)  ---")
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