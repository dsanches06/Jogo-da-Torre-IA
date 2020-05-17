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

(defun move-piece (x y dx dy board)
  (let ((new-board (copy-tree board))
        (new-x (+ x dx))
        (new-y (+ y dy))
        (piece (nth x (nth y board))) )
       ;; (if (equal (casa-disponivel new-x new-y board)  't)
    (when (posicao-validap new-x new-y)
      (setf (nth x (nth y new-board)) nil
          (nth new-x (nth new-y new-board)) piece ))
    new-board));)

(defun MOVE (x y n mov board) ;; n is number of squares to move
  (case mov
    (UP (move-piece x y 0 (- n) board))
    (DOWN (move-piece x y 0 n board))
    (LEFT (move-piece x y (- n) 0 board))
    (RIGHT (move-piece x y n 0 board))
    (otherwise NIL) ))

(defun valid-moves (x y board)
  (let (result)
    (dolist (mov '(up down left right) result)
      (dotimes (n 6)
        (when (move x y n mov board)
          (push (list n mov) result) )))))



(defun teste (&optional (tabuleiro (board)))
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
       (let* ((novo-tabuleiro1 (VERTICAL (first nova-posicao) (second nova-posicao) 2 'BAIXO novo-tabuleiro))
              (nova-posicao1 (obter-posicao novo-tabuleiro1)))
              (format t "~%------- MOVIMENTO VERTICAL => (2 CIMA)  ---")
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