
(defun tab-array ()
 (make-array 49 :displaced-to (lista-sem-numero-089))
 )

(defun tabuleiro-vazio (n m)
	"Gera um tabuleiro vazio com n linhas e m colunas"
		(make-list (1+ n) :initial-element (make-list (1+ m) :initial-element (lista-sem-numero-089))) 
)