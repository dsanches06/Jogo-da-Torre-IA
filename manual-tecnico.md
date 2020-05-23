# **Manual Técnico**

<br /><br />

<p align="center">
    <img src="./screenshots/ips.png">
</p>

## **Projeto - Epoca Especial (Jogo da Torre)**

![Cover](screenshots/capa.png)

### _Unidade Curricular: Inteligência Artificial_ &nbsp;2019/2020

**Realizado por**
- João dos Santos nº 
- Izilda Kossy nº 

**Docentes**
- Prof. 

<div style="page-break-after: always;"></div>

<h1>1- Introdução</h1>

Jogo da Torre

<h1>2- Estrutura do Projeto</h1>

 De modo a facilitar a compreensão e a manutenção do código desenvolvido para esta aplicação, a mesma está dividida em três ficheiros _**.lisp**_, estes são:

* <b>_puzzle.lisp_</b> - Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.

* <b>_procura.lisp_</b> - Código independente do problema. Deve conter a implementação de **algoritmos de procura em espaço de estados**, **os sucessores**, **calculo da efiência dos algoritmos** e outras funções auxiliares dos algoritmos de procura em espaço de estados. As funções presentes neste ficheiro são independentes do dominio do problema, podendo ser reutilizadas para outros problemas. O ficheiro procura.lisp é o ficheiro que apresenta um maior teor teórico relevante para a unidade curricular de **Inteligência Artificial**.

* <b>_projeto.lisp_</b> - Contém código relacionado com o problema.

* <b>_resultados.dat_</b> - Podem ser consultadas todas as métricas de eficiência (bem como outras informações) provenientes da execução da aplicação para um determinado problema.

* <b>_problemas.dat_</b> - Contém os tabuleiros iniciais possíveis para a execução da aplicação. Consiste numa série de listas que representam um tabuleiro separadas por um separador legal.

```
jogo-torre/
        ├── projeto.lisp ; Ficheiro onde é iniciada a aplicação
        ├── procura.lisp ; Contem a implementação dos algoritmos
        ├── puzzle.lisp	; Contem as funções do domínio do problema
        ├── resultados.dat ; Ficheiro que contem logs dos resultados da estatistica
        └── problemas.dat ; Ficheiro com os tabuleiros iniciais do problema
```

<h1>Detalhes da Implementação</h1>

 Dado que Common Lisp é uma linguagem de programação de natureza funcional o desenvolvimento da aplicação consistiu em desenvolver código num paradigma funcional, deste modo algumas técnicas como sequênciação, ciclos e atribuição não foram utilizadas para poder-se focar na recursividade e desenvolvimento de funções. Embora tenham sido permitidas algumas utilizações especiais de ciclos e atribuições, estes não foram utilizados.

<h1>Estudo dos Algoritmos</h1>

<h1>Limitações</h1>

