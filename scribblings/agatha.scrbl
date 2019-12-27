#lang scribble/manual

@title{agatha: Para Logica}
@author{Joseildo M S Filho, Andrei Formiga}

@defmodulelang{agatha}

@section{Introdução}
Está é um linguagem de dominio especifico, o objetivo é produzir tabelas-verdade
para formular de Lógica de primeira ordem.

@verbatim{
#lang agatha
f = A ^ B;
#f;
}

Assim o operador #(avalia expressão) cria a tabela e exibe o resultado.
@section{Operadores}
Temos os Operadores basicos de Lógica.
@verbatim{
#lang agatha
f = A ^ B; // Operador E
f = A + B; // Operador Ou
f = A -> B; // Operador Implica
f = ¬A; // Operador Não
}

@section{Da Linguagem}
Apesar de muito simples, devemos atentar a alguns detalhes.
1º as variaveis devem ser minusculas, ou seja, o lado esquerdo do = é minusculo.
2º as variaveis booleanas são maiusculas.
3º as variaveis naõ booleanas podem ser sobrescritas
4º toda linha de commando ou expressão deve terminar com ;
