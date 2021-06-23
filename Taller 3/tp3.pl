%% LENGUAJE S

% Variables (representadas con su correspondiente codificación): Y (1), X1 (2), Z1 (3), X2 (4), Z2 (5), ...

% Etiquetas (representadas con su correspondiente codificación): A(1), B(2), C(3), ...

% Instrucciones:
%  nada(L,V)   [L] V <- V
%  suma(L,V)   [L] V <- V+1
%  resta(L,V)  [L] V <- V-1
%  goto(L,V,E) [L] if V /= 0 goto E

% Estado (lista de pares variable-valor):
%  [(var,val)]

% evaluar(+Estado, +Var, -Val)
% Dado un estado y una variable, instancia en el tercer argumento el valor
%   de la variable en el estado. Las variables que no aparecen en el estado tienen valor 0.
% COMPLETAR
evaluar([], _, 0).
evaluar([(X,V)|_],X,V).
evaluar([(Y,_)|LS],X,V) :- X\=Y, evaluar(LS,X,V).
% Por si las dudas
% evaluar([(Y,_)|LS],X,V) :- X\=Y, evaluar(LS,X,V), !.

%% CODIFICACIÓN

%% Codificación de listas

% codificacionLista(?L, ?Z)
codificacionLista(L, Z) :- codificacionListaDesde(L, Z, 1).

% codificacionLista(?L, ?Z, +I)
codificacionListaDesde([], 1, _).
codificacionListaDesde([X|Xs], Z, I) :- ground([X|Xs]), Im1 is I+1, iesimoPrimo(I, P), codificacionListaDesde(Xs, Rec, Im1), Z is Rec*P**X.
codificacionListaDesde([X|Xs], Z, I) :- var(X), var(Xs), Im1 is I+1, iesimoPrimo(I, P), maximoExponenteQueDivideA(X,P,Z), Rec is Z/(P**X), codificacionListaDesde(Xs, Rec, Im1).

% divide(?A, +B)
divide(A, B) :- between(1, B, A), between(1, B, X), B is A*X.

% esPrimo(+P)
esPrimo(P) :- P \= 1, Pm1 is P-1, not((between(2, Pm1, X), divide(X, P))).

desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

% iesimoPrimo(+I, -P)
% COMPLETAR
iesimoPrimo(1,2).
iesimoPrimo(I,P) :- I>1, I2 is I-1, iesimoPrimo(I2,P2), P3 is P2+1, desde(P3,P), esPrimo(P), !.

% COMPLETAR
%maximoExponenteQueDivideA(0,P,Z) :- not(divide(P,Z)).
%maximoExponenteQueDivideA(1,1,_).
%maximoExponenteQueDivideA(X,P,Z) :- P > 1, divide(P,Z), Q is div(Z,P), maximoExponenteQueDivideA(X2,P,Q), X is X2+1.

% maximoExponenteQueDivideA(-X, +P, +Z)
maximoExponenteQueDivideA(X,P,Z) :- desde(0,Y), exp(P,PE,Y), not(divide(PE,Z)), X is Y-1, !.

% cambiar orden
%exp(+P,-R,+E)
exp(_,1,0).
exp(P,P,1).
exp(P,R,E) :- E>1, E2 is E-1, exp(P,R2,E2), R is P*R2.

%% OBSERVADORES

% pi1(+(X,Y), -X) % instancia en el segundo argumento la primer componente de la tupla (X, Y).
pi1((X, _), X).

% pi2(+(X,Y), -Y) % instancia en el segundo argumento la primer componente de la tupla (X, Y).
pi2((_, Y), Y).

% iesimo(+L, +I, -X) % indexar en lista: instancia en el tercer argumento el elemento en la posición I de la lista L.
iesimo([X|_], 1, X).
iesimo([_|Xs], I, E) :- I > 1, Im1 is I-1, iesimo(Xs, Im1, E).

% etiquetaInstruccion(+Instruccion, -Etiqueta)
etiquetaInstruccion(nada(L,_), L).
etiquetaInstruccion(suma(L,_), L).
etiquetaInstruccion(resta(L,_), L).
etiquetaInstruccion(goto(L,_,_), L).

% codigoInstruccion(+Instruccion, -Codigo)
codigoInstruccion(nada(_,_), 0).
codigoInstruccion(suma(_,_), 1).
codigoInstruccion(resta(_,_), 2).
codigoInstruccion(goto(_,_,E), C) :- C is E+2.

% variableInstruccion(+Instruccion, -Variable)
variableInstruccion(nada(_,V), V).
variableInstruccion(suma(_,V), V).
variableInstruccion(resta(_,V), V).
variableInstruccion(goto(_,V,_), V).

% longitud(+P, -L) % Instancia en el segundo argumento la longitud del programa P
longitud([], 0).
longitud([_|Xs], L) :- longitud(Xs, Lm1), L is Lm1 + 1.

% iEsimaInstruccion(+P, +Indice, -Instruccion) % Instancia en el tercer argumento la Indice-ésima instrucción del programa P.
iEsimaInstruccion(E, Indice, Instruccion) :- is_list(E), iesimo(E, Indice, Instruccion).

%% Simulación del lenguaje S

% Descripción instantánea (par índice-estado, donde el índice (empezando en 1)
% indica el número de la próxima instrucción a ejecutar):
%  (i,s)

% snap(+Xs, +P, +T, -Di)
% Instancia en el cuarto argumento la descripción instantánea resultante de
% ejecutar el programa P con entradas Xs tras T pasos.
% COMPLETAR

%Las descripciones instantáneas se representan con
%tuplas cuyas primeras componentes corresponden al ı́ndice de la próxima instrucción a ejecutar y
%cuyas segundas componentes corresponden al estado del programa.

%maximo(+N,+M,-Max)
maximo(N,M,N) :- N > M.
maximo(N,M,M) :- N =< M.

%avanzarIndice(+P, +S, +Ins, +I0, -I)
avanzarIndice([],_,_,_,0).
% Si la instruccion es distinta de goto
%avanzarIndice(P,_,Ins,I0,I) :- codigoInstruccion(Ins,C), C<3, length(P,N), I2 is I0+1, maximo(I2,N,I).
%avanzarIndice(P,S,)

%avanzarEstado(+Ins, +S0, -S)
avanzarEstado(nada(_,_),S,S).
avanzarEstado(goto(_,_,_),S,S).
%avanzarEstado(Ins,S0,S) :- codigoInstruccion(Ins,C), variableInstruccion(Ins,V), evaluar(S0,V,AcVal), , muchosAppends((V,AcVal),NT,S0,S), nuevaTupla(Val,C,R), .
avanzarEstado(suma(_,V),[(V,Va)|LS],[(V,NVa)|LS]) :- NVa is Va+1.
avanzarEstado(suma(_,V),[(X,ValX)|LS],S) :- V \= X, avanzarEstado(suma(_,V),LS,S2), append([(X,ValX)],S2,S).
avanzarEstado(resta(_,V),[(V,Va)|LS],[(V,NVa)|LS]) :- restaMinCero(Va,NVa).
avanzarEstado(resta(_,V),[(X,ValX)|LS],S) :- V \= X, avanzarEstado(resta(_,V),LS,S2), append([(X,ValX)],S2,S).

%append(L1,[T],L2), append(L2,L3,S0), append(L1,[NuevaTupla],L4), append(L4,L3,S).

%restaMinCero(+X,-Y).
restaMinCero(0,0).
restaMinCero(X,Y) :- X>0, Y is X-1.

% buscarValor(+S0,+V,-Val)
buscarValor(S0,V,Val) :- length(S0,N), between(0,N,I), iesimo(S0,I,T), pi1(T,V2), V=V2, pi2(T,Val).

nuevoValor(Val,C,R) :- R is V + 3 + (-2*C).
% V + 3 + (-2*C)

% stp(+Xs, +P, +T)
% Indica si el programa P con entradas Xs termina tras T pasos.
% Se dice que un programa terminó cuando la próxima instrucción a ejecutar es
% 1 más que la longitud del programa.
% COMPLETAR

%% Pseudo-Halt

% pseudoHalt(+X, +Y)
% COMPLETAR

% Buscar entradas para las cuales el programa Y termina
% pseudoHalt2(-X, +Y)
% COMPLETAR

% Buscar pares programa-entrada que terminen
% pseudoHalt3(-X, -Y)
% COMPLETAR

% programa(-P, +N)
programa([], 0).
programa([I|Is], N) :- N > 0, between(1,N,C), instruccion(I,C), N2 is N-C, programa(Is,N2).

% instruccion(-I, +N)
instruccion(nada(0,1),1).
instruccion(nada(L,V),N) :- N > 1, between(1,N,V), L is N-V.
instruccion(suma(0,1),1).
instruccion(suma(L,V),N) :- N > 1, between(1,N,V), L is N-V.
instruccion(resta(0,1),1).
instruccion(resta(L,V),N) :- N > 1, between(1,N,V), L is N-V.
instruccion(goto(0,1,1),2).
instruccion(goto(L,V,E),N) :- N > 2, N2 is N-1, between(1,N2,V), N3 is N-V, between(1,N3,E), L is N3-E.

%% TESTS

cantidadTestsEvaluar(2). % Actualizar con la cantidad de tests que entreguen
testEvaluar(1) :- evaluar([],1,0).
testEvaluar(2) :- evaluar([(4,0),(2,3)],2,3).
% Agregar más tests

cantidadTestsCodificacion(2). % Actualizar con la cantidad de tests que entreguen
testCodificacion(1) :- codificacionLista([],1).
testCodificacion(2) :- codificacionLista([1],2).
% Agregar más tests

cantidadTestsSnapYstp(2). % Actualizar con la cantidad de tests que entreguen
testSnapYstp(1) :- stp([],[],1).
testSnapYstp(2) :- snap([10],[suma(0,1)],0,(1,[(2,10)])).
% Agregar más tests

cantidadTestsHalt(1). % Actualizar con la cantidad de tests que entreguen
testHalt(1) :- pseudoHalt([1],[suma(0,1)]).
% Agregar más tests

tests(evaluar) :- cantidadTestsEvaluar(M), forall(between(1,M,N), testEvaluar(N)).
tests(codificacion) :- cantidadTestsCodificacion(M), forall(between(1,M,N), testCodificacion(N)).
tests(snapYstp) :- cantidadTestsSnapYstp(M), forall(between(1,M,N), testSnapYstp(N)).
tests(halt) :- cantidadTestsHalt(M), forall(between(1,M,N), testHalt(N)).

tests(todos) :-
  tests(evaluar),
  tests(codificacion),
  tests(snapYstp),
  tests(halt).

tests :- tests(todos).
