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

% CODIFICACIÓN

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

% desde(+X,-Y)
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

% OBSERVADORES

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
snap(Xs,_,0,(1,S0)) :- entradaAEstado(Xs,S0), !.
snap(Xs,P,T,(Ind2,Est2)) :- T>0, T2 is T-1, snap(Xs,P,T2,(Ind2,Est2)), termino(P,Ind2), !.
snap(Xs,P,T,(Ind,Est)) :- T>0, T2 is T-1, snap(Xs,P,T2,(Ind2,Est2)), not(termino(P,Ind2)), iEsimaInstruccion(P,Ind2,Inst), avanzarIndice(P,Est2,Inst,Ind2,Ind), avanzarEstado(Inst,Est2,Est), !.

%termino(+P,+Ind).
termino(P,Ind) :- longitud(P,Long), Ind > Long.

%entradaAEstado(+Xs,-S0).
entradaAEstado(Xs,S0) :- entradaAEstadoAux(Xs,0,S0).

%entradaAEstadoAux(+Xs,+I,-S0)
entradaAEstadoAux([],_,[]).
entradaAEstadoAux([X|LS],I,[(I2,X)|S0]) :- I2 is I+2, entradaAEstadoAux(LS,I2,S0).

% % Todo esto lo habia hecho porque pense que la codificacion de las variables de entrada dependian de la cantidad de variables temporales
% % Es decir, si tengo un programa sin variables temporales, X1 se codifica con 2, X2 con 3, X3 con 4,...
% % En cambio si tengo variables temporales Z1 y Z2, X1 se codifica con 2, X2 con 4, X3 con 6, ..
% %entradaAEstado(+P,+Xs,-S0).
% entradaAEstado(P,Xs,S0) :- entradaAEstadoAux(Xs,0,Est), incrementarCodigoVarsEntrada(P,Xs,Est,S0).

% %entradaAEstadoAux(+Xs,+I,-S0)
% entradaAEstadoAux([],_,[]).
% entradaAEstadoAux([X|LS],I,[(I2,X)|S0]) :- I2 is I+2, NextI is I+1, entradaAEstadoAux(LS,NextI,S0).

% %incrementarCodigoVarsEntrada(+P,+Xs,+Est,-S0)
% incrementarCodigoVarsEntrada(P,Xs,Est,S0) :- cantDeVariablesTemp(P,Xs,CantDeVariablesTemp), incrementarCodigoVarsEntradaAux(Est,CantDeVariablesTemp,S0).

% %incrementarCodigoVarsEntradaAux(+Est,+CantTemp,-S0)
% incrementarCodigoVarsEntradaAux([],_,[]).
% incrementarCodigoVarsEntradaAux(Est,0,Est).
% incrementarCodigoVarsEntradaAux([T|LS],CantTemp,[T|LSInc]) :- CantTemp>0, incrementarTodosLosCodigos(LS,LS2), CantTemp2 is CantTemp-1, incrementarCodigoVarsEntradaAux(LS2,CantTemp2,LSInc).

% %incrementarTodosLosCodigos(+Est,-EstIncrementado).
% incrementarTodosLosCodigos([],[]).
% incrementarTodosLosCodigos([(Cod,V)|LS],[(Cod2,V)|LSInc]) :- Cod2 is Cod+1, incrementarTodosLosCodigos(LS,LSInc).

% % Este predicado nos sirve para saber cuantas variables temporales hay en el programa
% %cantDeVariablesTemp(+P,+Xs,-CantTemp)
% %cantDeVariablesTemp(P,Xs,CantTemp) :- cantDeVariables(P,0,CantTotal), length(Xs,CantEntrada), CantTemp is CantTotal-CantEntrada-1.
% cantDeVariablesTemp(P,Xs,CantTemp) :- cantDeVariables(P,0,CantTotal), length(Xs,CantEntrada), CantEnPrograma is CantTotal-CantEntrada-1,maximo(CantEnPrograma,0,CantTemp).
% % El 1 que restamos es por Y

% %cantDeVariables(+P,+CantActual,-Cant)
% cantDeVariables([],_,0).
% cantDeVariables([Ins|LS],CantActual,Cant) :- variableInstruccion(Ins,NumVar), cantDeVariables(LS,CantActual,Cant2), maximo(NumVar,Cant2,Cant).

%maximo(+N,+M,-Max)
maximo(N,M,N) :- N > M.
maximo(N,M,M) :- N =< M.

%avanzarIndice(+P, +S, +Ins, +I0, -I)
avanzarIndice([],_,_,_,0).
% Si la instruccion es distinta de goto
avanzarIndice(P,_,Ins,I0,I) :- codigoInstruccion(Ins,C), C<3, longitud(P,N), I2 is I0+1, maximo(I2,N,I).
avanzarIndice(_,[],goto(_,_,_),I0,I) :- I is I0+1.
avanzarIndice(_,[(V,0)|_],goto(_,V,_),I0,I) :- I is I0+1.
avanzarIndice(P,[(V,M)|_],goto(_,V,E),_,I) :- M\=0, primeraAparicionEtiqueta(P,E,I).
avanzarIndice(P,[(X,_)|LS],goto(_,V,E),I0,I) :- X\=V, avanzarIndice(P,LS,goto(_,V,E),I0,I).

%primeraAparicionEtiqueta(+P,+E,-I)
primeraAparicionEtiqueta(P,E,I) :- longitud(P,N), between(1,N,I), iEsimaInstruccion(P,I,Ins), etiquetaInstruccion(Ins,E2), E2=E, !.
primeraAparicionEtiqueta(P,_,I) :- longitud(P,N), I is N+1.

%avanzarEstado(+Ins, +S0, -S)
% Estas dos primeras instrucciones no modifican el estado
avanzarEstado(nada(_,_),S,S).
avanzarEstado(goto(_,_,_),S,S).
avanzarEstado(suma(E,V),S1,S2) :- avanzarEstadoSuma(suma(E,V),S1,S2).
avanzarEstado(resta(E,V),S1,S2) :- avanzarEstadoResta(resta(E,V),S1,S2).

%avanzarEstadoSuma(suma(+E,+V),+S1,-S2)
avanzarEstadoSuma(suma(_,V),[(V,Va)|LS],[(V,NVa)|LS]) :- NVa is Va+1.
avanzarEstadoSuma(suma(_,V),[(X,ValX)|LS],S) :- V \= X, avanzarEstado(suma(_,V),LS,S2), append([(X,ValX)],S2,S).
% Si es la primera vez que aparece la variable
avanzarEstadoSuma(suma(_,V),[],[(V,1)]).

%avanzarEstadoResta(resta(+E,+V),+S1,-S2).
avanzarEstadoResta(resta(_,V),[(V,Va)|LS],[(V,NVa)|LS]) :- restaMinCero(Va,NVa).
avanzarEstadoResta(resta(_,V),[(X,ValX)|LS],S) :- V \= X, avanzarEstado(resta(_,V),LS,S2), append([(X,ValX)],S2,S).
% Si es la primera vez que aparece la variable
avanzarEstadoResta(resta(_,V),[],[(V,0)]).

%restaMinCero(+X,-Y).
restaMinCero(0,0).
restaMinCero(X,Y) :- X>0, Y is X-1.

% stp(+Xs, +P, +T)
% Indica si el programa P con entradas Xs termina tras T pasos.
% Se dice que un programa terminó cuando la próxima instrucción a ejecutar es
% 1 más que la longitud del programa.
% COMPLETAR
stp(Xs,P,T) :- longitud(P,Long), snap(Xs,P,T,Di), pi1(Di,Ind), Ind is Long+1.

% desde2(+X,?Y)
% Si Y no esta instanciada
desde2(X,Y) :- var(Y), desde(X,Y).
% Si Y esta instanciada
desde2(X,Y) :- nonvar(Y), X =< Y.

%todosLosPares(-X,-Y).
%todosLosPares(X,Y) :- desde(1,Y), between(0,Y,X).
todosLosPares(X,Y) :- desde(0,Z), between(0,Z,X), Y is Z-X.

%% Pseudo-Halt
% pseudoHalt(+X, +Y)
% COMPLETAR
pseudoHalt(X,P) :- desde(1,Step), stp([X],P,Step), !.

% Buscar entradas para las cuales el programa Y termina
% pseudoHalt2(-X, +Y)
% COMPLETAR
%pseudoHalt2(X,Y) :- desde2(0,X), pseudoHalt(X,Y).
%pseudoHalt2(X,Y) :- todosLosPares(Step,X), not(terminaParaUnStepAnterior(X,Y,Step)), stp([X],Y,Step).
pseudoHalt2(X,Y) :- todosLosPares(X,Step), not(terminaParaUnStepAnterior(X,Y,Step)), stp([X],Y,Step).

%terminaParaUnStepAnterior(+X,+Y,+Step)
terminaParaUnStepAnterior(X,Y,Step) :- Step2 is Step-1, maximo(Step2,0,Step3), stp([X],Y,Step3).
%terminaParaUnStepAnterior(X,Y,Step) :- Step2 is Step-1, between(0,Step2,Step3), stp([X],Y,Step3), !.

% Buscar pares programa-entrada que terminen
% pseudoHalt3(-X, -Y)
% COMPLETAR
%pseudoHalt3(X,Y) :- desde(1,Y), desde2(Y,X), programa(P,Y), between(1,Y,X), pseudoHalt2(X,P).
%pseudoHalt3(X,Y) :- desde(1,Y), programa(P,Y), pseudoHalt2(X,P).
%pseudoHalt3(X,Y) :- todosLosPares(X,Y), programa(P,Y), pseudoHalt2(X,P).
%pseudoHalt3(X,Y) :- todasLasTriplas(X,Y,Step), programa(P,Y), not(terminaParaUnStepAnterior(X,P,Step)), stp([X],P,Step).
pseudoHalt3(X,Y) :- todasLasTriplas(Step,X,Y), programa(P,Y), not(terminaParaUnStepAnterior(X,P,Step)), stp([X],P,Step).

%todasLasTriplas(-X,-Y,-Z)
%todasLasTriplas(X,Y,Z) :- desde(0,Z), between(0,Z,X), Y is Z-X.
%todasLasTriplas(X,Y,Z) :- desde(0,W), between(0,W,Z), Y is W-Z, between(0,W,V), X is W-V.
%todasLasTriplas(X,Y,Z) :- desde(0,W), between(0,W,Z), between(0,W,Y), X2 is W-Y-Z, maximo(X2,0,X).
todasLasTriplas(X,Y,Z) :- desde(0,W), between(0,W,Z), between(0,W,Y), between(0,W,X), W is X+Y+Z.
%todasLasTriplas(X,Y,Z) :- desde(0,W), between(0,W,Z), Y2 is W-Z, between(0,Y2,Y), X2 is Y2-Y, between(0,X2,X).

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

% TESTS

cantidadTestsEvaluar(4). % Actualizar con la cantidad de tests que entreguen
testEvaluar(1) :- evaluar([],1,0).
testEvaluar(2) :- evaluar([(4,0),(2,3)],2,3).
% Agregar más tests
testEvaluar(3) :- evaluar([(1,2),(2,2)],1,2).
testEvaluar(4) :- evaluar([(1,0),(2,5),(3,3)],4,0).

cantidadTestsCodificacion(2). % Actualizar con la cantidad de tests que entreguen
testCodificacion(1) :- codificacionLista([],1).
testCodificacion(2) :- codificacionLista([1],2).
% Agregar más tests

cantidadTestsSnapYstp(8). % Actualizar con la cantidad de tests que entreguen
testSnapYstp(1) :- stp([],[],1).
testSnapYstp(2) :- snap([10],[suma(0,1)],0,(1,[(2,10)])).
% Agregar más tests
testSnapYstp(3) :- snap([10],[suma(0,1)],1,(2,[(2,10),(1,1)])).
testSnapYstp(4) :- snap([],[suma(1,1),goto(0,1,1)],2,(1,[(1,1)])).
testSnapYstp(5) :- snap([],[suma(1,1),goto(0,1,1)],3,(2,[(1,2)])).

testSnapYstp(6) :- not(stp([10],[suma(0,1)],0)).
testSnapYstp(7) :- stp([10],[suma(0,1)],1).
testSnapYstp(8) :- between(0,100,X), not(stp([],[suma(1,1),goto(0,1,1)],X)).

cantidadTestsHalt(1). % Actualizar con la cantidad de tests que entreguen
testHalt(1) :- pseudoHalt(1,[suma(0,1)]).
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
armarPares(X,Y) :- desde(1,S), between(0,S,X), Y is S-X.
coprimos(X,Y) :- armarPares(X,Y), gcd(X,Y) =:= 1.
