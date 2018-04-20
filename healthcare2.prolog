:-include('factos.prolog').
% utente : #IdUt, Nome, Idade, Morada --> {V,F}
% prestador : #IdPrest, Nome, Especialidade, Instituição --> {V,F}
% cuidado : #IdCuidado, Data, idUt, IdPrest, Descrição, Custo, Rating --> {V,F}
% -------------------------------------------------------
% Definições iniciais

:- op(900, xfy, '::').
:- op(996, xfy, '&&' ).  % operador de conjuncao
:- op(997, xfy, '$$' ).  % operador de disjuncao
:- op(998, xfx, '=>' ).  % operador de implicacao
:- op(999, xfx, '<=>' ). % operador de equivalencia


:- dynamic int/1.  % operador de invariante interdito
:- dynamic inc/1.  % operador de invariante incerto
:- dynamic imp/1.  % operador de impreciso
:- dynamic '-'/1.


% -------------------------------------------------------
% --------------- Predicados auxiliares -----------------
% -------------------------------------------------------

equivalencia( X, X, verdadeiro ) :- X \= desconhecido.
equivalencia( desconhecido, _, desconhecido ).
equivalencia( _, desconhecido, desconhecido ).
equivalencia( verdadeiro, falso, falso ).
equivalencia( falso, verdadeiro, falso ).

implicacao( falso, _, verdadeiro ).
implicacao( _, verdadeiro, verdadeiro ).
implicacao( verdadeiro, desconhecido, desconhecido ).
implicacao( desconhecido, X, desconhecido ) :- X \= verdadeiro.
implicacao( verdadeiro, falso, falso ).

disjuncao( verdadeiro, _, verdadeiro ).
disjuncao( _, verdadeiro, verdadeiro ).
disjuncao( desconhecido, Y, desconhecido ) :- Y \= verdadeiro.
disjuncao( Y, desconhecido, desconhecido ) :- Y \= verdadeiro.
disjuncao( falso, falso, falso ).

conjuncao( verdadeiro, verdadeiro, verdadeiro ).
conjuncao( falso, _, falso ).
conjuncao( _, falso, falso ).
conjuncao( desconhecido, verdadeiro, desconhecido ).
conjuncao( verdadeiro, desconhecido, desconhecido ).

%TODO: Pressuposto do mundo fechado de TUDO
%       -utente(Id,N,I,S,C) :-
%          nao(utente(Id,N,I,S,C)),
%          nao(excecao(utente(Id,N,I,S,C))).

% Predicado demo:
% Questao, Resposta -> {V,F}
demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

% Extensao do meta-predicado demo :: Expressão, Resposta -> {V,F}
demo( P <=> X, V ) :- demo( P, V1 ), demo( X, V2 ), equivalencia( V1, V2, V ), !.
demo( P => X, V ) :- demo( P, V1 ), demo( X, V2 ), implicacao( V1, V2, V ), !.
demo( P $$ X, V ) :- demo( P, V1 ), demo( X, V2 ), disjuncao( V1, V2, V ), !.
demo( P && X, V ) :- demo( P, V1 ), demo( X, V2 ), conjuncao( V1, V2, V ), !.

% Predicado pertence:
% Elemento, Lista -> {V,F}
pertence(H,[H|_]).
pertence(X,[H|T]) :-
	X \= H,
        pertence(X,T).

% Predicado de negação nao:
% Predicado -> {V,F}
nao(Q) :- Q,!,false.
nao(_).

% Predicado unico:
% Lista1, Lista2 -> {V,F}
unicos([],[]).
unicos([H|T], R) :-
	pertence(H,T),
	unicos(T,R).
unicos([H|T], [H|R]) :-
	nao(pertence(H,T)),
	unicos(T,R).

% Predicado comprimento:
% Lista, Comprimento -> {V,F}
comprimento([],0).
comprimento([_H],1).
comprimento([_H|T],N) :- comprimento(T,M), N is M+1.

% Predicado solucoes:
% Termo, Predicado, Lista -> {V,F}
solucoes(T,Q,S) :- findall(T,Q,S).

% Predicado soma:
% Lista, Valor -> {V,F}
soma([],0).
soma([N|Ns], T) :- soma(Ns,X), T is X+N.

% Predicado media:
% Lista, Valor -> {V,F}
media([],0).
media(List,Med) :-
    soma(List,X),
    comprimento(List,L),
    Med is (div(X,L)).

% Predicado isEqual
% Vê se dois elementos sao iguais
isEqual(A,A).

% Predicado isNotEqual
% Vê se dois elementos nao sao iguais
isNotEqual(A,B):- A\=B.

% Predicado subsImpreciso
% Vê se existem predicados imprecisos com aquele ID e se existirem apaga-os
subsImpreciso(Id,p):-
        impreciso(Id,p),
        solucoes(excecao(prestador(Id,N,E,L)),excecao(prestador(Id,N,E,L)),N),
        apagarTudo(N),
        apagar(impreciso(Id,p)).
subsImpreciso(Id,p).

% Predicado apagarTudo
% Apaga todos os predicados da lista
apagarTudo([H|T]):-
        apagar(H),
        apagarTudo(T).
apagarTudo([]).

% Predicado apagarIncerto
% Vê se existe um predicado incerto com aquele ID e se existir apaga-o
apagarIncerto(Id,p) :-
        incerto(Id,p),
        apagar(incerto(Id,p)).
apagarIncerto(Id,p).

% Predicado apagarPrestador
% Vê se existe um predicado incerto com aquele ID e se existir apaga o prestador correspondente
apagarPrestador(Id) :-
        incerto(Id,p),
        apagar(prestador(Id,_,_,_)).

% -------------------------------------------------------
%  Evoluções prestador:
% -------------------------------------------------------
% evolucao: F, Type -> {V,F}

evolucao( prestador(Id, Nome, E,Ins), positivo) :-
    solucoes(I, +prestador(Id,Nome,E,Ins)::I, Li),
    insercao(prestador(Id, Nome, E,Ins)),
    insercao(positivo(Id,p)),
    testa(Li),
    apagarPrestador(Id),
    apagarIncerto(Id,p),
    subsImpreciso(Id,p).

evolucao( prestador(Id,Nome,Especialidade,Local) , negativo ) :-
    solucoes( I, +(-prestador(Id,Nome,Especialidade,Local) )::I, Li ),
    insercao(-prestador(Id,Nome,Especialidade,Local) ),
    testa(Li),
    insercao(negativo(Id,p)).

evolucao( [prestador(Id,Nome,Especialidade,Local) | R], impreciso ) :-
    solucoes( Inv, imp(prestador(Id,Nome,Especialidade,Local))::Inv, L ),
    insercao(excecao(prestador(Id,Nome,Especialidade,Local) )),
    testa(L),
    insercao(impreciso(Id,p)),
    apagarPrestador(Id),
    apagarIncerto(Id,p),
    evolucao( R,impreciso ).
evolucao( [], impreciso ).

evolucao( prestador( Id,Nome,Especialidade,Local ), incerto, local) :-
        solucoes(I, inc(prestador(Id,Nome,Especialidade,Local))::I, Li),
        insercao(prestador( Id,Nome,Especialidade,yyyy)),
        testa(Li),
        insercao(incerto(Id,p)).

evolucao( prestador( Id,Nome,Especialidade,Local ), incerto, nome) :-
        solucoes(I, inc(prestador(Id,Nome,Especialidade,Local))::I, Li),
        insercao(prestador( Id,yyyy,Especialidade,Local )),
        testa(Li),
        insercao(incerto(Id,p)).

evolucao( prestador( Id,Nome,Especialidade,Local ), incerto, especialidade) :-
        solucoes(I, inc(prestador(Id,Nome,Especialidade,Local))::I, Li),
        insercao(prestador( Id,Nome,yyyy,Local )),
        testa(Li),
        insercao(incerto(Id,p)).

evolucao( prestador( Id,Nome,Especialidade,Local ), interdito, local) :-
        solucoes(I, int(prestador(Id,Nome,Especialidade,Local))::I, Li),
        insercao(prestador( Id,Nome,Especialidade,noLocal )),
        insercao(interdito(Id,p)),
        testa(Li).

evolucao( prestador( Id,Nome,Especialidade,Local ), interdito, nome) :-
        solucoes(I, int(prestador(Id,Nome,Especialidade,Local))::I, Li),
        insercao(prestador( Id,noName,Especialidade,Local)),
        insercao(interdito(Id,p)),
        testa(Li).

evolucao( prestador( Id,Nome,Especialidade,Local ), interdito, especialidade) :-
        solucoes(I, int(prestador(Id,Nome,Especialidade,Local))::I, Li),
        insercao(prestador( Id,Nome,noEspecialidade,Local )),
        insercao(interdito(Id,p)),
        testa(Li).

% -------------------------------------------------------
%  Evoluções utente:
% -------------------------------------------------------
evolucao( utente(Id, Nome, Idade ,Local), positivo) :-
    solucoes(I, +utente(Id, Nome, Idade ,Local)::I, Li),
    insercao(utente(Id, Nome, Idade ,Local)),
    insercao(positivo(Id,u)),
    testa(Li),
    apagar((incerto(Id, u) :- (apagar((excecao(utente(Id, Nome, Idade ,Local))  :- incerto(Id,u)))))),
    subsImpreciso(Id,u).

evolucao( utente(Id, Nome, Idade ,Local) , negativo ) :-
    solucoes( I, +(-utente(Id, Nome, Idade ,Local) )::I, Li ),
    insercao(-utente(Id, Nome, Idade ,Local) ),
    testa(Li),
    insercao(negativo(Id,u)).

evolucao( [utente(Id, Nome, Idade ,Local) | R], impreciso ) :-
    solucoes( Inv, imp(utente(Id, Nome, Idade ,Local))::Inv, L ),
    insercao(excecao(utente(Id, Nome, Idade ,Local) )),
    testa(L),
    insercao(impreciso(Id,u)),
    apagarUtente(Id),
    apagarIncerto(Id,u),
    evolucao( R,impreciso ).

evolucao( [], impreciso ).

evolucao( utente(Id, Nome, Idade ,Local), incerto, local) :-
        solucoes(I, inc(utente(Id, Nome, Idade ,Local))::I, Li),
        insercao(utente(Id, Nome, Idade ,xxxx)),
        testa(Li),
        insercao(incerto(Id,p)).

evolucao( prestador( Id,Nome,Especialidade,Local ), incerto, nome) :-
        solucoes(I, inc(utente(Id, Nome, Idade ,Local))::I, Li),
        insercao(utente(Id, xxxx, Idade ,Local)),
        testa(Li),
        insercao(incerto(Id,u)).

evolucao( utente(Id, Nome, Idade ,Local), incerto, idade) :-
        solucoes(I, inc(utente(Id, Nome, Idade ,Local))::I, Li),
        insercao(utente(Id, Nome, xxxx ,Local)),
        testa(Li),
        insercao(incerto(Id,u)).

evolucao( utente(Id, Nome, Idade ,Local), interdito, local) :-
        solucoes(I, int(utente(Id, Nome, Idade ,Local))::I, Li),
        insercao(utente(Id, Nome, Idade ,noLocal)),
        insercao(interdito(Id,u)),
        testa(Li).

evolucao( utente(Id, Nome, Idade ,Local), interdito, nome) :-
        solucoes(I, int(utente(Id, Nome, Idade ,Local))::I, Li),
        insercao(utente(Id, noName, Idade ,Local)),
        insercao(interdito(Id,u)),
        testa(Li).

evolucao( utente(Id, Nome, Idade ,Local), interdito, idade) :-
        solucoes(I, int(utente(Id, Nome, Idade ,Local))::I, Li),
        insercao(utente(Id, Nome, noIdade ,Local)),
        insercao(interdito(Id,u)),
        testa(Li).

% -------------------------------------------------------
%  Evoluções cuidado:
% -------------------------------------------------------
evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), positivo) :-
    solucoes(I, +cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating)::I, Li),
    insercao(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating)),
    insercao(positivo(Id,c)),
    testa(Li),
    apagar((incerto(Id, c) :- (apagar((excecao(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))  :- incerto(Id,c)))))),
    subsImpreciso(Id,c).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating) , negativo ) :-
    solucoes( I, +(-cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating) )::I, Li ),
    insercao(-cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating) ),
    testa(Li),
    insercao(negativo(Id,c)).

evolucao( [cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating) | R], impreciso ) :-
    solucoes( Inv, imp(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::Inv, L ),
    insercao(excecao(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating) )),
    testa(L),
    insercao(impreciso(Id,c)),
    apagarCuidado(Id),
    apagarIncerto(Id,c),
    evolucao( R,impreciso ).

evolucao( [], impreciso ).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), incerto, data) :-
        solucoes(I, inc(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        insercao(cuidado(Id,zzzz,IdU,IdP,Tipo,Custo,Rating)),
        testa(Li),
        insercao(incerto(Id,c)).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), incerto, idu) :-
        solucoes(I, inc(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        insercao(cuidado(Id,Data,zzzz,IdP,Tipo,Custo,Rating)),
        testa(Li),
        insercao(incerto(Id,c)).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), incerto, idp) :-
        solucoes(I, inc(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        insercao(cuidado(Id,Data,IdU,zzzz,Tipo,Custo,Rating)),
        testa(Li),
        insercao(incerto(Id,c)).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), incerto, descricao) :-
        solucoes(I, inc(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        insercao(cuidado(Id,Data,IdU,IdP,zzzz,Custo,Rating)),
        testa(Li),
        insercao(incerto(Id,c)).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), incerto, custo) :-
        solucoes(I, inc(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        insercao(cuidado(Id,Data,IdU,IdP,Tipo,zzzz,Rating)),
        testa(Li),
        insercao(incerto(Id,c)).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), incerto, rating) :-
        solucoes(I, inc(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        insercao(cuidado(Id,Data,IdU,IdP,Tipo,Custo,zzzz)),
        testa(Li),
        insercao(incerto(Id,c)).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), interdito, data) :-
        solucoes(I, int(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        insercao(cuidado(Id,noData,IdU,IdP,Tipo,Custo,Rating)),
        insercao(interdito(Id,c)),
        testa(Li).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), interdito, idu) :-
        solucoes(I, int(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        insercao(cuidado(Id,Data,noIdu,IdP,Tipo,Custo,Rating)),
        insercao(interdito(Id,c)),
        testa(Li).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), interdito, idp) :-
        solucoes(I, int(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        insercao(cuidado(Id,Data,IdU,noIdp,Tipo,Custo,Rating)),
        insercao(interdito(Id,c)),
        testa(Li).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), interdito, descricao) :-
        solucoes(I, int(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        insercao(cuidado(Id,Data,IdU,IdP,noDescricao,Custo,Rating)),
        insercao(interdito(Id,c)),
        testa(Li).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), interdito, custo) :-
        solucoes(I, int(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        insercao(cuidado(Id,Data,IdU,IdP,Tipo,noCusto,Rating)),
        insercao(interdito(Id,c)),
        testa(Li).

evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), interdito, rating) :-
        solucoes(I, int(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        insercao(cuidado(Id,Data,IdU,IdP,Tipo,Custo,noRating)),
        insercao(interdito(Id,c)),
        testa(Li).


insercao(T) :- assert(T).
insercao(T) :- retract(T),!,fail.

testa([]).
testa([H|T]) :- H, testa(T).

%--------------------------------------------------------
% --------------- Invariantes de evolução ---------------
% -------------------------------------------------------
% -------------------------------------------------------
%--------------------------------------------------------
% Invariantes de Conhecimento Positivo:  nao permitir a insercao de conhecimento
%                                        repetido
% -------------------------------------------------------
+utente(Id,Nome,Idade,Local) :: (solucoes( (Id,Nome,Idade,Local), (utente(Id,Nome,Idade,Local)),S ),
                                 comprimento( S,N ),
				                 N==1).

+prestador(Id,Nome,Especialidade,Local) :: (solucoes( (Id,Nome,Especialidade,Local), prestador(Id,Nome,Especialidade,Local), S ),
                                            comprimento( S,N ),
				                            N==1).

+cuidado(Id, Data, IdU,IdP, Descricao,Custo, Rating) :: (solucoes( (Id, Data, IdU,IdP, Descricao,Custo), cuidado(Id, Data, IdU,IdP, Descricao,Custo, Rating), S ),
                                            comprimento( S,N ),

				                            N==1).

% Não permitir a inserção de conhecimento já existente positivo
+prestador(Id,_,_,_) :: (solucoes( (Id), positivo(Id,p),S ),
                                         comprimento( S,N ),
   			                             N==1).


+utente(Id,_,_,_) :: (solucoes( (Id), positivo(Id,u),S ),
                                 comprimento( S,N ),
				                 N==1).



+cuidado(Id,_,_,_,_,_,_) :: (solucoes( (Id), positivo(Id,c),S ),
                                         comprimento( S,N ),
   			                             N==1).

% Não permitir a inserção de conhecimento interdito
+prestador(Id,_,_,_) :: (solucoes( Id, (interdito(Id,p)),S ),
                                            comprimento( S,N ),
                                                        N==0). 


+utente(Id,_,_,_) :: (solucoes( Id, (interdito(Id,u)),S ),
                                            comprimento( S,N ),
                                                        N==0). 

+cuidado(Id,_,_,_,_,_,_) :: (solucoes( Id, (interdito(Id,c)),S ),
                                            comprimento( S,N ),
                                                        N==0). 

% Não permitir a inserção de conhecimento contraditório (com neg)
+prestador( Id,Nome,Especialidade,Instituicao) :: ( solucoes( (Id), -prestador( Id,Nome,Especialidade,Instituicao), S ),
                            comprimento( S, N ),
                            N == 0 ).

+utente( Id,Nome,Idade,Morada ) :: ( solucoes( (Id), -utente( Id,Nome,Idade,Morada ), S ),
                            comprimento( S, N ),
                            N == 0 ).

+cuidado( Id,Data,Idu,Idp,Descricao,Custo,Rating ) :: ( solucoes( (Id), -cuidado( Id,Data,Idu,Idp,Descricao,Custo,Rating ), S ),
                            comprimento( S, N ),
                            N == 0 ).
%--------------------------------------------------------
% Invariantes de Conhecimento Negativo:  nao permitir a insercao de conhecimento
%                                        repetido
% -------------------------------------------------------

+(-prestador( Id,_,_,_ )) :: ( solucoes( (Id), negativo(Id,p), S ),
                            comprimento( S, N ),
                            N == 1 ).

+(-utente( Id,_,_,_ )) :: ( solucoes( (Id), negativo(Id,u), S ),
                            comprimento( S, N ),
                            N == 1 ).

+(-cuidado( Id,_,_,_,_,_ )) :: ( solucoes( (Id), negativo(Id,c), S ),
                            comprimento( S, N ),
                            N == 1 ).
                            
% Não permitir a inserção de conhecimento contraditório (com pos)
+(-prestador( Id,Nome,Especialidade,Instituicao)) :: ( solucoes( (Id), prestador( Id,Nome,Especialidade,Instituicao), S ),
                            comprimento( S, N ),
                            N == 0 ).

+(-utente( Id,Nome,Idade,Morada )) :: ( solucoes( (Id), utente( Id,Nome,Idade,Morada ), S ),
                            comprimento( S, N ),
                            N == 0 ).

+(-cuidado( Id,Data,Idu,Idp,Descricao,Custo,Rating )) :: ( solucoes( (Id), cuidado( Id,Data,Idu,Idp,Descricao,Custo,Rating ), S ),
                            comprimento( S, N ),
                            N == 0 ).

% Invariante que impede a inserção de conhecimento contraditório (com impreciso) e a atualização de conhecimento interdito
+(-prestador(Id, Nome, Especialidade, Local)) :: (solucoes(
        (Id, Nome, Especialidade, Local),
        (excecao(prestador(Id,Nome,Especialidade,Local))),
        S),
        comprimento(S,N), N==0).

+(-utente(Id, Nome, Idade, Morada)) :: (solucoes(
        (Id, Nome, Idade, Morada),
        (excecao(prestador(Id,Nome,Idade,Morada))),
        S),
        comprimento(S,N), N==0).

+(-cuidado(Id, Data, IdU, IdP, Descricao, Custo, Rating)) :: (solucoes(
        (Id, Data, IdU, IdP, Nome, Descricao, Custo, Rating),
        (excecao(prestador(Id, Data, IdU, IdP,Nome,Descricao,Custo, Rating))),
        S),
        comprimento(S,N), N==0).

%--------------------------------------------------------
% Invariantes de Conhecimento Incerto:  nao permitir a evolucao para                                                    conhecimento incerto
% -------------------------------------------------------
inc(prestador(Id,_,_,_)) :: (solucoes( (Id,_,_,_), (prestador(Id,_,_,_)),S ),
                                            comprimento( S,N ),
				                N==1).

inc(utente(Id,_,_,_)) :: (solucoes( (Id,_,_,_), (utente(Id,_,_,_)),S ),
                                            comprimento( S,N ),
				                N==1).

inc(cuidado(Id,_,_,_,_,_,_)) :: (solucoes( (Id,_,_,_,_,_), (cuidado(Id,_,_,_,_,_,_)),S ),
                                            comprimento( S,N ),
				                N==1).

%--------------------------------------------------------
% Invariantes de Conhecimento Impreciso:  nao permitir a evolucao de interdito
% -------------------------------------------------------
imp(prestador(Id,_,_,_)) :: (solucoes( Id, (interdito(Id,p)),S ),
                                            comprimento( S,N ),
                                                N==0). 
                                                
imp(utente(Id,_,_,_)) :: (solucoes( Id, (interdito(Id,u)),S ),
                                            comprimento( S,N ),
                                                N==0). 

imp(cuidado(Id,_,_,_,_,_,_)) :: (solucoes( Id, (interdito(Id,c)),S ),
                                            comprimento( S,N ),
                                                N==0).

% Impedir inserção de conhecimento impreciso se houver positivo
imp(prestador(Id,_,_,_)) :: (solucoes( (Id), (positivo(Id,p)),S ),
                                            comprimento( S,N ),
				                N==0).

imp(utente(Id,_,_,_)) :: (solucoes( (Id), (positivo(Id,u)),S ),
                                            comprimento( S,N ),
				                N==0).

imp(cuidado(Id,_,_,_,_,_,_)) :: (solucoes( (Id), (positivo(Id,c)),S ),
                                            comprimento( S,N ),
				                N==0).

% Impedir inserção de conhecimento repetido
imp(prestador(Id,Nome,Especialidade,Local)) :: (solucoes( (Id,Nome,Especialidade,Local), (prestador(Id,Nome, Especialidade, Local)),S ),
                                            comprimento( S,N ),
				                N==1).

imp(utente(Id,Nome,Idade,Morada)) :: (solucoes( (Id,Nome,Idade,Morada), (utente(Id,Nome, Idade, Morada)),S ),
                                            comprimento( S,N ),
				                N==1).

imp(cuidado(Id,Data, IdU, IdP,Descricao,Custo, Rating)) :: (solucoes( (Id,Data, IdU, IdP,Descricao,Custo, Rating), (cuidado(Id,Data, IdU, IdP,Descricao, Custo, Rating)),S ),
                                            comprimento( S,N ),
				                N==1).

%--------------------------------------------------------
% Invariantes de Conhecimento Interdito:  nao permitir a evolucao para                                                    conhecimento incerto
% -------------------------------------------------------

int(prestador(Id,_,_,_)) :: (solucoes( (Id,_,_,_), (prestador(Id,_,_,_)),S ),
                                            comprimento( S,N ),
                                            N==1).

int(utente(Id,_,_,_)) :: (solucoes( (Id,_,_,_), (utente(Id,_,_,_)),S ),
                                            comprimento( S,N ),
				                N==1).

int(cuidado(Id,_,_,_,_,_,_)) :: (solucoes( (Id,_,_,_,_,_,_), (cuidado(Id,_,_,_,_,_,_)),S ),
                                            comprimento( S,N ),
				                N==1).

% -------------------------------------------------------
%  Involuções:
% -------------------------------------------------------
% involucao: F -> {V,F}
involucao( P ) :- solucoes(Inv, -P::Inv, Li),
                apagar(P),
                testa(Li).

% involucao: F, Type -> {V,F}
involucao( F,positivo ) :-
    solucoes(Inv, -F::Inv, Li),
    apagar(F),
    testa(Li).

involucao( F,negativo ) :-
    solucoes(Inv, -(-F)::Inv, Li),
    apagar(F),
    testa(Li).

involucao( [OPT1 | R], impreciso ) :-
    solucoes( Inv, -OPT1::Inv, L ),
    apagar(excecao(OPT1)),
    testa(L),
    involucao( R,impreciso ).

involucao( [], impreciso ).

involucao( prestador( Id,Nome,Especialidade,_ ), incerto, local) :-
        involucao( prestador( Id,Nome,Especialidade,yyyy ), positivo ).

involucao( prestador( Id,_,Especialidade,Local ), incerto, nome) :-
        involucao( prestador( Id,yyyy,Especialidade,Local ), positivo ).

involucao( prestador( Id,Nome,_,Local ), incerto, especialidade) :-
        involucao( prestador( Id,Nome,yyyy,Local ), positivo ).

involucao( prestador( Id,Nome,Especialidade,Local ), incerto, id) :-
        involucao( prestador( yyyy,Nome,Especialidade,Local ), positivo ).

involucao( prestador( Id,Nome,Especialidade,Local ), interdito, local) :-
        involucao( prestador( Id,Nome,Especialidade,noLocal ), positivo ).

involucao( prestador( Id,Nome,Especialidade,Local ), interdito, nome) :-
        involucao( prestador( Id,noName,Especialidade,Local ), positivo ).

involucao( prestador( Id,Nome,Especialidade,Local ), interdito, especialidade):-
        involucao( prestador( Id,Nome,noEspecialidade,Local ), positivo ).

involucao( prestador( Id,Nome,Especialidade,Local ), interdito, id) :-
        involucao( prestador( noId,Nome,Especialidade,Local ), positivo ).

apagar(T) :- retract(T).
apagar(T) :- assert(T),!,fail.

%--------------------------------------------------------
% --------------- Invariantes de involução --------------
% -------------------------------------------------------
% -------------------------------------------------------

% Pressupostos do mundo fechado
-Q :- nao(Q), nao(excecao(Q)).

% -------------------------------------------------------
% Invariante Referencial:  não permitir a remoção de utentes/prestadores
%                          que têm um cuidado associado
% -------------------------------------------------------
-utente(IdUt,_,_,_) :: (solucoes( (IdUt),(cuidado(_,_,IdUt,_,_,_,_)),S ),
                                  comprimento( S,N ),
				                  N==0).

-prestador(IdPrest,Nome,Especialidade,Local) :: (solucoes( (IdPrest), cuidado(_,_,_,IdPrest,_,_,_),S ),
                                                comprimento( S,N ),
				                                N==0).

% ------------------------------------------------------
% --------------- Conhecimento Incerto -----------------
% ------------------------------------------------------

excecao(utente(Id,_,Idade,Local)):- utente(Id,xxxx,Idade,Local).
excecao(utente(Id,Nome,_,Local)):- utente(Id,Nome,xxxx,Local).
excecao(utente(Id,Nome,Idade,_)):- utente(Id,Nome,Idade,xxxx).

excecao(prestador(Id,_,Especialidade,Local)) :- prestador(Id,yyyy,Especialidade,Local).
excecao(prestador(Id,Nome,_,Local)) :- prestador(Id,Nome,yyyy,Local).
excecao(prestador(Id,Nome,Especialidade,_)) :- prestador(Id,Nome,Especialidade,yyyy).

%TODO: Fazer para os outros. O xxxx e yyyy foram nomes que escolhi para representar quando não sabemos quem foi (mas podemos vir a saber)

% ------------------------------------------------------
% -------------- Conhecimento Impreciso ----------------
% ------------------------------------------------------
% Há um utente de id 100 com 22 anos da guarda que ou se chama maria ou joana
excecao(utente(100,maria,22,guarda)).
excecao(utente(100,joana,22,guarda)).

excecao(prestador(50,dr_paula_barros,cardiologia,sao_joao)).
excecao(prestador(50,dr_paula_barros,infectologia,sao_joao)).
excecao(prestador(54, dr_ivo_moreira,medicina_geral, yyyy)).

excecao(cuidado(51,noData,13,9,sopro_no_coracao,15,81)).
excecao(cuidado(52,2003-09-17,7,7,otite,30,noRating)).
excecao(cuidado(53,2003-09-17,15,2,constipacao,noCusto,70)).

excecao(cuidado(14,2016-06-04,14,1,eczema,45,100)).
excecao(cuidado(5,2008-03-22,5,5,catarata,30,30)).

%TODO: HOW TO DO EXCECOES
%TODO: Involucoes (nao mexer nos invariantes, involucoes são easy)
%TODO: Confirmar que estão TODOS os invariantes
% ------------------------------------------------------
% -------------- Conhecimento Interdito ----------------
% ------------------------------------------------------
% Quando não se sabe nem nunca se vai saber algo de um utente
excecao(utente(Id,_,Idade,Local)):- utente(Id,noName,Idade,Local).
excecao(utente(Id,Nome,_,Local)):- utente(Id,Nome,noIdade,Local).
excecao(utente(Id,Nome,Idade,_)):- utente(Id,Nome,Idade,noLocal).

excecao(prestador(Id,_,Especialidade,Local)):- prestador(Id,noName,Especialidade,Local).
excecao(prestador(Id,Nome,_,Local)):- prestador(Id,Nome,noEspecialidade,Local).
excecao(prestador(Id,Nome,Especialidade,_)):- prestador(Id,Nome,Especialidade,noLocal).

nulo(noName).
nulo(noIdade).
nulo(noLocal).
nulo(noEspecialidade).
nulo(noDescricao).
nulo(noCusto).
nulo(noRating).
nulo(noData).

%:- utente(Id, marta_silva, 39, faro), nao(nulo(Id)).
%TODO: Fazer para os outros.


