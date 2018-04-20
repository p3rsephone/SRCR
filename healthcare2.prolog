:-include('factos.prolog').
% utente : #IdUt, Nome, Idade, Morada --> {V,F}
% prestador : #IdPrest, Nome, Especialidade, Instituição --> {V,F}
% cuidado : Data, #idUt, #IdPrest, Descrição, Custo --> {V,F}
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
:- dynamic excecao/1.

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
% -------------------------------------------------------
%  Registar utentes, prestadores e cuidados de saúde:
% -------------------------------------------------------

subsImpreciso(Id,p):-
        impreciso(Id,p),
        solucoes((excecao(prestador(Id,N,E,L))),excecao(prestador(Id,N,E,L)),N),
        apagarTudo(N),
        apagar(impreciso(Id,p)).

subsImpreciso(Id,p).

apagarTudo([H|T]):-
        apagar(H),
        apagarTudo(T).

apagarTudo([]).

apagarIncerto(Id,T) :-
        incerto(Id,T),
        apagar(incerto(Id,T)).

apagarIncerto(Id,T).

apagarPrestador(Id) :-
        incerto(Id,p),
        apagar(prestador(Id,_,_,_)).

apagarPrestador(Id).

apagarUtente(Id) :-
        incerto(Id,u),
        apagar(utente(Id,_,_,_)).

apagarUtente(Id).

apagarCuidado(Id) :-
        incerto(Id,c),
        apagar(cuidado(Id,_,_,_,_,_,_)).

apagarCuidado(Id).

%--------------------------------------------------------
%------------------evolucao prestador
%--------------------------------------------------------


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
% TODO: Everyone else

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


%--------------------------------------------------------
%------------------evolucao utente
%--------------------------------------------------------


evolucao( utente(Id, Nome, Idade ,Local), positivo) :-
    solucoes(I, +utente(Id, Nome, Idade ,Local)::I, Li),
    insercao(utente(Id, Nome, Idade ,Local)),
    insercao(positivo(Id,u)),
    testa(Li),
    apagarUtente(Id),
    apagarIncerto(Id,u),
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
% TODO: Everyone else

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


%--------------------------------------------------------
%------------------evolucao cuidados
%--------------------------------------------------------


evolucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), positivo) :-
    solucoes(I, +cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating)::I, Li),
    insercao(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating)),
    insercao(positivo(Id,c)),
    testa(Li),
    apagarPrestador(Id),
    apagarIncerto(Id,c),
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

% TODO: Everyone else

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
% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido
% -------------------------------------------------------
+utente(Id,Nome,Idade,Local) :: (solucoes( (Id,Nome,Idade,Local), (utente(Id,Nome,Idade,Local)),S ),
                                 comprimento( S,N ),
				                 N==1).
%sem conhecimento interdito
+utente(Id,Nome,Idade,Local) :: (solucoes( Id, (interdito(Id,u)),S ),
                                            comprimento( S,N ),
                                        N==0). 

% Garantir que não existe conhecimento positivo contraditótio
+(-utente(Id,Nome,Idade,Local)) :: ( solucoes( (Id), positivo(Id,u), S ),
                            comprimento( S, N ),
                            N == 0 ).

% Não repetir conhecimento negativo
+(-utente(Id,_,_,_)) :: ( solucoes( (Id), negativo(Id,u), S ),
                            comprimento( S, N ),
                            N == 1 ).

% Invariante que impede a inserção de conhecimento negativo acerca de conhecimento interdito sobre a especialidade de utentes
+(-utente(Id,Nome,Idade,Local)) :: (solucoes( Id, (interdito(Id,u)),S ),
                                                comprimento( S,N ),
				                N==0).

inc(utente(Id,Nome,Idade,Local)) :: (solucoes( (Id,Nome,Idade,Local), (utente(Id,_,_,_)),S ),
                                            comprimento( S,N ),
				                N==1).

imp(utente(Id,Nome,Idade,Local)) :: (solucoes( Id, (interdito(Id,u)),S ),
                                            comprimento( S,N ),
                                                N==0).

imp(utente(Id,Nome,Idade,Local)) :: (solucoes( (Id,Nome,Idade,Local), (positivo(Id,u)),S ),
                                            comprimento( S,N ),
				                N==0).

imp(utente(Id,Nome,Idade,Local)) :: (solucoes((Id,Nome,Idade,Local), (utente(Id,Nome,Idade,Local)),S ),
                                            comprimento( S,N ),
				                N==1).

int(utente(Id,Nome,Idade,Local)) :: (solucoes((Id,Nome,Idade,Local), (utente(Id,_,_,_)),S ),
                                            comprimento( S,N ),
                                        N==1).

+prestador(Id,Nome,Especialidade,Local) :: (solucoes( (Id,Nome,Especialidade,Local), prestador(Id,Nome,Especialidade,Local), S ),
                                            comprimento( S,N ),
				                            N==1).
%sem conhecimento interdito
+prestador(Id,Nome,Especialidade,Local) :: (solucoes( Id, (interdito(Id,p)),S ),
                                            comprimento( S,N ),
                                                        N==0). 

% Garantir que não existe conhecimento positivo contraditótio
+(-prestador( Id,_,_,_)) :: ( solucoes( (Id), positivo(Id,p), S ),
                            comprimento( S, N ),
                            N == 0 ).

% Não repetir conhecimento negativo
+(-prestador( Id,_,_,_ )) :: ( solucoes( (Id), negativo(Id,p), S ),
                            comprimento( S, N ),
                            N == 1 ).

% Invariante que impede a inserção de conhecimento negativo acerca de conhecimento interdito sobre a especialidade de utentes
+(-prestador(Id,Nome,Especialidade,Local)) :: (solucoes( Id, (interdito(Id,p)),S ),
                                                comprimento( S,N ),
				                N==0).

inc(prestador(Id,Nome,Especialidade,Local)) :: (solucoes( (Id,Nome,Especialidade,Local), (prestador(Id,_,_,_)),S ),
                                            comprimento( S,N ),
				                N==1).

imp(prestador(Id,Nome,Especialidade,Local)) :: (solucoes( Id, (interdito(Id,p)),S ),
                                            comprimento( S,N ),
                                                N==0). 

imp(prestador(Id,Nome,Especialidade,Local)) :: (solucoes( (Id,Nome,Especialidade,Local), (positivo(Id,p)),S ),
                                            comprimento( S,N ),
				                N==0).

imp(prestador(Id,Nome,Especialidade,Local)) :: (solucoes( (Id,Nome,Especialidade,Local), (prestador(Id,Nome, Especialidade, Local)),S ),
                                            comprimento( S,N ),
				                N==1).

int(prestador(Id,Nome,Especialidade,Local)) :: (solucoes( (Id,Nome,Especialidade,Local), (prestador(Id,_,_,_)),S ),
                                            comprimento( S,N ),
N==1).
%%TODO: adicionar prestador e cuidado
+cuidado(Id,Data,IdUt,IdPrest,Descricao,Custo,Rating) :: (solucoes( (Id,Data,IdUt,IdPrest,Descricao,Custo), (cuidado(Id,Data,IdUt,IdPrest,Descricao,Custo,Rating)),S ),
        comprimento( S,N ),
	N==1).

%sem conhecimento interdito
+cuidado(Id,Data,IdUt,IdPrest,Descricao,Custo,Rating) :: (solucoes( Id, (interdito(Id,c)),S ),
                                            comprimento( S,N ),
                                                        N==0). 

% Garantir que não existe conhecimento positivo contraditótio
+(-cuidado(Id,_,_,_,_,_,_)) :: ( solucoes( (Id), positivo(Id,c), S ),
                            comprimento( S, N ),
                            N == 0 ).

% Não repetir conhecimento negativo
+(-cuidado(Id,_,_,_,_,_,_)) :: ( solucoes( (Id), negativo(Id,c), S ),
                            comprimento( S, N ),
                            N == 1 ).

% Invariante que impede a inserção de conhecimento negativo acerca de conhecimento interdito sobre a especialidade de utentes
+(-cuidado(Id,Data,IdUt,IdPrest,Descricao,Custo,Rating)) :: (solucoes( Id, (interdito(Id,c)),S ),
                                                comprimento( S,N ),
				                N==0).

inc(cuidado(Id,Data,IdUt,IdPrest,Descricao,Custo,Rating)) :: (solucoes( (Id,Data,IdUt,IdPrest,Descricao,Custo,Rating), (cuidado(Id,_,_,_,_,_,_)),S ),
                                            comprimento( S,N ),
				                N==1).

imp(cuidado(Id,Data,IdUt,IdPrest,Descricao,Custo,Rating)) :: (solucoes( Id, (interdito(Id,c)),S ),
                                            comprimento( S,N ),
                                                N==0). 

imp(cuidado(Id,Data,IdUt,IdPrest,Descricao,Custo,Rating)) :: (solucoes( (Id,Data,IdUt,IdPrest,Descricao,Custo,Rating), (positivo(Id,c)),S ),
                                            comprimento( S,N ),
				                N==0).

imp(cuidado(Id,Data,IdUt,IdPrest,Descricao,Custo,Rating)) :: (solucoes((Id,Data,IdUt,IdPrest,Descricao,Custo,Rating), (cuidado(Id,Data,IdUt,IdPrest,Descricao,Custo,Rating)),S ),
                                            comprimento( S,N ),
				                N==1).

int(cuidado(Id,Data,IdUt,IdPrest,Descricao,Custo,Rating)) :: (solucoes((Id,Data,IdUt,IdPrest,Descricao,Custo,Rating), (cuidado(Id,_,_,_,_,_,_)),S ),
                                            comprimento( S,N ),
                                        N==1).
% Invariante que impede a inserção de conhecimento negativo acerca de conhecimento impreciso com valores contraditórios
% Prestador
+(-prestador(Id, Nome, Especialidade, Local)) :: (solucoes(
        (Id, Nome, Especialidade, Local),
        (excecao(prestador(Id,Nome,Especialidade,Local))),
        S),
        comprimento(S,N), N==0)

+(-utente(Id, Nome, Idade, Morada)) :: (solucoes(
        (Id, Nome, Idade, Morada),
        (excecao(prestador(Id,Nome,Idade,Morada))),
        S),
        comprimento(S,N), N==0)

+(-cuidado(Id,Data, IdU, IdP, Descricao, Utente)) :: (solucoes(
        (Id,Data, IdU, IdP, Nome, Descricao, Utente),
        (excecao(cuidado(Id,Data, IdU, IdP,Nome,Descricao,Utente))),
        S),
        comprimento(S,N), N==0)

% -------------------------------------------------------
% Invariante Referencial : não permitir a inserção de utentes/prestadores
%                          positivo se já existir positivo
% -------------------------------------------------------
+utente(Id,_,_,_) :: (solucoes( (Id), positivo(Id,u),S ),
                                 comprimento( S,N ),
				                 N==1).

+prestador(Id,_,_,_) :: (solucoes( (Id), positivo(Id,p),S ),
                                         comprimento( S,N ),
   			                             N==1).

%TODO: cuidado

% -------------------------------------------------------
% Invariante Referencial:  não permitir a remoção de utentes/prestadores
%                          que têm um cuidado associado
% -------------------------------------------------------
-utente(IdUt,_,_,_) :: (solucoes( (IdUt),(cuidado(_,IdUt,_,_,_,_)),S ),
                                  comprimento( S,N ),
				                  N==0).

-prestador(IdPrest,Nome,Especialidade,Local) :: (solucoes( (IdPrest,Nome,Especialidade,Local), cuidado(_,_,IdPrest,_,_,_),S ),
                                                comprimento( S,N ),
				                                N==0).
% ------------------------------------------------------
%  Remover utentes, prestadores e cuidados de saúde:
% ------------------------------------------------------
%--------------------------------------------------------
%------------------evolucao prestador
%--------------------------------------------------------


% evolucao: F, Type -> {V,F}
involucao( prestador(Id, Nome, E,Ins), positivo) :-
    solucoes(I, -prestador(Id,Nome,E,Ins)::I, Li),
    apagar(prestador(Id, Nome, E,Ins)),
    apagar(positivo(Id,p)),
    testa(Li).

involucao( prestador(Id,Nome,Especialidade,Local) , negativo ) :-
    solucoes( I, -(-prestador(Id,Nome,Especialidade,Local) )::I, Li ),
    apagar(-prestador(Id,Nome,Especialidade,Local) ),
    testa(Li),
    apagar(negativo(Id,p)).

involucao( [prestador(Id,Nome,Especialidade,Local) | R], impreciso ) :-
    solucoes( Inv, -imp(prestador(Id,Nome,Especialidade,Local))::Inv, L ),
    apagar(excecao(prestador(Id,Nome,Especialidade,Local) )),
    testa(L),
    apagar(impreciso(Id,p)),
    evolucao( R,impreciso ).

involucao( [], impreciso ).

involucao( prestador( Id,Nome,Especialidade,Local ), incerto, local) :-
        solucoes(I, -inc(prestador(Id,Nome,Especialidade,Local))::I, Li),
        apagar(prestador( Id,Nome,Especialidade,yyyy)),
        testa(Li),
        apagar(incerto(Id,p)).

involucao( prestador( Id,Nome,Especialidade,Local ), incerto, nome) :-
        solucoes(I, -inc(prestador(Id,Nome,Especialidade,Local))::I, Li),
        apagar(prestador( Id,yyyy,Especialidade,Local )),
        testa(Li),
        apagar(incerto(Id,p)).

involucao( prestador( Id,Nome,Especialidade,Local ), incerto, especialidade) :-
        solucoes(I, -inc(prestador(Id,Nome,Especialidade,Local))::I, Li),
        apagar(prestador( Id,Nome,yyyy,Local )),
        testa(Li),
        apagar(incerto(Id,p)).
% TODO: Everyone else

involucao( prestador( Id,Nome,Especialidade,Local ), interdito, local) :-
        solucoes(I, -int(prestador(Id,Nome,Especialidade,Local))::I, Li),
        apagar(prestador( Id,Nome,Especialidade,noLocal )),
        apagar(interdito(Id,p)),
        testa(Li).

involucao( prestador( Id,Nome,Especialidade,Local ), interdito, nome) :-
        solucoes(I, -int(prestador(Id,Nome,Especialidade,Local))::I, Li),
        apagar(prestador( Id,noName,Especialidade,Local)),
        apagar(interdito(Id,p)),
        testa(Li).

involucao( prestador( Id,Nome,Especialidade,Local ), interdito, especialidade) :-
        solucoes(I, -int(prestador(Id,Nome,Especialidade,Local))::I, Li),
        apagar(prestador( Id,Nome,noEspecialidade,Local )),
        apagar(interdito(Id,p)),
        testa(Li).


%--------------------------------------------------------
%------------------evolucao utente
%--------------------------------------------------------


involucao( utente(Id, Nome, Idade ,Local), positivo) :-
    solucoes(I, -utente(Id, Nome, Idade ,Local)::I, Li),
    apagar(utente(Id, Nome, Idade ,Local)),
    apagar(positivo(Id,u)),
    testa(Li).

involucao( utente(Id, Nome, Idade ,Local) , negativo ) :-
    solucoes( I, -(-utente(Id, Nome, Idade ,Local) )::I, Li ),
    apagar(-utente(Id, Nome, Idade ,Local) ),
    testa(Li),
    apagar(negativo(Id,u)).

involucao( [utente(Id, Nome, Idade ,Local) | R], impreciso ) :-
    solucoes( Inv, -imp(utente(Id, Nome, Idade ,Local))::Inv, L ),
    apagar(excecao(utente(Id, Nome, Idade ,Local) )),
    testa(L),
    apagar(impreciso(Id,u)),
    evolucao( R,impreciso ).

involucao( [], impreciso ).

involucao( utente(Id, Nome, Idade ,Local), incerto, local) :-
        solucoes(I, -inc(utente(Id, Nome, Idade ,Local))::I, Li),
        apagar(utente(Id, Nome, Idade ,xxxx)),
        testa(Li),
        apagar(incerto(Id,p)).

involucao( prestador( Id,Nome,Especialidade,Local ), incerto, nome) :-
        solucoes(I, -inc(utente(Id, Nome, Idade ,Local))::I, Li),
        apagar(utente(Id, xxxx, Idade ,Local)),
        testa(Li),
        apagar(incerto(Id,u)).

involucao( utente(Id, Nome, Idade ,Local), incerto, idade) :-
        solucoes(I, -inc(utente(Id, Nome, Idade ,Local))::I, Li),
        apagar(utente(Id, Nome, xxxx ,Local)),
        testa(Li),
        apagar(incerto(Id,u)).
% TODO: Everyone else

involucao( utente(Id, Nome, Idade ,Local), interdito, local) :-
        solucoes(I, -int(utente(Id, Nome, Idade ,Local))::I, Li),
        apagar(utente(Id, Nome, Idade ,noLocal)),
        apagar(interdito(Id,u)),
        testa(Li).

involucao( utente(Id, Nome, Idade ,Local), interdito, nome) :-
        solucoes(I, -int(utente(Id, Nome, Idade ,Local))::I, Li),
        apagar(utente(Id, noName, Idade ,Local)),
        apagar(interdito(Id,u)),
        testa(Li).

involucao( utente(Id, Nome, Idade ,Local), interdito, idade) :-
        solucoes(I, -int(utente(Id, Nome, Idade ,Local))::I, Li),
        apagar(utente(Id, Nome, noIdade ,Local)),
        apagar(interdito(Id,u)),
        testa(Li).


%--------------------------------------------------------
%------------------evolucao cuidados
%--------------------------------------------------------


involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), positivo) :-
    solucoes(I, -cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating)::I, Li),
    apagar(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating)),
    insercao(positivo(Id,c)),
    testa(Li).

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating) , negativo ) :-
    solucoes( I, -(-cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating) )::I, Li ),
    apagar(-cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating) ),
    testa(Li),
    apagar(negativo(Id,c)).

involucao( [cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating) | R], impreciso ) :-
    solucoes( Inv, -imp(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::Inv, L ),
    apagar(excecao(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating) )),
    testa(L),
    apagar(impreciso(Id,c)),
    evolucao( R,impreciso ).

involucao( [], impreciso ).

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), incerto, data) :-
        solucoes(I, -inc(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        apagar(cuidado(Id,zzzz,IdU,IdP,Tipo,Custo,Rating)),
        testa(Li),
        apagar(incerto(Id,c)).

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), incerto, idu) :-
        solucoes(I, -inc(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        apagar(cuidado(Id,Data,zzzz,IdP,Tipo,Custo,Rating)),
        testa(Li),
        apagar(incerto(Id,c)).

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), incerto, idp) :-
        solucoes(I, -inc(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        apagar(cuidado(Id,Data,IdU,zzzz,Tipo,Custo,Rating)),
        testa(Li),
        apagar(incerto(Id,c)).

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), incerto, descricao) :-
        solucoes(I, -inc(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        apagar(cuidado(Id,Data,IdU,IdP,zzzz,Custo,Rating)),
        testa(Li),
        apagar(incerto(Id,c)).

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), incerto, custo) :-
        solucoes(I, -inc(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        apagar(cuidado(Id,Data,IdU,IdP,Tipo,zzzz,Rating)),
        testa(Li),
        apagar(incerto(Id,c)).

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), incerto, rating) :-
        solucoes(I, -inc(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        apagar(cuidado(Id,Data,IdU,IdP,Tipo,Custo,zzzz)),
        testa(Li),
        apagar(incerto(Id,c)).

% TODO: Everyone else

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), interdito, data) :-
        solucoes(I, -int(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        apagar(cuidado(Id,noData,IdU,IdP,Tipo,Custo,Rating)),
        apagar(interdito(Id,c)),
        testa(Li).

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), interdito, idu) :-
        solucoes(I, -int(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        apagar(cuidado(Id,Data,noIdu,IdP,Tipo,Custo,Rating)),
        apagar(interdito(Id,c)),
        testa(Li).

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), interdito, idp) :-
        solucoes(I, -int(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        apagar(cuidado(Id,Data,IdU,noIdp,Tipo,Custo,Rating)),
        apagar(interdito(Id,c)),
        testa(Li).

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), interdito, descricao) :-
        solucoes(I, -int(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        apagar(cuidado(Id,Data,IdU,IdP,noDescricao,Custo,Rating)),
        apagar(interdito(Id,c)),
        testa(Li).

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), interdito, custo) :-
        solucoes(I, -int(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        apagar(cuidado(Id,Data,IdU,IdP,Tipo,noCusto,Rating)),
        apagar(interdito(Id,c)),
        testa(Li).

involucao( cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating), interdito, rating) :-
        solucoes(I, -int(cuidado(Id,Data,IdU,IdP,Tipo,Custo,Rating))::I, Li),
        apagar(cuidado(Id,Data,IdU,IdP,Tipo,Custo,noRating)),
        apagar(interdito(Id,c)),
        testa(Li).


apagar(T) :- retract(T).
apagar(T) :- assert(T),!,fail.

% ------------------------------------------------------
% --------------- Conhecimento Negativo ----------------
% ------------------------------------------------------
%Predicado -Predicado:
%ID do utente, Nome do utente, Idade do Utente, Cidade do Utente
-Q :- nao(Q), nao(excecao(Q)).

% ------------------------------------------------------
% --------------- Conhecimento Incerto -----------------
% ------------------------------------------------------

excecao(utente(Id,_,Idade,Local)):- utente(Id,john_doe,Idade,Local).
excecao(utente(Id,Nome,_,Local)):- utente(Id,Nome,xxxx,Local).
excecao(utente(Id,Nome,Idade,_)):- utente(Id,Nome,Idade,xxxx).

excecao(prestador(_,Nome,Especialidade,Local)) :- prestador(yyyy,Nome,Especialidade,Local).
excecao(prestador(Id,_,Especialidade,Local)) :- prestador(Id,yyyy,Especialidade,Local).
excecao(prestador(Id,Nome,_,Local)) :- prestador(Id,Nome,yyyy,Local).
excecao(prestador(Id,Nome,Especialidade,_)) :- prestador(Id,Nome,Especialidade,yyyy).

%TODO: Fazer para os outros. O xxxx e john_doe foram nomes que escolhi para representar quando não sabemos quem foi (mas podemos vir a saber)

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
%TODO: Add excecao nas evolucoes para qq cena imperfeitas
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

nulo(noIdu).
nulo(noIdp).
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


