%
% utente : #IdUt, Nome, Idade, Morada --> {V,F}
% prestador : #IdPrest, Nome, Especialidade, Instituição --> {V,F}
% cuidado : Data, #idUt, #IdPrest, Descrição, Custo --> {V,F}
% -------------------------------------------------------
% Definições iniciais
:- op( 900,xfy,'::').
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.

% -------------------------------------------------------
% Predicados auxiliares

pertence(H,[H|T]).
pertence(X,[H|T]) :-
	X \= H,
pertence(X,T).

nao(Q) :- Q,!,false.
nao(Q).

unicos([],[]).
unicos([H|T], R) :-
	pertence(H,T),
	unicos(T,R).
unicos([H|T], [H|R]) :-
	nao(pertence(H,T)),
	unicos(T,R).


comprimento([H],1).
comprimento([H|T],N) :- comprimento(T,M), N is M+1.

solucoes(T,Q,S) :- findall(T,Q,S).
% -------------------------------------------------------
% Factos
utente(1, mario_silva, 18, faro).
utente(2, raquel_guimaraes, 37, lisboa).
utente(3, joaquim_canas, 25, braga).
utente(4, vania_costa, 67, viseu).

prestador(1, dr_joao_lopes, dermatologia, sao_joao).
prestador(2, dr_marisa_silva, medicina_geral, santo_antonio).
prestador(3, dr_bruno_ferreira, cardiologia, santa_maria).
prestador(4, dr_fernanda_moreira, cirurgia_geral, cufe).

cuidado(2008-03-22,1,2,constipacao,15).
cuidado(2015-05-03,2,3,arritmia,45).
cuidado(2003-09-17,3,1,eczema,30).
cuidado(2017-12-30,4,4,hernia,150).
% -------------------------------------------------------
%  Registar utentes, prestadores e cuidados de saúde:
%
regista(T) :- solucoes(Inv, +T::Inv, S),
               insercao(T),
               testa(S).

insercao(T) :- assert(T).
insercao(T) :- retract(T),!,fail.

testa([]).
testa([H|T]) :- H, testa(T).
%--------------------------------------------------------
% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido
+utente(Id,Nome,Idade,Local) :: (solucoes( (Id,Nome,Idade,Local), (utente(Id,Nome,Idade,Local)),S ),
                                 comprimento( S,N ),
				                 N==1).

+prestador(Id,Nome,Especialidade,Local) :: (solucoes( (Id,Nome,Especialidade,Local), (utente(Id,Nome,Especialidade,Local)),S ),
                                            comprimento( S,N ),
				                            N==1).

+cuidado(Data,IdUt,IdPrest,Descricao,Custo) :: (solucoes( (Data,IdUt,IdPrest,Descricao,Custo), (cuidado(Data,IdUt,IdPrest,Descricao,Custo)),S ),
                                                comprimento( S,N ),
				                                N==1).

% Invariante Estrutural:  não permitir a remoção de utentes/prestadores
%                         que têm um cuidado associado
-utente(IdUt,Nome,Idade,Local) :: (solucoes( (IdUt),(cuidado(Data,IdUt,IdPrest,Descricao,Custo)),S ),
                                  comprimento( S,N ),
				                  N==0).

-prestador(IdPrest,Nome,Especialidade,Local) :: (solucoes( (IdPrest,Nome,Especialidade,Local), cuidado(Data,IdUt,IdPrest,Descricao,Custo),S ),
                                                comprimento( S,N ),
				                                N==0).
% ------------------------------------------------------
%  Remover utentes, prestadores e cuidados de saúde:
%
remove(T) :- solucoes(I,-T::I,S),
              apagar(T),
              testa(S).

apagar(T) :- retract(T).
apagar(T) :- assert(T),!,fail.
% ------------------------------------------------------
%  Identificar utentes (Critérios de seleção):
%
%
%  Identificar utentes de um prestador/especialidade/instituição:
%
utentes_pei(P,E,I,S):-
  solucoes(
    (IdU,Nome,Idade,Morada),
    (
      cuidado(_,IdU,IdP,_,_),
      prestador(IdP,P,E,I),
      utente(IdU,Nome,Idade,Morada)
    ),
   L
  ),
  unicos(L,S).

utentes_p(P,S):-
  solucoes(
    (IdU,Nome,Idade,Morada),
    (
      cuidado(_,IdU,IdP,_,_),
      prestador(IdP,P,_,_),
      utente(IdU,Nome,Idade,Morada)
    ),
   L
  ),
  unicos(L,S).

utentes_e(E,S):-
  solucoes(
    (IdU,Nome,Idade,Morada),
    (
      cuidado(_,IdU,IdP,_,_),
      prestador(IdP,_,E,_),
      utente(IdU,Nome,Idade,Morada)
    ),
   L
  ),
  unicos(L,S).

utentes_i(I,S):-
  solucoes(
    (IdU,Nome,Idade,Morada),
    (
      cuidado(_,IdU,IdP,_,_),
      prestador(IdP,_,_,I),
      utente(IdU,Nome,Idade,Morada)
    ),
   L
  ),
  unicos(L,S).
% ------------------------------------------------------
%  Identificar as instituições:
%

% ------------------------------------------------------
%  Identificar os cuidados de saúde prestados por instituição/cidade/datas:
%


%  Identificar cuidados de saúde realizados por utente/instituição/prestador
%
cuidados_saude(U,I,P,S):-
  solucoes(
    (Data,Descricao,Custo),
    (
      cuidado(Data,IdU,IdP,Descricao,Custo),
      prestador(IdP,P,_,I),
      utente(IdU,U,_,_)
    ),
   S
 ).

cuidados_saude_u(U,S):-
  solucoes(
    (Data,Descricao,Custo),
    (
      cuidado(Data,IdU,IdP,Descricao,Custo),
      prestador(IdP,_,_,_),
      utente(IdU,U,_,_)
    ),
   S
 ).

cuidados_saude_i(I,S):-
  solucoes(
    (Data,Descricao,Custo),
    (
      cuidado(Data,IdU,IdP,Descricao,Custo),
      prestador(IdP,_,_,I),
      utente(IdU,_,_,_)
    ),
   S
 ).

cuidados_saude_p(P,S):-
  solucoes(
    (Data,Descricao,Custo),
    (
      cuidado(Data,IdU,IdP,Descricao,Custo),
      prestador(IdP,P,_,_),
      utente(IdU,_,_,_)
    ),
   S
 ).
% ------------------------------------------------------
%  Determinar todas as instituições/prestadores a que um utente já recorreu:
%

% ------------------------------------------------------
%  TANIA Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas:
%
