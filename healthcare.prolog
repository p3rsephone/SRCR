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

% Elemento, Lista -> {V,F}
pertence(H,[H|_T]).
pertence(X,[H|_T]) :-
	X \= H,
pertence(X,_T).

% Predicado -> {V,F}
nao(_Q) :- _Q,!,false.
nao(_Q).

% Lista1, Lista2 -> {V,F}
unicos([],[]).
unicos([H|T], R) :-
	pertence(H,T),
	unicos(T,R).
unicos([H|T], [H|R]) :-
	nao(pertence(H,T)),
	unicos(T,R).

% Lista, Comprimento -> {V,F}
comprimento([_H],1).
comprimento([_H|T],N) :- comprimento(T,M), N is M+1.

% Termo, Predicado, Lista -> {V,F}
solucoes(T,Q,S) :- findall(T,Q,S).

% Lista, Valor -> {V,F}
soma([],0).
soma([N|Ns], T) :- soma(Ns,X), T is X+N.
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

+prestador(Id,Nome,Especialidade,Local) :: (solucoes( (Id,Nome,Especialidade,Local), (prestador(Id,Nome,Especialidade,Local)),S ),
                                            comprimento( S,N ),
				                            N==1).

+cuidado(Data,IdUt,IdPrest,Descricao,Custo) :: (solucoes( (Data,IdUt,IdPrest,Descricao,Custo), (cuidado(Data,IdUt,IdPrest,Descricao,Custo)),S ),
                                                comprimento( S,N ),
				                                N==1).


% Invariante Referencial : não permitir a inserção de utentes/prestadores com o mesmo ID
+utente(Id,_,_,_) :: (solucoes( (Id,Nome,Idade,Local), (utente(Id,Nome,Idade,Local)),S ),
                                 comprimento( S,N ),
				                 N==1).

+prestador(Id,_,_,_) :: (solucoes( (Id,Nome,Especialidade,Local), (prestador(Id,Nome,Especialidade,Local)),S ),
                                         comprimento( S,N ),
   			                             N==1).


% Invariante Referencial:  não permitir a remoção de utentes/prestadores
%                          que têm um cuidado associado
-utente(IdUt,_,_,_) :: (solucoes( (IdUt),(cuidado(_,IdUt,_,_,_)),S ),
                                  comprimento( S,N ),
				                  N==0).

-prestador(IdPrest,Nome,Especialidade,Local) :: (solucoes( (IdPrest,Nome,Especialidade,Local), cuidado(_,_,IdPrest,_,_),S ),
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

identificar_utente_Id(Id,S):-
  solucoes(
          (Id,Nome,Idade,Local),
          (utente(Id,Nome,Idade,Local)),
          S
          ).


identificar_utente_Nome(Nome,S):-
  solucoes(
          (Id,Nome,Idade,Local),
          (utente(Id,Nome,Idade,Local)),
          S
          ).

identificar_utente_Idade(Idade,S):-
  solucoes(
          (Id,Nome,Idade,Local),
          (utente(Id,Nome,Idade,Local)),
          S
          ).

identificar_utente_Local(Local,S):-
  solucoes(
          (Id,Nome,Idade,Local),
          (utente(Id,Nome,Idade,Local)),
          S
          ).

identificar_utente_Nome_Idade_Local(Nome,Idade,Local,S):-
  solucoes(
          (Id,Nome,Idade,Local),
          (utente(Id,Nome,Idade,Local)),
          S
          ).
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
%  Identificar as instituições: -Sérgio
%

%Lista de todas as instituições no sistema
instituicoes(S):-
    solucoes(I, prestador(A,B,C,I), L),
    unicos(L,S).

%ID do prestador, lista das instituições desse prestador    
instituicoes_ID_P(ID, S):-
    solucoes(I, prestador(ID,B,C,I), L),
    unicos(L,S).

%Nome do prestador, lista das instituições que os prestadores com esse nome já visitaram   
instituicoes_Nome_P(N, S):-
    solucoes(I, prestador(A,N,C,I), L),
    unicos(L,S).

%Área do prestador, lista das instituições que os utentes dessa área já visitaram 
instituicoes_Area_P(A, S):-
    solucoes(I, prestador(B,C,A,I), L),
    unicos(L,S).
    
%ID do utente, lista das instituições que esse utente já visitou
instituicoes_ID_U(U,S):-
    solucoes(I,(cuidado(D,U,P,O,M),prestador(P,N,A,I)),L),
    unicos(L,S).

%Nome do utente, lista das instituições já visitadas pelos utentes com esse nome  
instituicoes_Nome_U(U,S):-
    solucoes(I,(utente(ID,U,IDA,LO),cuidado(D,ID,P,O,M),prestador(P,N,A,I)),L),
    unicos(L,S).

%Idade do utente, lista das instituições que os utentes com essa idade já visitaram
instituicoes_Idade_U(U,S):-
    solucoes(I,(utente(ID,NO,U,LO),cuidado(D,ID,P,O,M),prestador(P,N,A,I)),L),
    unicos(L,S).

%Local do utente, lista das instituições que os utentes desse local já visitaram
instituicoes_Local_U(U,S):-
    solucoes(I,(utente(ID,NO,IDA,U),cuidado(D,ID,P,O,M),prestador(P,N,A,I)),L),
    unicos(L,S).

% ------------------------------------------------------
%  Identificar os cuidados de saúde prestados por instituição/cidade/datas:
%
cuidados_por_instituicao(Instituicao,S) :-
    solucoes(
        (Data, Descricao, Custo),
        (
            prestador(IdP,_,_,Instituicao),
            cuidado(Data,_,IdP,Descricao,Custo)
        ),
        S
    ).

cuidados_por_cidade(Cidade,S) :-
    solucoes(
        (Data, Descricao,Custo),
        (
            utente(IdU,_,_,Cidade),
            cuidado(Data,IdU,_,Descricao,Custo)
        ),
        S
    ).

cuidados_por_data(Data,S) :-
    solucoes(
        (Data, Descricao,Custo),
        (cuidado(Data,_,_,Descricao,Custo)),
        S).

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
%  Determinar todas as instituições/prestadores a que um utente já recorreu: -Sérgio
%
%ID do utente, Lista com os pares (instuição, nome do prestador) que esse utente já visitou
instituicoes_prestadores(U,S):-
    solucoes((I,NO),(cuidado(D,U,P,O,M),prestador(P,NO,A,I)),L),
    unicos(L,S).

% ------------------------------------------------------
% Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas:
%
custo_por_utente(IdU,C) :-
    solucoes(
        Custo,
        (
            cuidado(_,IdU,_,_,Custo)
        )
        S),
    soma(S,C).

custo_por_prestador(IdP,C) :-
    solucoes(
        Custo,
        (
            cuidado(_,_,IdP,_,Custo)
        )
        S),
    soma(S,C).

custo_por_datas(Data,C) :-
    solucoes(
        Custo,
        (
            cuidado(Data,_,_,_,Custo)
        )
        S),
    soma(S,C).

custo_por_especialidade(Especialidade,C) :-
    solucoes(
        Custo,
        (
            prestador(IdP,_,Especialidade,_),
            cuidado(_,_,IdP,_,Custo)
        )
        S),
    soma(S,C).
