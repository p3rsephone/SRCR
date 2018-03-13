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
utente(5, sergio_costa, 21, coimbra).
utente(6, tania_silva, 52, viana_do_castelo).
utente(7, alberto_barros, 11, bragança).
utente(8, jose_resende, 98, beja).
utente(9, maria_santos, 31, aveiro).
utente(10, antonio_oliveira, 37, braga).
utente(11, daniel_marques, 76, lisboa).
utente(12, francisco_ales, 56, leiria).
utente(13, paulo_antunes, 34, lisboa).
utente(14, rita_moreira, 22, braga).
utente(15, sara_paredes, 25, porto.
utente(16, pedro_afonso, 67, porto).

prestador(1, dr_joao_lopes, dermatologia, sao_joao).
prestador(2, dr_marisa_silva, medicina_geral, santo_antonio).
prestador(3, dr_bruno_ferreira, cardiologia, santa_maria).
prestador(4, dr_fernanda_moreira, cirurgia_geral, cufe).
prestador(5, dr_larissa_dias, oftalmologia, trofa_saude).
prestador(6, dr_paulo_gomes, dermatologia, santo_antonio).
prestador(7, dr_guilherme_melo, otorrinolaringologia, cufe).
prestador(8, dr_rafaela_martins, infectologia, sao_joao).
prestador(9, dr_igor_pereira, cardiologia, sao_maria).
prestador(10, dr_marina_almeida, ginecologia, santo_antonio).
prestador(11, dr_andré_carvalho, oftalmologia, santa_joao).
prestador(12, dr_yasmin_barbosa, cirurgia_geral, cufe).

cuidado(2008-03-22,1,2,constipacao,15).
cuidado(2015-05-03,2,3,arritmia,45).
cuidado(2003-09-17,3,1,eczema,30).
cuidado(2017-12-30,4,4,hernia,150).
cuidado(2008-03-22,5,5,catarata,15).
cuidado(2015-05-03,6,10,menopausa,45).
cuidado(2003-09-17,7,7,otite,30).
cuidado(2017-12-30,8,6,pele_irritada,150).
cuidado(2008-03-22,9,8,fungos,15).
cuidado(2015-05-03,10,2,constipacao,45).
cuidado(2003-09-17,11,12,quisto,30).
cuidado(2017-12-30,12,11,catarata,150).
cuidado(2008-03-22,13,9,sopro_no_coracao,15).
cuidado(2015-05-03,14,1,eczema,45).
cuidado(2003-09-17,15,2,constipacao,30).
cuidado(2017-12-30,16,3,arritmia,150).

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
%  TANIA Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas:
%
