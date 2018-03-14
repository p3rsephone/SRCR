%
% utente : #IdUt, Nome, Idade, Morada --> {V,F}
% prestador : #IdPrest, Nome, Especialidade, Instituição --> {V,F}
% cuidado : Data, #idUt, #IdPrest, Descrição, Custo --> {V,F}
% -------------------------------------------------------
% Definições iniciais
:- op(900,xfy,'::').
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/6.

% -------------------------------------------------------
% --------------- Predicados auxiliares -----------------
% -------------------------------------------------------

% Predicado pertence:
% Elemento, Lista -> {V,F}
pertence(H,[H|_T]).
pertence(X,[H|_T]) :-
	X \= H,
pertence(X,_T).

% Predicado de negação nao:
% Predicado -> {V,F}
nao(_Q) :- _Q,!,false.
nao(_Q).

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

%Predicado contar_occ:
%elemento, lista, quantas vezes aparece o elemento na lista
contar_occ(E, L, N) :-
        adicionar(E, L, L2), comprimento(L2, N).

%Predicado adicionar
%elemento, lista, lista com apenas os "elemento"
adicionar(_E,[],[]).

adicionar(E,[H|T],[H|L1]):-
    adicionar(E,T,L1),
    isEqual(E,H).

adicionar(E,[H|T],L1):-
        adicionar(E,T,L1),
        isNotEqual(E,H).

%Predicado maior_dupl
%lista com duplos, algum elemento, as vezes que aparece nos cuidados
maior_dupl([],0,0).

maior_dupl([(P,N)|T],P,N):-
    maior_dupl(T,_,C),
    N >= C.

maior_dupl([(_,N)|T],R,C):-
        maior_dupl(T,R,C),
        C > N.

% -------------------------------------------------------
% ---------------------- Factos -------------------------
% -------------------------------------------------------

utente(1, mario_silva, 18, faro).
utente(2, raquel_guimaraes, 37, lisboa).
utente(3, joaquim_canas, 25, braga).
utente(4, vania_costa, 67, viseu).
utente(5, sergio_costa, 21, coimbra).
utente(6, tania_silva, 52, viana_do_castelo).
utente(7, alberto_barros, 11, braganca).
utente(8, jose_resende, 98, beja).
utente(9, maria_santos, 31, aveiro).
utente(10, antonio_oliveira, 37, braga).
utente(11, daniel_marques, 76, lisboa).
utente(12, francisco_ales, 56, leiria).
utente(13, paulo_antunes, 34, lisboa).
utente(14, rita_moreira, 22, braga).
utente(15, sara_paredes, 25, porto).
utente(16, pedro_afonso, 67, porto).

prestador(1, dr_joao_lopes, dermatologia, sao_joao).
prestador(2, dr_marisa_silva, medicina_geral, santo_antonio).
prestador(3, dr_bruno_ferreira, cardiologia, santa_maria).
prestador(4, dr_fernanda_moreira, cirurgia_geral, cufe).
prestador(5, dr_larissa_dias, oftalmologia, trofa_saude).
prestador(6, dr_paulo_gomes, dermatologia, santo_antonio).
prestador(7, dr_guilherme_melo, otorrinolaringologia, cufe).
prestador(8, dr_rafaela_martins, infectologia, sao_joao).
prestador(9, dr_igor_pereira, cardiologia, santa_maria).
prestador(10, dr_marina_almeida, ginecologia, santo_antonio).
prestador(11, dr_andre_carvalho, oftalmologia, sao_joao).
prestador(12, dr_yasmin_barbosa, cirurgia_geral, cufe).

cuidado(2008-03-22,1,2,constipacao,15,60).
cuidado(2015-05-03,2,3,arritmia,45,80).
cuidado(2003-09-17,3,1,eczema,30,91).
cuidado(2017-12-30,4,4,hernia,150,84).
cuidado(2008-03-22,5,5,catarata,15,30).
cuidado(2015-05-03,6,10,menopausa,45,42).
cuidado(2003-09-17,7,7,otite,30,75).
cuidado(2017-12-30,8,6,pele_irritada,150,70).
cuidado(2008-03-22,9,8,fungos,15,60).
cuidado(2015-05-03,10,2,constipacao,45,95).
cuidado(2003-09-17,11,12,quisto,30,50).
cuidado(2017-12-30,12,11,catarata,150,90).
cuidado(2008-03-22,13,9,sopro_no_coracao,15,81).
cuidado(2015-05-03,14,1,eczema,45,100).
cuidado(2003-09-17,15,2,constipacao,30,70).
cuidado(2017-12-30,16,3,arritmia,150,65).

% -------------------------------------------------------
%  Registar utentes, prestadores e cuidados de saúde:
% -------------------------------------------------------
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
% -------------------------------------------------------
+utente(Id,Nome,Idade,Local) :: (solucoes( (Id,Nome,Idade,Local), (utente(Id,Nome,Idade,Local)),S ),
                                 comprimento( S,N ),
				                 N==1).

+prestador(Id,Nome,Especialidade,Local) :: (solucoes( (Id,Nome,Especialidade,Local), (prestador(Id,Nome,Especialidade,Local)),S ),
                                            comprimento( S,N ),
				                            N==1).

+cuidado(Data,IdUt,IdPrest,Descricao,Custo,Rating) :: (solucoes( (Data,IdUt,IdPrest,Descricao,Custo), (cuidado(Data,IdUt,IdPrest,Descricao,Custo,Rating)),S ),
                                                comprimento( S,N ),
				                                N==1).

% -------------------------------------------------------
% Invariante Referencial : não permitir a inserção de utentes/prestadores
%                          com o mesmo ID
% -------------------------------------------------------
+utente(Id,_,_,_) :: (solucoes( (Id,Nome,Idade,Local), (utente(Id,Nome,Idade,Local)),S ),
                                 comprimento( S,N ),
				                 N==1).

+prestador(Id,_,_,_) :: (solucoes( (Id,Nome,Especialidade,Local), (prestador(Id,Nome,Especialidade,Local)),S ),
                                         comprimento( S,N ),
   			                             N==1).

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
remove(T) :- solucoes(I,-T::I,S),
              apagar(T),
              testa(S).

apagar(T) :- retract(T).
apagar(T) :- assert(T),!,fail.
% ------------------------------------------------------
%  Identificar utentes (Critérios de seleção):
% ------------------------------------------------------
% Predicado identificar_utente_por_Id:
% Id, Lista de Utentes -> {V,F}
identificar_utente_por_Id(Id,S):-
  solucoes(
          (Id,Nome,Idade,Local),
          (utente(Id,Nome,Idade,Local)),
          S
          ).

% Predicado identificar_utente_por_Nome:
% Nome, Lista de Utentes -> {V,F}
identificar_utente_por_Nome(Nome,S):-
  solucoes(
          (Id,Nome,Idade,Local),
          (utente(Id,Nome,Idade,Local)),
          S
          ).

% Predicado identificar_utente_por_Idade:
% Idade, Lista de Utentes -> {V,F}
identificar_utente_por_Idade(Idade,S):-
  solucoes(
          (Id,Nome,Idade,Local),
          (utente(Id,Nome,Idade,Local)),
          S
          ).

% Predicado identificar_utente_por_Local:
% Local, Lista de Utentes -> {V,F}
identificar_utente_por_Local(Local,S):-
  solucoes(
          (Id,Nome,Idade,Local),
          (utente(Id,Nome,Idade,Local)),
          S
          ).

% Predicado identificar_utente_por_Nome_Idade_Local:
% Nome,Idade,Local,Lista de Utentes -> {V,F}
identificar_utente_por_Nome_Idade_Local(Nome,Idade,Local,S):-
  solucoes(
          (Id,Nome,Idade,Local),
          (utente(Id,Nome,Idade,Local)),
          S
          ).

% ------------------------------------------------------
%  Identificar utentes de um prestador/especialidade/instituição:
% ------------------------------------------------------
% Predicado utentes_por_prestador_especialidade_instituicao:
% Prestador, Especialidade, Instituicao, Lista de Utentes -> {V,F}
utentes_por_prestador_especialidade_instituicao(P,E,I,S):-
  solucoes(
    (IdU,Nome,Idade,Morada),
    (
      cuidado(_,IdU,IdP,_,_,_),
      prestador(IdP,P,E,I),
      utente(IdU,Nome,Idade,Morada)
    ),
   L
  ),
  unicos(L,S).

% Predicado utentes_por_prestador:
% Prestador, Lista de Utentes -> {V,F}
utentes_por_prestador(P,S):-
  solucoes(
    (IdU,Nome,Idade,Morada),
    (
      cuidado(_,IdU,IdP,_,_,_),
      prestador(IdP,P,_,_),
      utente(IdU,Nome,Idade,Morada)
    ),
   L
  ),
  unicos(L,S).

% Predicado utentes_por_especialidade:
% Especialidade, Lista de Utentes -> {V,F}
utentes_por_especialidade(E,S):-
  solucoes(
    (IdU,Nome,Idade,Morada),
    (
      cuidado(_,IdU,IdP,_,_,_),
      prestador(IdP,_,E,_),
      utente(IdU,Nome,Idade,Morada)
    ),
   L
  ),
  unicos(L,S).

% Predicado utentes_por_instituicao:
% Instituicao, Lista de Utentes -> {V,F}
utentes_por_instituicao(I,S):-
  solucoes(
    (IdU,Nome,Idade,Morada),
    (
      cuidado(_,IdU,IdP,_,_,_),
      prestador(IdP,_,_,I),
      utente(IdU,Nome,Idade,Morada)
    ),
   L
  ),
  unicos(L,S).

% ------------------------------------------------------
%  Identificar as instituições:
% ------------------------------------------------------
% Predicado instituicoes:
%Lista de todas as instituições no sistema -> {V,F}
instituicoes(S):-
    solucoes(I, prestador(_,_,_,I), L),
    unicos(L,S).

% Predicado instituicoes_por_IdPrestador:
%ID do prestador, lista das instituições desse prestador -> {V,F}
instituicoes_por_IdPrestador(ID, S):-
    solucoes(I, prestador(ID,_,_,I), L),
    unicos(L,S).

% Predicado instituicoes_por_NomePrestador:
%Nome do prestador, lista das instituições que os prestadores com esse nome já visitaram -> {V,F}
instituicoes_por_NomePrestador(N, S):-
    solucoes(I, prestador(_,N,_,I), L),
    unicos(L,S).

% Predicado instituicoes_por_Especialidade:
%Especialidade do prestador, lista das instituições que os utentes dessa área já visitaram -> {V,F}
instituicoes_por_Especialidade(E, S):-
    solucoes(I, prestador(_,_,E,I), L),
    unicos(L,S).

% Predicado utentes_por_IdUtente:
%ID do utente, lista das instituições que esse utente já visitou -> {V,F}
instituicoes_por_IdUtente(U,S):-
    solucoes(I,(cuidado(_,U,P,_,_,_,_),prestador(P,_,_,I)),L),
    unicos(L,S).

% Predicado instituicoes_por_NomeUtente:
%Nome do utente, lista das instituições já visitadas pelos utentes com esse nome -> {V,F}
instituicoes_por_NomeUtente(U,S):-
    solucoes(I,(utente(ID,U,_,_),cuidado(_,ID,P,_,_,_,_),prestador(P,_,_,I)),L),
    unicos(L,S).

% Predicado instituicoes_por_Idade:
%Idade do utente, lista das instituições que os utentes com essa idade já visitaram -> {V,F}
instituicoes_por_Idade(U,S):-
    solucoes(I,(utente(ID,_,U,_),cuidado(_,ID,P,_,_,_,_),prestador(P,_,_,I)),L),
    unicos(L,S).

% Predicado utentes_por_instituicao:
%Local do utente, lista das instituições que os utentes desse local já visitaram -> {V,F}
instituicoes_Local(U,S):-
    solucoes(I,(utente(ID,_,_,U),cuidado(_,ID,P,_,_,_,_),prestador(P,_,_,I)),L),
    unicos(L,S).

% ------------------------------------------------------
%  Identificar os cuidados de saúde prestados por instituição/cidade/datas:
% ------------------------------------------------------
% Predicado cuidados_por_instituicao:
% Instituicao,Lista dos cuidados -> {V,F}
cuidados_por_instituicao(Instituicao,S) :-
    solucoes(
        (Data, Descricao, Custo),
        (
            prestador(IdP,_,_,Instituicao),
            cuidado(Data,_,IdP,Descricao,Custo,_)
        ),
        S
    ).

% Predicado cuidados_por_cidade:
% Cidade,Lista dos cuidados -> {V,F}
cuidados_por_cidade(Cidade,S) :-
    solucoes(
        (Data, Descricao,Custo),
        (
            utente(IdU,_,_,Cidade),
            cuidado(Data,IdU,_,Descricao,Custo,_)
        ),
        S
    ).

% Predicado cuidados_por_data:
% Data,Lista dos cuidados -> {V,F}
cuidados_por_data(Data,S) :-
    solucoes(
        (Data, Descricao,Custo),
        (cuidado(Data,_,_,Descricao,Custo,_)),
        S).

% ------------------------------------------------------
%  Identificar cuidados de saúde realizados por utente/instituição/prestador
% ------------------------------------------------------
% Predicado cuidados_por_utente_instituicao_prestador:
% Utente,Instituicao,Prestador,Lista dos cuidados -> {V,F}
cuidados_por_utente_instituicao_prestador(U,I,P,S):-
  solucoes(
    (Data,Descricao,Custo),
    (
      cuidado(Data,IdU,IdP,Descricao,Custo,_),
      prestador(IdP,P,_,I),
      utente(IdU,U,_,_)
    ),
   S
 ).

% Predicado cuidados_por_utente:
% Utente,Lista dos cuidados -> {V,F}
cuidados_por_utente(U,S):-
  solucoes(
    (Data,Descricao,Custo),
    (
      cuidado(Data,IdU,IdP,Descricao,Custo,_),
      prestador(IdP,_,_,_),
      utente(IdU,U,_,_)
    ),
   S
 ).

% Predicado cuidados_por_prestador:
% Predicado,Lista dos cuidados -> {V,F}
cuidados_por_prestador(P,S):-
  solucoes(
    (Data,Descricao,Custo),
    (
      cuidado(Data,IdU,IdP,Descricao,Custo,_),
      prestador(IdP,P,_,_),
      utente(IdU,_,_,_)
    ),
   S
 ).

% ------------------------------------------------------
%  Determinar todas as instituições/prestadores a que um utente já recorreu:
% ------------------------------------------------------
% Predicado todas_instituicoes_prestadores_por_utente:
% Id do utente, Lista com os pares (instuição, nome do prestador) que esse utente já visitou
todas_instituicoes_prestadores_por_utente(U,S):-
    solucoes((I,NO),(cuidado(_,U,P,_,_,_),prestador(P,NO,_,I)),L),
    unicos(L,S).

% ------------------------------------------------------
% Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas:
% ------------------------------------------------------
% Predicado custo_por_utente:
% Id do Utente, Custo total -> {V,F}
custo_por_utente(IdU,C) :-
    solucoes(
        Custo,
        cuidado(_,IdU,_,_,Custo,_),
        S),
    soma(S,C).

% Predicado custo_por_prestador:
%Id do Prestador, Custo total -> {V,F}
custo_por_prestador(IdP,C) :-
    solucoes(
        Custo,
        cuidado(_,_,IdP,_,Custo,_),
        S),
    soma(S,C).

% Predicado custo_por_datas:
%Data, Custo total -> {V,F}
custo_por_datas(Data,C) :-
    solucoes(
        Custo,
        cuidado(Data,_,_,_,Custo,_),
        S),
    soma(S,C).

% Predicado custo_por_especialidade:
%Especialidade, Custo total -> {V,F}
custo_por_especialidade(Especialidade,C) :-
    solucoes(
        Custo,
        (
            prestador(IdP,_,Especialidade,_),
            cuidado(_,_,IdP,_,Custo,_)
        ),
        S),
    soma(S,C).

% ------------------------------------------------------
% ---------------------- Extras ------------------------
% ------------------------------------------------------

% ------------------------------------------------------
% Calcular a idade média dos utentes por prestador/instituicao/especialidade:
% ------------------------------------------------------
% Predicado idade_media_por_prestador:
% Id do Utente, Idade média -> {V,F}
idade_media_por_prestador(IdP,IdadeMedia) :-
    solucoes(
        Idade,
        (
            utente(IdU,_,Idade,_),
            prestador(IdP,_,_,_),
            cuidado(_,IdU,IdP,_,_,_)
        ),
        S),
    media(S,IdadeMedia).

% Predicado idade_media_por_instituicao:
% Instituicao, Idade média -> {V,F}
idade_media_por_instituicao(I,IdadeMedia) :-
    solucoes(
        Idade,
        (
            utente(IdU,_,Idade,_),
            prestador(IdP,_,_,I),
            cuidado(_,IdU,IdP,_,_,_)
        ),
        S),
    media(S,IdadeMedia).

% Predicado idade_media_por_especialidade:
% Especialidade, Idade média -> {V,F}
idade_media_por_especialidade(E,IdadeMedia) :-
    solucoes(
        Idade,
        (
            utente(IdU,_,Idade,_),
            prestador(IdP,_,E,_),
            cuidado(_,IdU,IdP,_,_,_)
        ),
        S),
    media(S,IdadeMedia).

% ------------------------------------------------------
% Determinar todos os utentes com a mesma descricao:
% ------------------------------------------------------
% Predicado custo_por_utente:
% Id do Utente, Custo total -> {V,F}
utentes_por_descricao(D,S) :-
    solucoes(
        (IdU,Nome,Idade,Local),
        (
            utente(IdU,Nome,Idade,Local),
            cuidado(_,IdU,_,D,_,_)
        ),
        S).
% ------------------------------------------------------
% Calcular o custo médio dos cuidados por prestador/instituicao/especialidade:
% ------------------------------------------------------
% Predicado custo_medio_por_prestador:
% Id do Prestador, Custo médio -> {V,F}
custo_medio_por_prestador(IdP,CustoMedio) :-
    solucoes(
        Custo,
        (
            prestador(IdP,_,_,_),
            cuidado(_,_,IdP,_,Custo,_)
        ),
        S),
    media(S,CustoMedio).

% Predicado custo_medio_por_instituicao:
% Instituicao, Custo médio -> {V,F}
custo_medio_por_instituicao(I,CustoMedio) :-
    solucoes(
        Custo,
        (
            prestador(IdP,_,_,I),
            cuidado(_,_,IdP,_,Custo,_)
        ),
        S),
    media(S,CustoMedio).

% Predicado custo_medio_por_especialidade:
% Especialidade, Custo médio -> {V,F}
custo_medio_por_especialidade(E,CustoMedio) :-
    solucoes(
        Custo,
        (
            prestador(IdP,_,E,_),
            cuidado(_,_,IdP,_,Custo,_)
        ),
        S),
    media(S,CustoMedio).

% ------------------------------------------------------
% Verificar o serviço mais prestado por um prestador:
% ------------------------------------------------------
%Predicado media_prestador:
%ID do prestador, media do serviço por ele prestado

media_prestador(ID,M):-
    solucoes(R,(cuidado(_,_,ID,_,_,R)),S),
    media(S,M).

% ------------------------------------------------------
% Verificar o melhor prestador/instituicao e a quantidade de vezes que prestou servicos:
% ------------------------------------------------------
%Predicado melhor_prestador:
%ID do melhor prestador, quantidade de vezes que ele prestou serviços

melhor_prestador(ID,Q):-
        solucoes(P,cuidado(_,_,P,_,_,_),L),
        solucoes((Pr,N),(prestador(Pr,_,_,_),contar_occ(Pr,L,N)),S),
        maior_dupl(S,ID,Q).

%Predicado melhor_instuicao
%instituicao com mais cuidados, quantidade de cuidados
melhor_instituicao(I,Q):-
    solucoes(IN,(cuidado(_,_,P,_,_,_), prestador(P,_,_,IN)),L),
    solucoes((INS,N),(prestador(_,_,_,INS),contar_occ(INS,L,N)),S),
    unicos(S,Novo),
    maior_dupl(Novo,I,Q).
