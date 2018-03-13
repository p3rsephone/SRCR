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
% Funções auxiliares
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

cuidado(22-03-2008,1,2,constipacao,15).
cuidado(03-05-2015,2,3,arritmia,45).
cuidado(17-09-2003,3,1,eczema,30).
cuidado(30-12-2017,4,4,hernia,150).
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
%  Identificar utentes (Critérios de seleção): -Sérgio
%

%  Identificar utentes de um prestador/especialidade/instituição:
%

% ------------------------------------------------------
%  Identificar as instituições: -Sérgio
%
%instituicoes([H|T]):-
%    prestador(A,B,C,H),
%    instituicoes(T).

instituicoes_todas(L):-
    solucoes(I, prestador(A,B,C,I), L).

instituicoes_todas(ID, S):-
        solucoes(I, prestador(ID,B,C,I), L).
% ------------------------------------------------------
%  Identificar os cuidados de saúde prestados por instituição/cidade/datas:
%

%  Identificar cuidados de saúde realizados por utente/instituição/prestador
%

% ------------------------------------------------------
%  Determinar todas as instituições/prestadores a que um utente já recorreu: -Sérgio
%

% ------------------------------------------------------
%  Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas:
%
