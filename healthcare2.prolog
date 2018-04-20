%
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


:- dynamic '-'/1.
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/6.

% -------------------------------------------------------
% --------------- Predicados auxiliares -----------------
% -------------------------------------------------------

% Extensao do meta-predicado demo :: Expressão, Resposta -> {V,F}
demo( P <=> X, V ) :- demo( P, V1 ), demo( X, V2 ), equivalencia( V1, V2, V ), !.
demo( P => X, V ) :- demo( P, V1 ), demo( X, V2 ), implicacao( V1, V2, V ), !.
demo( P $$ X, V ) :- demo( P, V1 ), demo( X, V2 ), disjuncao( V1, V2, V ), !.
demo( P && X, V ) :- demo( P, V1 ), demo( X, V2 ), conjuncao( V1, V2, V ), !.

equivalencia( X, X, verdadeiro ) :- X \= desconhecido.
equivalencia( desconhecido, Y, desconhecido ).
equivalencia( X, desconhecido, desconhecido ).
equivalencia( verdadeiro, falso, falso ).
equivalencia( falso, verdadeiro, falso ).

implicacao( falso, X, verdadeiro ).
implicacao( X, verdadeiro, verdadeiro ).
implicacao( verdadeiro, desconhecido, desconhecido ).
implicacao( desconhecido, X, desconhecido ) :- X \= verdadeiro.
implicacao( verdadeiro, falso, falso ).

disjuncao( verdadeiro, X, verdadeiro ).
disjuncao( X, verdadeiro, verdadeiro ).
disjuncao( desconhecido, Y, desconhecido ) :- Y \= verdadeiro.
disjuncao( Y, desconhecido, desconhecido ) :- Y \= verdadeiro.
disjuncao( falso, falso, falso ).

conjuncao( verdadeiro, verdadeiro, verdadeiro ).
conjuncao( falso, _, falso ).
conjuncao( _, falso, falso ).
conjuncao( desconhecido, verdadeiro, desconhecido ).
conjuncao( verdadeiro, desconhecido, desconhecido ).

% Predicado demo:
% Questao, Resposta -> {V,F}
demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

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
% -------------------------------------------------------
% -------------- Conhecimento Imperfeito ----------------
% ---------------------- Utente -------------------------
utente(noID, marta_silva, 39, faro).
%TODO: Add more


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
% -------------------------------------------------------
% -------------- Conhecimento Imperfeito ----------------
% --------------------- Prestador -----------------------
prestador(noID, dr_carlos_faria, ginecologia, cufe).
prestador(55, dr_maria_neves, noEspecialidade, trofa_saude).
prestador(yyyy, dr_tiago_maltes, dermatologia, santa_maria).
prestador(54, dr_ivo_moreira,medicina_geral, yyyy).

%TODO: Add more

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
% -------------- Conhecimento Imperfeito ----------------
% --------------------- Cuidado -------------------------
cuidado(noData,13,9,sopro_no_coracao,15,81).
cuidado(2003-09-17,7,7,otite,30,noRating).
cuidado(2003-09-17,15,2,constipacao,noCusto,70).

cuidado(2016-06-04,14,1,eczema,45,100).
cuidado(2017-12-30,16,3,osso_partido,150,65).
cuidado(2008-03-22,5,5,catarata,30,30).
cuidado(2015-05-03,6,10,menopausa,45,42).
cuidado(2017-12-30,9,6,pele_irritada,150,70).
% -------------------------------------------------------
%  Registar utentes, prestadores e cuidados de saúde:
% -------------------------------------------------------

% evolucao: F, Type -> {V,F}
evolucao( F, positivo) :-
    solucoes(I, +F::I, Li),
    insercao(F),
    testa(Li).

evolucao( F, negativo ) :-
    solucoes( I, +(-F)::I, Li ),
    insercao(-F),
    testa(Li).

evolucao( [OPT1 | R], impreciso ) :-
    solucoes( Inv, +OPT1::Inv, L ),
    insercao(excecao(OPT1)),
    testa(L),
    evolucao( R,impreciso ).

evolucao( [], impreciso ).

evolucao( prestador( Id,Nome,Especialidade,Local ), incerto, local) :-
        evolucao( prestador( Id,Nome,Especialidade,yyyy ), positivo ).

evolucao( prestador( Id,Nome,Especialidade,Local ), incerto, nome) :-
        evolucao( prestador( Id,yyyy,Especialidade,Local ), positivo ).

evolucao( prestador( Id,Nome,Especialidade,Local ), incerto, especialidade) :-
        evolucao( prestador( Id,Nome,yyyy,Local ), positivo ).

evolucao( prestador( Id,Nome,Especialidade,Local ), incerto, id) :-
        evolucao( prestador( yyyy,Nome,Especialidade,Local ), positivo ).

evolucao( prestador( Id,Nome,Especialidade,Local ), interdito, local) :-
        evolucao( prestador( Id,Nome,Especialidade,noLocal ), positivo ).

evolucao( prestador( Id,Nome,Especialidade,Local ), interdito, nome) :-
        evolucao( prestador( Id,noName,Especialidade,Local ), positivo ).

evolucao( prestador( Id,Nome,Especialidade,Local ), interdito, especialidade) :-
        evolucao( prestador( Id,Nome,noEspecialidade,Local ), positivo ).

evolucao( prestador( Id,Nome,Especialidade,Local ), interdito, id) :-
        evolucao( prestador( noId,Nome,Especialidade,Local ), positivo ).

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

% Garantir que não existe conhecimento positivo contraditótio
% Não repetir conhecimento negativo
+(-prestador( Id,Nome,Especialidade,Local )) :: ( solucoes( (Id), prestador(Id,Nome,Especialidade,Local), S ),
                            comprimento( S, N ),
                            N == 0 ).

+(-prestador( Id,Nome,Especialidade,Local )) :: ( solucoes( (Id), -prestador(Id,Nome,Especialidade,Local), S ),
                            comprimento( S, N ),
                            N == 1 ).
% Invariante que impede a inserção de conhecimento negativo acerca de conhecimento interdito sobre a especialidade de utentes
+(-prestador( Id,Nome,Especialidade,Local )) :: (solucoes( ( Id,Nome,Especialidade,Local ), (prestador( Id,Nome,Especialidade,Local ), nulo(Especialidade)), S ),
                              comprimento( S,N ),
                              N == 0).

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

involucao( prestador( Id,Nome,Especialidade,Local ), incerto, local) :-
        involucao( prestador( Id,Nome,Especialidade,yyyy ), positivo ).

involucao( prestador( Id,Nome,Especialidade,Local ), incerto, nome) :-
        involucao( prestador( Id,yyyy,Especialidade,Local ), positivo ).

involucao( prestador( Id,Nome,Especialidade,Local ), incerto, especialidade) :-
        involucao( prestador( Id,Nome,yyyy,Local ), positivo ).

involucao( prestador( Id,Nome,Especialidade,Local ), incerto, id) :-
        involucao( prestador( yyyy,Nome,Especialidade,Local ), positivo ).

involucao( prestador( Id,Nome,Especialidade,Local ), interdito, local) :-
        involucao( prestador( Id,Nome,Especialidade,noLocal ), positivo ).

involucao( prestador( Id,Nome,Especialidade,Local ), interdito, nome) :-
        involucao( prestador( Id,noName,Especialidade,Local ), positivo ).

involucao( prestador( Id,Nome,Especialidade,Local ), interdito, especialidade) :-
        involucao( prestador( Id,Nome,noEspecialidade,Local ), positivo ).

involucao( prestador( Id,Nome,Especialidade,Local ), interdito, id) :-
        involucao( prestador( noId,Nome,Especialidade,Local ), positivo ).

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

excecao(utente(_,Nome,Idade,Local)) :- utente(xxxx,Nome,Idade,Local).
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
excecao(prestador(51,dr_antonio_torres,oftalmologia,cufe)).
excecao(prestador(51,dr_manuel_vieira,oftalmologia,cufe)).
excecao(prestador(52,dr_claudia_carvalho,cirurgia_geral,cufe)).
excecao(prestador(53,dr_claudia_carvalho,cidade,cufe)).

%TODO: Fazer para os outros.

excecao(cuidado(noData,13,9,sopro_no_coracao,15,81)).
excecao(cuidado(2003-09-17,7,7,otite,30,noRating)).
excecao(cuidado(2003-09-17,15,2,constipacao,noCusto,70)).
excecao(cuidado(2016-06-04,14,1,eczema,45,100)).
excecao(cuidado(2017-12-30,16,3,osso_partido,150,65)).
excecao(cuidado(2008-03-22,5,5,catarata,30,30)).
excecao(cuidado(2015-05-03,6,10,menopausa,45,42)).
excecao(cuidado(2017-12-30,9,6,pele_irritada,150,70)).
)% ------------------------------------------------------
% -------------- Conhecimento Interdito ----------------
% ------------------------------------------------------
% Quando não se sabe nem nunca se vai saber algo de um utente
excecao(utente(_,Nome,Idade,Local):- utente(noId,Nome,Idade,Local)).
excecao(utente(Id,_,Idade,Local):- utente(Id,noName,Idade,Local)).
excecao(utente(Id,Nome,_,Local)):- utente(Id,Nome,noIdade,Local).
excecao(utente(Id,Nome,Idade,_)):- utente(Id,Nome,Idade,noLocal).

excecao(prestador(_,Nome,Especialidade,Local):- prestador(noId,Nome,Especialidade,Local)).
excecao(prestador(Id,_,Especialidade,Local):- prestador(Id,noName,Especialidade,Local)).
excecao(prestador(Id,Nome,_,Local)):- prestador(Id,Nome,noEspecialidade,Local).
excecao(prestador(Id,Nome,Especialidade,_)):- prestador(Id,Nome,Especialidade,noLocal).

nulo(noId).
nulo(noName).
nulo(noIdade).
nulo(noLocal).
nulo(noEspecialidade).
nulo(noDescricao).
nulo(noCusto).
nulo(noRating).
nulo(noData).

+prestador(Id,Name,Especialidade, Local) :- (solucoes(Id,(prestador(Id, dr_carlos_faria,ginecologia,cufe), nao(nulo(Id))), L),
                                            comprimento(L, N),
                                            N==0).

+prestador(Id,Name,Especialidade, Local) :- (solucoes(Especialidade,(prestador(55, dr_maria_neves,Especialidade,trofa_saude), nao(nulo(Especialidade))), L),
                                            comprimento(L, N),
                                            N==0).

%:- utente(Id, marta_silva, 39, faro), nao(nulo(Id)).
%TODO: Fazer para os outros.
