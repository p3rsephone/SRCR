% -------------------------------------------------------
% ---------------------- Factos -------------------------
% -------------------------------------------------------
perfeito(1,p).
perfeito(2,p).
perfeito(3,p).
perfeito(4,p).
perfeito(5,p).
perfeito(6,p).
perfeito(7,p).
perfeito(8,p).
perfeito(9,p).
perfeito(10,p).
perfeito(11,p).
perfeito(12,p).
perfeito(1,u).
perfeito(2,u).
perfeito(3,u).
perfeito(4,u).
perfeito(5,u).
perfeito(6,u).
perfeito(7,u).
perfeito(8,u).
perfeito(9,u).
perfeito(10,u).
perfeito(11,u).
perfeito(12,u).
perfeito(13,u).
perfeito(14,u).
perfeito(15,u).
perfeito(16,u).

interdito(55,p).
incerto(54,p).
impreciso(50,p).
impreciso(100,u).

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
prestador(55, dr_maria_neves, noEspecialidade, trofa_saude).
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





% ------------------------------------------------------
% -------------- Conhecimento Impreciso ----------------
% ------------------------------------------------------
% Há um utente de id 100 com 22 anos da guarda que ou se chama maria ou joana
excecao(utente(100,maria,22,guarda)).
excecao(utente(100,joana,22,guarda)).

excecao(prestador(50,dr_paula_barros,cardiologia,sao_joao)).
excecao(prestador(50,dr_paula_barros,infectologia,sao_joao)).


%TODO: Fazer para os outros.

excecao(cuidado(noData,13,9,sopro_no_coracao,15,81)).
excecao(cuidado(2003-09-17,7,7,otite,30,noRating)).
excecao(cuidado(2003-09-17,15,2,constipacao,noCusto,70)).
excecao(cuidado(2016-06-04,14,1,eczema,45,100)).
excecao(cuidado(2017-12-30,16,3,osso_partido,150,65)).
excecao(cuidado(2008-03-22,5,5,catarata,30,30)).
excecao(cuidado(2015-05-03,6,10,menopausa,45,42)).
excecao(cuidado(2017-12-30,9,6,pele_irritada,150,70)).
