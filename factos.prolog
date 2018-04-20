:- dynamic positivo/2.
:- dynamic incerto/2.
:- dynamic interdito/2.
:- dynamic impreciso/2.
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/6.
% -------------------------------------------------------
% ---------------------- Factos -------------------------
% -------------------------------------------------------
positivo(1,p).
positivo(2,p).
positivo(3,p).
positivo(4,p).
positivo(5,p).
positivo(6,p).
positivo(7,p).
positivo(8,p).
positivo(9,p).
positivo(10,p).
positivo(11,p).
positivo(12,p).
positivo(1,u).
positivo(2,u).
positivo(3,u).
positivo(4,u).
positivo(5,u).
positivo(6,u).
positivo(7,u).
positivo(8,u).
positivo(9,u).
positivo(10,u).

positivo(11,u).
positivo(12,u).
positivo(13,u).
positivo(14,u).
positivo(15,u).
positivo(16,u).
positivo(1,c).
positivo(2,c).
positivo(3,c).
positivo(4,c).
positivo(5,c).
positivo(6,c).
positivo(7,c).
positivo(8,c).
positivo(9,c).
positivo(10,c).
positivo(11,c).
positivo(12,c).
positivo(13,c).
positivo(14,c).
positivo(15,c).
positivo(16,c).

interdito(55,p).
interdito(51,c).
interdito(52,c).
interdito(53,c).
incerto(54,p).
impreciso(50,p).
impreciso(100,u).
impreciso(14,c).
impreciso(5,c).

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
% -------------- Conhecimento Impositivo ----------------
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
% -------------- Conhecimento Impositivo ----------------
% --------------------- Prestador -----------------------
prestador(55, dr_maria_neves, noEspecialidade, trofa_saude).
prestador(54, dr_ivo_moreira,medicina_geral, yyyy).

%TODO: Add more

cuidado(1,2008-03-22,1,2,constipacao,15,60).
cuidado(2,2015-05-03,2,3,arritmia,45,80).
cuidado(3,2003-09-17,3,1,eczema,30,91).
cuidado(4,2017-12-30,4,4,hernia,150,84).
cuidado(5,2008-03-22,5,5,catarata,15,30).
cuidado(6,2015-05-03,6,10,menopausa,45,42).
cuidado(7,2003-09-17,7,7,otite,30,75).
cuidado(8,2017-12-30,8,6,pele_irritada,150,70).
cuidado(9,2008-03-22,9,8,fungos,15,60).
cuidado(10,2015-05-03,10,2,constipacao,45,95).
cuidado(11,2003-09-17,11,12,quisto,30,50).
cuidado(12,2017-12-30,12,11,catarata,150,90).
cuidado(13,2008-03-22,13,9,sopro_no_coracao,15,81).
cuidado(14,2015-05-03,14,1,eczema,45,100).
cuidado(15,2003-09-17,15,2,constipacao,30,70).
cuidado(16,2017-12-30,16,3,arritmia,150,65).
% -------------------------------------------------------
% -------------- Conhecimento Impositivo ----------------
% --------------------- Cuidado -------------------------
cuidado(51,noData,13,9,sopro_no_coracao,15,81).
cuidado(52,2003-09-17,7,7,otite,30,noRating).
cuidado(53,2003-09-17,15,2,constipacao,noCusto,70).

cuidado(14,2016-06-04,14,1,eczema,45,100).
cuidado(5,2008-03-22,5,5,catarata,30,30).