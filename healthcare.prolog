%
% utente : #IdUt, Nome, Idade, Morada --> {V,F}
% prestador : #IdPrest, Nome, Especialidade, Instituição --> {V,F}
% cuidado : Data, #idUt, #IdPrest, Descrição, Custo --> {V,F}
%
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
%  Registar utentes:
%

%  Registar prestadores:
%

%  Registar cuidados de saúde:
%

% ------------------------------------------------------
%  Remover utentes:
%

%  Remover prestadores:
%

%  Remover cuidados de saúde:
%

% ------------------------------------------------------
%  Identificar utentes (Critérios de seleção):
%

%  Identificar utentes de um prestador/especialidade/instituição:
%

% ------------------------------------------------------
%  Identificar as instituições:
%

% ------------------------------------------------------
%  Identificar os cuidados de saúde prestados por instituição/cidade/datas:
%

%  Identificar cuidados de saúde realizados por utente/instituição/prestador
%
% ------------------------------------------------------
%  Determinar todas as instituições/prestadores a que um utente já recorreu:
%

% ------------------------------------------------------
%  Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas:
%
