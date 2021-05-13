%% Matilde Tocha 99108

:- [codigo_comum].

% -----------------------------------------------------------------------------
%                           combinacoes_soma/4
% Significa que Combs eh a lista ordenada cujos elementos sao as combinacoes 
% N a N, dos elementos de Els cuja soma e Soma.
% -----------------------------------------------------------------------------
combinacoes_soma(N, Els, Soma, Combs) :-
    findall(Comb, 
        (combinacao(N, Els, Comb), foldl(plus, Comb, 0, Soma_Aux), Soma_Aux =:= Soma), 
        Combs).

% -----------------------------------------------------------------------------
%                           permutacoes_soma/4
% Significa que Perms eh a lista ordenada cujos elementos sao as permutacoes
% das combinacoes N a N, dos elementos de Els cuja soma eh Soma.
% -----------------------------------------------------------------------------
permutacoes_soma(N, Els, Soma, Perms) :-
    findall(Perm, (combinacao(N, Els, Comb), permutation(Comb, Perm)), Aux),
    findall(Perm, 
        (member(Perm, Aux), foldl(plus, Perm, 0, Soma_Aux), Soma_Aux =:= Soma), 
        Res),
    sort(Res, Perms).

% -----------------------------------------------------------------------------
%                         pos_listas_Fila/2
% Predicado Auxiliar.
% Significa que Posicoes eh a lista de listas [pos, lista], significando
% que essas listas contem as posicoes dos elementos da Fila que sejam listas.
% -----------------------------------------------------------------------------
pos_listas_Fila(Fila, Posicoes) :-
    length(Fila, N),
    findall([Pos, Candidato], 
        (between(1, N, Pos), nth1(Pos, Fila, Candidato), is_list(Candidato)),
        Posicoes).

% -----------------------------------------------------------------------------
%                         espaco/4
% Predicado Auxiliar.
% Significa que Vars_Esp eh a lista cujos elementos sao as variaveis de um 
% espaco.
% -----------------------------------------------------------------------------
espaco(Fila, Posicoes, Pos_Somas, Vars_Esp) :- 
    espaco(Fila, Posicoes, Pos_Somas, Vars_Esp, [], []).

espaco(_, [], Aux_Somas, Aux, Aux_Somas, Aux).

espaco(Fila, [[_, Lst2]], Pos_Somas, Vars_Esp, Aux_Somas, Aux) :- 
    length(Fila, Max), nth1(Max, Fila, Lst1), Lst1 == Lst2,
    espaco(Fila, [], Pos_Somas, Vars_Esp, Aux_Somas, Aux).

espaco(Fila, [[El, _]], Pos_Somas, Vars_Esp, Aux_Somas, Aux) :- 
    length(Fila, Max), Min is El + 1,
    bagof(Membro, Indice^(between(Min, Max, Indice), nth1(Indice, Fila, Membro)), Vars),
    espaco(Fila, [], Pos_Somas, Vars_Esp, [El | Aux_Somas] ,[Vars | Aux]).

espaco(Fila, [El1, El2 | R], Pos_Somas, Vars_Esp, Aux_Somas, Aux) :-
    nth0(0, El1, Pos1), nth0(0, El2, Pos2),
    Min is Pos1 + 1, Max is Pos2 - 1,
    Min < Max,
    bagof(Membro, Indice^(between(Min, Max, Indice), nth1(Indice, Fila, Membro)), Vars),
    append([El2], R, New_R),
    espaco(Fila, New_R, Pos_Somas, Vars_Esp, [Pos1 | Aux_Somas] ,[Vars | Aux]).

espaco(Fila, [El1, El2 | R], Pos_Somas, Vars_Esp, Aux_Somas, Aux) :-
    nth0(0, El1, Pos1), nth0(0, El2, Pos2),
    Min is Pos1 + 1, Max is Pos2 - 1,
    Min >= Max,
    append([El2], R, New_R),
    espaco(Fila, New_R, Pos_Somas, Vars_Esp, Aux_Somas, Aux).

% -----------------------------------------------------------------------------
%                           somas_H_V/4
% Significa Somas eh uma lista cujos elementos sao as somas da fila, 
% conforme se trate de uma fila horizontal ou vertical.
% -----------------------------------------------------------------------------
somas_H_V(H_V, Posicoes, Fila, Somas) :-
    H_V == v,
    findall(Soma, 
        (member(Pos, Posicoes), nth1(Pos, Fila, Somas), nth0(0, Somas, Soma)), 
        Somas).

somas_H_V(H_V, Posicoes, Fila, Somas) :-
    H_V == h,
    findall(Soma, 
        (member(Pos, Posicoes), nth1(Pos, Fila, Somas), nth0(1, Somas, Soma)), 
        Somas).

% -----------------------------------------------------------------------------
%                           espaco_fila/3
% Significa que Esp eh um espaco de Fila.
% -----------------------------------------------------------------------------
espaco_fila(Fila, Esp, H_V) :-
    pos_listas_Fila(Fila, Posicoes),
    espaco(Fila, Posicoes, Pos_Somas, Vars), 
    length(Vars, N), sort(Vars, Vars_Sorted), sort(Pos_Somas, Pos_Somas_Sorted),
    somas_H_V(H_V, Pos_Somas_Sorted, Fila, Somas),
    between(1, N, Indice), 
    nth1(Indice, Vars_Sorted, Espaco), nth1(Indice, Somas, Soma), 
    Esp = espaco(Soma, Espaco).

% -----------------------------------------------------------------------------
%                           espacos_fila/3
% Significa que Espacos eh a lista de todos os espacos de Fila, da esquerda 
% para a direita.
% -----------------------------------------------------------------------------
espacos_fila(H_V, Fila, []) :-
    \+ espaco_fila(Fila, _, H_V), !.

espacos_fila(H_V, Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos).

% -----------------------------------------------------------------------------
%                           espacos_puzzle/2
% Significa que Espacos eh a lista de todos os espacos de Puzzle.
% -----------------------------------------------------------------------------
espacos_puzzle(Puzzle, Espacos) :-
    mat_transposta(Puzzle, Puzzle_Transp),
    bagof(Esp, 
        Fila^(member(Fila, Puzzle), espaco_fila(Fila, Esp, h)), 
        Espacos_h),
    bagof(Esp, 
        Fila^(member(Fila, Puzzle_Transp), espaco_fila(Fila, Esp, v)), 
        Espacos_v),
    append(Espacos_h, Espacos_v, Espacos).

% -----------------------------------------------------------------------------
%                           posicoes_comuns/2
% Predicado Auxiliar.
% Devolve "true" se Vars e Vars_Cand tem pelo menos uma posicao em comum
% e false em caso contrario.
% -----------------------------------------------------------------------------
posicoes_comuns([V | _], Vars_Cand) :-
    member(V_Cand, Vars_Cand), unifiable(V_Cand, V, []), !. 

posicoes_comuns([_ | Vars], Vars_Cand) :- posicoes_comuns(Vars, Vars_Cand).

% -----------------------------------------------------------------------------
%                           espacos_com_posicoes_comuns_aux/3
% Significa que Esp_Pos_Comum eh um espaco com variaveis em comum com Esp, 
% exceptuando Esp. Os espacos aparecem pela mesma ordem que aparecem em Espacos.
% -----------------------------------------------------------------------------
espacos_com_posicoes_comuns_aux(Espacos, Esp, Esp_Pos_Comum) :-
    Esp = espaco(_, Vars),
    member(Candidato, Espacos), Candidato = espaco(_, Vars_Cand), 
    posicoes_comuns(Vars, Vars_Cand), Candidato \= Esp,
    Esp_Pos_Comum = Candidato.

% -----------------------------------------------------------------------------
%                           espacos_com_posicoes_comuns/3
% Significa que Espacos_com eh a lista de espacos com variaveis em comum
% com Esp, exceptuando Esp. Os espacos em Esp_com aparecem pela mesma ordem
% que aparecem em Espacos.
% -----------------------------------------------------------------------------
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    bagof(Esp_Pos_Comum, 
        espacos_com_posicoes_comuns_aux(Espacos, Esp, Esp_Pos_Comum), 
        Esps_com).
    
% -----------------------------------------------------------------------------
%                           percorre_lst_perms/3
% Predicado Auxiliar.
% Significa que Comuns eh a lista de listas [pos, numero], significando 
% que as listas verificadas contem o numero "numero" na posicao "pos".
% -----------------------------------------------------------------------------
percorre_lst_perms(P, Perm, Comuns) :-
    length(P, N),
    findall([Pos, Num], (between(1, N, Pos), nth1(Pos, P, Num), nth1(Pos, Perm, Candidato),
        Num =:= Candidato), Comuns).

% -----------------------------------------------------------------------------
%                           comuns/2
% Predicado Auxiliar
% Significa que Res eh a lista que contem todos os Comuns.
% -----------------------------------------------------------------------------
comuns([P | Perms], Res) :-
    findall(Comuns, 
        (member(Perm, Perms), percorre_lst_perms(P, Perm, Comuns), Comuns \= []), 
        Res).

% -----------------------------------------------------------------------------
%                           numeros_comuns/2
% Significa que Numeros_Comuns eh uma lista de pares (pos, numero), significando
% que todas as listas de Lst_Perms contem o numero "numero" na posicao "pos".
% -----------------------------------------------------------------------------
numeros_comuns(Lst_Perms, Numeros_comuns) :-
    comuns(Lst_Perms, Aux),
    length(Lst_Perms, Len_Perms), length(Aux, Len_Comuns),
    Limite is Len_Perms - 1,  Len_Comuns < Limite,              % basta 1 falhar para nao ter numeros comuns. 
    Numeros_comuns = [], !.

numeros_comuns(Lst_Perms, Numeros_comuns) :-
    comuns(Lst_Perms, Aux),    
    nth0(0, Aux, P),
    findall((Pos, Num), 
        (member(Perm, Aux), P == Perm, member(Comuns, P), 
        nth0(0, Comuns, Pos), nth0(1, Comuns, Num)), 
        Res),
    list_to_set(Res, Numeros_comuns).

    