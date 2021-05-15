%% Matilde Tocha 99108

:- [codigo_comum].

% -----------------------------------------------------------------------------
%                           combinacoes_soma
% -----------------------------------------------------------------------------  
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
%                           permutacoes_soma
% -----------------------------------------------------------------------------  
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
%                            espaco_fila
% -----------------------------------------------------------------------------  
% -----------------------------------------------------------------------------
%                           pos_listas_Fila/2
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
%                           espaco/4
% Predicado Auxiliar.
% Significa que Vars_Esp eh a lista cujos elementos sao as variaveis de um 
% espaco.
% -----------------------------------------------------------------------------
espaco(Fila, Posicoes, Pos_Somas, Vars_Esp) :- 
    espaco(Fila, Posicoes, Pos_Somas, Vars_Esp, [], []).

espaco(_, [], Aux_Somas, Aux, Aux_Somas, Aux).

espaco(Fila, [[_, Lst2]], Pos_Somas, Vars_Esp, Aux_Somas, Aux) :- 
    length(Fila, Max), nth1(Max, Fila, Lst1), unifiable(Lst1, Lst2, []),
    espaco(Fila, [], Pos_Somas, Vars_Esp, Aux_Somas, Aux).

espaco(Fila, [[El, _]], Pos_Somas, Vars_Esp, Aux_Somas, Aux) :- 
    length(Fila, Max), Min is El + 1,
    bagof(Membro, Indice^(between(Min, Max, Indice), nth1(Indice, Fila, Membro)), Vars),
    espaco(Fila, [], Pos_Somas, Vars_Esp, [El | Aux_Somas], [Vars | Aux]).

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
%                            somas_H_V/4
% Predicado Auxiliar.
% Significa Somas eh uma lista cujos elementos sao as somas da fila, 
% conforme se trate de uma fila horizontal ou vertical.
% -----------------------------------------------------------------------------
somas_H_V(v, Posicoes, Fila, Somas) :-
    findall(Soma, 
        (member(Pos, Posicoes), nth1(Pos, Fila, Somas), nth0(0, Somas, Soma)), 
        Somas).

somas_H_V(h, Posicoes, Fila, Somas) :-
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
%                           espacos_fila
% -----------------------------------------------------------------------------  
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
%                           espacos_puzzle
% -----------------------------------------------------------------------------  
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
%                           espacos_com_posicoes_comuns_aux
% -----------------------------------------------------------------------------  
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
% Predicado Auxiliar.
% Significa que Esp_Pos_Comum eh um espaco com variaveis em comum com Esp, 
% exceptuando Esp. Os espacos aparecem pela mesma ordem que aparecem em Espacos.
% -----------------------------------------------------------------------------
espacos_com_posicoes_comuns_aux(Espacos, Esp, Esp_Pos_Comum) :-
    Esp = espaco(_, Vars),
    member(Candidato, Espacos), Candidato = espaco(_, Vars_Cand), 
    posicoes_comuns(Vars, Vars_Cand), \+ unifiable(Candidato, Esp, []),
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
%                           permutacoes_soma_espacos
% -----------------------------------------------------------------------------  
% -----------------------------------------------------------------------------
%                           elementos_perm/2
% Predicado Auxiliar. Serve para aumentar a eficiencia do codigo, visto que a 
% soma, com numeros acima ou iguais ah Soma , nunca sera igual a Soma. 
% Significa que Els eh a lista de inteiros menores que Soma. 
% -----------------------------------------------------------------------------
elementos_perm(Soma, Els) :-
    Soma =< 9, Max is Soma - 1, 
    findall(El, between(1, Max, El), Els), !.

elementos_perm(Soma, Els) :-
    Soma > 9, Els = [1, 2, 3, 4, 5, 6, 7, 8, 9].

% -----------------------------------------------------------------------------
%                           permutacoes_soma_espacos_aux/2
% Predicado Auxiliar.
% Significa que Perm_Soma eh uma lista de 2 elementos, em que o 1o elemento
% eh um espaco de Espacos e o 2o eh a lista ordenada de permutacoes cuja 
% soma eh igual ah soma do espaco.
% -----------------------------------------------------------------------------
permutacoes_soma_espacos_aux(Espacos, Perm_soma) :-
    member(Esp, Espacos), Esp = espaco(Soma, Vars), 
    length(Vars, N), elementos_perm(Soma, Els), 
    permutacoes_soma(N, Els, Soma, Perms), 
    Perm_soma = [Esp, Perms].

% -----------------------------------------------------------------------------
%                           permutacoes_soma_espacos/2
% Significa que Perms_Soma eh a lista de listas de 2 elementos, em que o 1o 
% elemento eh um espaco de Espacos e o 2o eh a lista ordenada de permutacoes
% cuja soma eh igual ah soma do espaco.
% -----------------------------------------------------------------------------  
permutacoes_soma_espacos(Espacos, Perms_soma) :-
    bagof(Perm_soma, permutacoes_soma_espacos_aux(Espacos, Perm_soma), Perms_soma).


% -----------------------------------------------------------------------------
%                           permutacao_possivel_espaco
% -----------------------------------------------------------------------------  
% -----------------------------------------------------------------------------
%                           posicoes_espacos_comuns/3
% Predicado Auxiliar.
% Significa que Pos_Comuns eh a lista cujos elementos sao as posicoes dos espacos
% Esps_Comuns em Espacos.
% -----------------------------------------------------------------------------  
posicoes_espacos_comuns(Espacos, Esps_Comuns, Pos_Comuns) :- 
    posicoes_espacos_comuns(Espacos, Esps_Comuns, Pos_Comuns, []), !.

posicoes_espacos_comuns(_, [], Aux, Aux).

posicoes_espacos_comuns(Espacos, [Comum | Esps_Comuns], Pos_Comuns, Aux) :-
    length(Espacos, N), between(1, N, Pos), 
    nth1(Pos, Espacos, Candidato), unifiable(Comum, Candidato, []),
    posicoes_espacos_comuns(Espacos, Esps_Comuns, Pos_Comuns, [Pos | Aux]).

% -----------------------------------------------------------------------------
%                           posicao_espaco/3
% Predicado Auxiliar.
% Significa que Pos_Esp eh a posicao do espaco Esp em Espacos.
% -----------------------------------------------------------------------------  
posicao_espaco(Espacos, Esp, Pos_Esp) :-
    length(Espacos, N), between(1, N, Pos), 
    nth1(Pos, Espacos, Candidato), unifiable(Esp, Candidato, []), Pos_Esp = Pos, !.

% -----------------------------------------------------------------------------
%                           permutacao_possivel/5
% Predicado Auxiliar.
% Significa que Perm_Poss eh uma possivel permutacao para o espaco Esp.
% ----------------------------------------------------------------------------- 
permutacao_possivel([P_Esp | _], Perms_Cand, Indice1, Indice2, Perm_Poss) :-
    member(P_Cand, Perms_Cand), 
    nth1(Indice1, P_Esp, Num1), nth1(Indice2, P_Cand, Num2), Num1 =:= Num2, 
    Perm_Poss = P_Esp.

permutacao_possivel([_ | Perms_Esp], Perms_Cand, Indice1, Indice2, Perm_Poss) :-
    permutacao_possivel(Perms_Esp, Perms_Cand, Indice1, Indice2, Perm_Poss).

% -----------------------------------------------------------------------------
%                           permutacoes_possiveis_aux/5
% Predicado Auxiliar.
% Significa que Perms_Poss eh uma lista com diferentes permutacoes que podem
% ser possiveis para o espaco Esp.
% ----------------------------------------------------------------------------- 
permutacoes_possiveis_aux(Vars_Esp, Vars_Cand, Perms_Esp, Perms_Cand, Perms_Poss) :-
    length(Vars_Esp, N1), between(1, N1, Indice1), nth1(Indice1, Vars_Esp, V_Esp),
    length(Vars_Cand, N2), between(1, N2, Indice2), nth1(Indice2, Vars_Cand, V_Cand), 
    unifiable(V_Esp, V_Cand, []),
    setof(Perm_Poss, 
        permutacao_possivel(Perms_Esp, Perms_Cand, Indice1, Indice2, Perm_Poss),
        Perms_Poss).

% -----------------------------------------------------------------------------
%                           inters_atual/3
% Predicado Auxiliar.
% Significa que Atual eh ultima lista de permutacoes possiveis, que foi 
% intercetada com as listas de permutacoes possiveis anteriores.
% -----------------------------------------------------------------------------  
inters_atual([], Poss, Poss).

inters_atual(Inters, Atual, _) :- nth0(0, Inters, Atual).

% -----------------------------------------------------------------------------
%                           intersecao_permutacoes/2
% Predicado Auxiliar.
% Significa que Res eh a lista final de permutacoes possiveis para o espaco Esp.
% -----------------------------------------------------------------------------  
intersecao_permutacoes([Poss], [], [Poss]). 

intersecao_permutacoes(Poss, Inters, Res) :-
    intersection(Poss, Inters, Res).

% -----------------------------------------------------------------------------
%                           percorre_perms_soma/4
% Predicado Auxiliar.
% Significa que Res eh a lista de todas as possiveis permutacoes para o espaco
% Esp.
% -----------------------------------------------------------------------------  
percorre_perms_soma(Pos_Esp, [Pos | Pos_Comuns], Perms_soma, Res) :- 
    percorre_perms_soma(Pos_Esp, [Pos | Pos_Comuns], Perms_soma, Res, []).

percorre_perms_soma(_, [], _, Inters, Inters).

percorre_perms_soma(Pos_Esp, [Pos | Pos_Comuns], Perms_soma, Res, Inters) :-
    nth1(Pos, Perms_soma, Candidato), nth1(Pos_Esp, Perms_soma, Perm_Soma_Esp),
    nth0(0, Candidato, espaco(_, Vars_Cand)), nth0(0, Perm_Soma_Esp, espaco(_, Vars_Esp)),
    nth0(1, Candidato, Perms_Cand), nth0(1, Perm_Soma_Esp, Perms_Esp),
    permutacoes_possiveis_aux(Vars_Esp, Vars_Cand, Perms_Esp, Perms_Cand, Poss),
    inters_atual(Inters, Atual, Poss),
    intersection(Poss, Atual, Aux), 
    percorre_perms_soma(Pos_Esp, Pos_Comuns, Perms_soma, Res, [Aux | Inters]).

% -----------------------------------------------------------------------------
%                           permutacao_possivel_espaco/4
% Significa que Perm eh uma permutacao possivel para o espaco Esp.
% -----------------------------------------------------------------------------  
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_Comuns), 
    posicoes_espacos_comuns(Espacos, Esps_Comuns, Pos_Comuns), 
    posicao_espaco(Espacos, Esp, Pos_Esp), 
    percorre_perms_soma(Pos_Esp, Pos_Comuns, Perms_soma, Aux),
    nth0(0, Aux, Perms_Poss), member(Perm, Perms_Poss).


% -----------------------------------------------------------------------------
%                           permutacoes_possiveis_espaco
% ----------------------------------------------------------------------------- 
% -----------------------------------------------------------------------------
%                           permutacoes_possiveis_espaco/4
% Significa que Perms_poss eh umas lista de 2 elementos em que o primeiro eh a
% lista de variaveis de Esp e o segundo eh a lista ordenada de permutacoes
% possiveis para o espaco Esp.
% ----------------------------------------------------------------------------- 
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
    Esp = espaco(_, Vars),
    findall(Perm, 
        permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma),
        Aux),
    Perms_poss = [Vars, Aux].


% -----------------------------------------------------------------------------
%                           permutacoes_possiveis_espacos
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
%                           permutacoes_possiveis_espacos/2
% Significa que Perms_poss_esps eh a lista de permutacoes possiveis.
% ----------------------------------------------------------------------------- 
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    bagof(Perms_poss, 
        Esp^(member(Esp, Espacos), 
        permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)),
        Perms_poss_esps).


% -----------------------------------------------------------------------------
%                           numeros_comuns
% -----------------------------------------------------------------------------
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

    