%% -------------------------------------------------------
aux([P | R], [P | Res]) :-
    var(P),
    aux(R, Res).

aux([P | _], []) :- is_list(P), !.

variaveis([P | R], H_V, A, L) :- 
    var(P), 
    variaveis(R, H_V, A, L).

variaveis([P | R], _, [], _) :- 
    is_list(P), 
    aux(R, Aux),
    length(Aux, N), N == 0.

variaveis([P | R], H_V, Aux, V) :- 
    is_list(P), 
    aux(R, Aux),
    length(Aux, N), N > 0,
    H_V == v, nth0(0, P, V).

variaveis([P | R], H_V, Aux, H) :- 
    is_list(P), 
    aux(R, Aux),
    length(Aux, N), N > 0,
    H_V == h, nth0(1, P, H).

espaco_fila(Fila, [], H_V) :-
    variaveis(Fila, H_V, Aux, _),
    Aux == [].

espaco_fila(Fila, Esp, H_V) :-
    variaveis(Fila, H_V, Aux, L),
    Esp = espaco(L, Aux).

%% ---------------------------------------------------------

pos_variaveis(Fila, Posicoes) :-
    length(Fila, N),
    findall([Pos, Candidato], 
    (between(1, N, Pos), nth1(Pos, Fila, Candidato), is_list(Candidato)),
    Posicoes).

espaco(Fila, Posicoes, Res) :- espaco(Fila, Posicoes, Res, []).

espaco(_, [], Aux, Aux).

espaco(Fila, [[El, _]], Res, Aux) :- 
    length(Fila, Max), Min is El + 1,
    bagof(Membro, Indice^(between(Min, Max, Indice), nth1(Indice, Fila, Membro)), Vars),
    espaco(Fila, [], Res, [Vars | Aux]).

espaco(Fila, [El1, El2 | R], Res, Aux) :-
    nth0(0, El1, Pos1), nth0(0, El2, Pos2),
    Min is Pos1 + 1, Max is Pos2 - 1,
    Min < Max,
    bagof(Membro, Indice^(between(Min, Max, Indice), nth1(Indice, Fila, Membro)), Vars),
    append([El2], R, New_R),
    espaco(Fila, New_R, Res, [Vars | Aux]).

espaco(Fila, [El1, El2 | R], Res, Aux) :-
    nth0(0, El1, Pos1), nth0(0, El2, Pos2),
    Min is Pos1 + 1, Max is Pos2 - 1,
    Min >= Max,
    append([El2], R, New_R),
    espaco(Fila, New_R, Res, Aux).

espacos_fila(H_V, Fila, Espacos) :-
    pos_variaveis(Fila, Posicoes),
    espaco(Fila, Posicoes, Vars), length(Vars, N), sort(Vars, Vars_Sorted),
    H_V == v,
    findall(Soma, (member(Mem, Posicoes), nth0(1, Mem, Somas), nth0(0, Somas, Soma), Soma =\= 0), Somas_v),
    bagof(espaco(Soma, Espaco), Indice^(between(1, N, Indice), nth1(Indice, Vars_Sorted, Espaco), nth1(Indice, Somas_v, Soma)), Espacos), !.

espacos_fila(H_V, Fila, Espacos) :-
    pos_variaveis(Fila, Posicoes),
    espaco(Fila, Posicoes, Vars), length(Vars, N), sort(Vars, Vars_Sorted),
    H_V == h,
    findall(Soma, (member(Mem, Posicoes), nth0(1, Mem, Somas), nth0(1, Somas, Soma), Soma =\= 0), Somas_h),
    bagof(espaco(Soma, Espaco), Indice^(between(1, N, Indice), nth1(Indice, Vars_Sorted, Espaco), nth1(Indice, Somas_h, Soma)), Espacos), !.

espaco_fila1(Fila, Esp, H_V) :-
    espacos_fila(H_V, Fila, Espacos), member(Mem, Espacos),
    Esp = Mem.

% ------------------------------------------------------------------------------
Puzzle = [[[0, 0], [0, 0], [0, 0], [17, 0], [10, 0]],
		 [[0, 0], [24, 0], [11, 3], P24, P25], 
         [[0,16], P32, P33, P34, P35],
		 [[0,26], P42, P43, P44, P45],
         [[0,17], P52, P53, [0,0], [0,0]]],
         espacos_puzzle(Puzzle, Espacos),
nth1(1, Espacos, Esp1),
espacos_com_posicoes_comuns(Espacos, Esp1, Esps_com).

Espacos = [espaco(3, [P24, P25]), espaco(16, [P32, P33, P34, P35]), 
        espaco(26, [P42, P43, P44, P45]), espaco(17, [P52, P53]),
        espaco(24, [P32, P42, P52]), espaco(11, [P33, P43, P53]), 
        espaco(17, [P24, P34, P44]), espaco(10, [P25, P35, P45])],
Esp1 = espaco(3, [P24, P25]),
espacos_com_posicoes_comuns(Espacos, Esp1, Esps_com).

