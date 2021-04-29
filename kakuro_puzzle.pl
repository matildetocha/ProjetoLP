%% Matilde Tocha 99108

:- [codigo_comum].
combinacoes(N, Els, Combs) :-
    findall(Comb, combinacao(N, Els, Comb), Combs).

% ----------------------------------------------------------------------
%                           combinacoes_soma/4
% ----------------------------------------------------------------------
combinacoes_soma(N, Els, Soma, Combs) :-
    combinacoes(N, Els, Aux),
    findall([El1, El2], 
        (member(Mem, Aux), nth0(0, Mem, El1), nth0(1, Mem, El2), Soma is El1 + El2), 
        Combs).

% ----------------------------------------------------------------------
%                           permutacoes_soma/4
% ----------------------------------------------------------------------
permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes(N, Els, Aux),
    findall(Perm, (member(Mem, Aux), permutation(Mem, Perm)), Aux2),
    findall([El1, El2], 
        (member(Mem, Aux2), nth0(0, Mem, El1), nth0(1, Mem, El2), Soma is El1 + El2), 
        Res),
    sort(Res, Perms).

  