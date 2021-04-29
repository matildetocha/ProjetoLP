:- [codigo_comum].

combinacoes_soma(0, _, _, []).

combinacoes_soma(N, Els, Soma, [Comb | Combs]) :-
    combinacao(N, Els, Comb),
    nth0(0, Comb, El1),
    nth0(1, Comb, El2),
    El1 + El2 =:= Soma,
    combinacoes_soma(N, Els, Soma, Combs).

combinacoes_soma(N, Els, Soma, Combs) :-
    combinacao(N, Els, Comb),
    nth0(0, Comb, El1),
    nth0(1, Comb, El2),
    El1 + El2 =\= Soma,
    combinacoes_soma(N, Els, Soma, Combs).
