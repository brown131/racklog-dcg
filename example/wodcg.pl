%%%% A simple English grammar without DCG notation.

sentence(S0,S) :- noun_phrase(S0,S1), verb_phrase(S1,S).
noun_phrase(S0,S) :- proper_noun(S0,S).
noun_phrase(S0,S) :- det(S0,S1), noun(S1,S).
noun_phrase(S0,S) :- det(S0,S1), noun(S1,S2), rel_clause(S2,S).
verb_phrase(S0,S) :- trans_verb(S0,S1), noun_phrase(S1,S).
verb_phrase(S0,S) :- intrans_verb(S0,S).
rel_clause(S0,S) :- dem_pronoun(S0,S1), verb_phrase(S1,S).
det([the|X], X).
det([a|X], X).
noun([cat|X], X).
noun([bat|X], X).
proper_noun([john|X], X).
proper_noun([mary|X], X).
dem_pronoun([that|X], X).
trans_verb([eats|X], X).
intrans_verb([lives|X], X).

% sentence(X, []).
% sentence([a, cat, eats, the, bat], []).
% noun(X, []).
