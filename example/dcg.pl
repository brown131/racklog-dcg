%%%% A simple English grammar in Prolog DCG notation.

sentence --> noun_phrase, verb_phrase.
noun_phrase --> proper_noun.
noun_phrase --> det, noun.
noun_phrase --> det, noun, rel_clause.
verb_phrase --> trans_verb, noun_phrase.
verb_phrase --> intrans_verb.
rel_clause --> dem_pronoun, verb_phrase.
det --> [the].
det --> [a].
noun --> [cat].
noun --> [bat].
proper_noun --> [john].
proper_noun --> [mary].
dem_pronoun --> [that].
trans_verb --> [eats].
intrans_verb --> [lives].

% sentence(X, []).
% sentence([a, cat, eats, the, bat], []).
% noun(X, []).
