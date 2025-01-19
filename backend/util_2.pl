:- module(util, [
    semanticallySameAs/2,
    semanticallyDifferent/2,
    cached_result/3,
    clear_cache/0
]).

:- dynamic cache/2.

% Semantic similarity
semanticallySameAs(X, Y) :- rdf_equal(X, Y).

% Semantic difference
semanticallyDifferent(X, Y) :- \+ semanticallySameAs(X, Y).

% Caching mechanisms (not in use)
cached_result(Key, Value, ComputePredicate) :-
    (   cache(Key, Value) ->
        true
    ;   call(ComputePredicate, Value),
        assertz(cache(Key, Value))
    ).

clear_cache :-
    retractall(cache(_, _)).