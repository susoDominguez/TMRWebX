:- module(predicate_properties, [set_predicate_properties/0]).
:- use_module(library(semweb/rdf11)).

% set RDF predicate properties
set_predicate_properties :-
    forall(member(Predicate, [
        vocab4i:comparedWith-[symmetric(true), transitive(false)],
        %owl:sameAs-[symmetric(true), transitive(true)],
        vocab:subsumes-[transitive(true), symmetric(false)],
        vocab:subsumedBy-[transitive(true), symmetric(false), inverse_of(vocab:subsumes)],
        %vocab:administrationOf-[transitive(false), symmetric(false)],
        vocab:hasComponent-[transitive(true), symmetric(false)],
        vocab:incompatibleWith-[symmetric(true), transitive(false)],
        vocab:hasEqOrder-[symmetric(true), transitive(true)],
        vocab:hasHigherOrderThan-[transitive(true), symmetric(false)]
    ]),
    (   Predicate = Prop-[PropOptions],
        maplist(rdf_set_predicate(Prop), PropOptions)
    )). 