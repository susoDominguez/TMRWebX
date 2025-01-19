:- module(formula_builder, [create_formula/3]).

:- use_module(library(semweb/rdf11)).
%:- use_module(library(semweb/rdfs)).

% Create the predicate logic formula that represents a precondition given a URI
create_formula(URI, Formula, Mapping) :-
    build_tree(URI, Tree), !,
    assign_prolog_vars(Tree, Formula, Mapping).

% Build the logical tree representation
build_tree(URI, p(Label)) :-
    rdf(URI, rdf:type, vocab:SituationType),
    !,
    rdf(URI, rdfs:label, literal(Label)).

% Handle and relationships
build_tree(URI, *(Parts)) :-
    rdf(URI, rdf:type, vocab:PreconditionType),
    rdf(URI, vocab:and, _),
    !,
    findall(Member, rdf(URI, vocab:and, Member), Members),
    maplist(build_tree, Members, Parts).

% Handle or relationships
build_tree(URI, +(Parts)) :-
    rdf(URI, rdf:type, vocab:PreconditionType),
    rdf(URI, vocab:or, _),
    !,
    findall(Member, rdf(URI, vocab:or, Member), Members),
    maplist(build_tree, Members, Parts).

% Handle neg relationships
build_tree(URI, ~(Part)) :-
    rdf(URI, rdf:type, vocab:PreconditionType),
    rdf(URI, vocab:neg, Member),
    !,
    build_tree(Member, Part).