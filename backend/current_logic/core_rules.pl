:- module(core_rules, [
    rdf_semantic_eq_only/2,
    rdf_equivalence/2,
    subsumes/2,
    hasComponentRelation/2
]).

:- use_module(rdf_prefixes). % Load your prefix module for vocab namespaces

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(dif)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RDF Semantic Equality and Inequality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TODO: Add optimized versions using cut to avoid backtracking after one success

% rdf_equivalence(+Resource1, +Resource2)
% True if Resource1 and Resource2 are semantically but not syntactically equal.
rdf_semantic_eq_only(Resource1, Resource2) :-
    rdf_reachable(Resource1, owl:sameAs, Resource2), % owl:sameAs is reflexive and symmetric (syntactic/semantic equality)
    dif(Resource1, Resource2). % Ensure they are not the same term

% rdf_equivalence(+Resource1, +Resource2)
% True if Resource1 and Resource2 are syntactically or semantically equal.
rdf_equivalence(Resource1, Resource2) :-
    (   Resource1 = Resource2
    ;   rdf_semantic_eq_only(Resource1, Resource2)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subsumption Logic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% subsumes(+SuperType, ?SubType)
% True if SuperType subsumes SubType, directly or semantically.
% Main subsumes predicate
subsumes(SType, Type) :-
    ground(SType),
    dif(SType, Type), % Quickly eliminate identical cases
    (   % Optimized order of cases to minimize computation
        direct_subsumption(SType, Type)
    ;   
        subsumption_via_semantic_equivalence(SType, Type)
    ;   
        variable_subsumption_exploration(SType, Type)
    ).

% Case 1: Direct subsumption
direct_subsumption(SType, Type) :-
    % Check if Type is reachable directly from SType
    ground(Type),
    rdf_reachable(SType, vocab:subsumes, Type).

% Case 2: Subsumption via semantic equivalence
subsumption_via_semantic_equivalence(SType, Type) :-
    % Check if Type has a semantic equivalent that SType subsumes
    ground(Type),
    rdf_semantic_eq_only(Type, Type2), % Find the semantic equivalent
    rdf_reachable(SType, vocab:subsumes, Type2).

% Case 3: Variable exploration for subsumption
variable_subsumption_exploration(SType, Type) :-
    var(Type), % If Type is a variable, explore all possible values
    rdf_reachable(SType, vocab:subsumes, IntermediateType),
    dif(SType, IntermediateType), % avoid trivial cases
    Type = IntermediateType.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Component Relationships
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% hasDirectComponent(+CType, -Type)
% True if Type is a direct component of CType.
hasDirectComponent(CType, Type) :-
    rdf_reachable(CType, vocab:hasComponent, Type), 
    dif(CType, Type),  % Ensure CType and Type are distinct
    nonvar(Type).      % Ensure Type is instantiated

% hasComponentRelation(+CType, -Type)
% True if CType has a direct or semantic component relationship with Type.
hasComponentRelation(CType, Type) :-
    rdfs_individual_of(CType, vocab:DrugCombinationType),
    (
        % Direct component
        hasDirectComponent(CType, Type);
        
        % Component through semantic equivalence
        rdf_semantic_eq_only(Type, AltType),
        hasDirectComponent(CType, AltType)
    ),
    nonvar(Type). % Ensure Type is instantiated 
