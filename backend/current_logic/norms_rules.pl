:- module(norm_rules, [similarTo/2, has_relation_action_types/2, regulates/4, regulates/5, regulates/6, causes/4, causes/5, basedOn/3, incompatibleWith/3, inverse_transitions/2, inverse_transitions/3, similar_transitions/2, similar_transitions/3, is_greater_or_eq_modifier_than/2, is_greater_modifier_than/2]).

:- use_module(core_rules).
:- use_module(rdf_tmr_prefixes).

:- use_module(library(semweb/rdf11)).        % Enhanced RDF querying and typed literals
:- use_module(library(semweb/rdf_db)).       % Core RDF database management
:- use_module(library(semweb/rdfs)).         % RDFS reasoning
:- use_module(library(dif)).            

rdf_db:rdf_reasoner(rdfs).
% rdf_db:rdf_reasoner(owl).


%%%%%%%%%%
%TODO: Reg could be given by service to contextualize the interactions search

% this version of the predicate discards the CBs so no iteration of regulates occurs for each CB in Norm
regulates(Reg, Norm, ActionT, Strength):-
    rdfs_individual_of(Reg, vocab:'ClinicalGuideline'), %for each available CG. This must be filtered to the contextualized CG
    rdf(Norm, vocab:partOf, Reg, Norm),
    rdfs_individual_of(Norm, vocab:'ClinicalRecommendation'),
    rdf(Norm, vocab:strength, Strength, Norm),
    rdf(Norm, vocab:aboutExecutionOf, ActionT, Norm),
    % Check for optional property identifying source guideline in combined dataset
    ( rdf(Norm, vocab:isPartOf, SourceReg, Norm) ; SourceReg = Reg ).

regulates(Reg, Norm, ActionT, Strength, CBelief):-
    regulates(Reg, Norm, ActionT, Strength),
    rdf(Norm, vocab:basedOn, CBelief, Norm).

regulates(Reg, Norm, ActionT, Strength, CBelief, Contrib):-
    regulates(Reg, Norm, ActionT, Strength, CBelief),
    rdf(CBelief, vocab:contribution, Contrib, Norm).


/* *********************************** */
%  BELIEF-BASED RULES
/* *********************************** */

causes(ActionT, Tr, Frequency, CBelief) :-
    rdfs_individual_of(CBelief, vocab:'CausationBelief'),
    rdf(CBelief, vocab:frequency, Frequency, CBelief),
    rdf(ActionT, vocab:causes, Tr, CBelief).

causes(ActionT, Tr, Frequency, CBelief, Source) :-
    causes(ActionT, Tr, Frequency, CBelief),
    rdf(CBelief, prov:wasDerivedFrom, Source).

basedOn(Norm, CB, Cntrb):-
        ground(Norm),
        rdf(Norm, vocab:basedOn, CB, Norm),
        rdf(CB, vocab:contribution, Cntrb, Norm).


similarTo(CBelief1, CBelief2) :-
    rdfs_individual_of(CBelief1, vocab:'CausationBelief'),
    rdfs_individual_of(CBelief2, vocab:'CausationBelief'),
    causes(ActionT1, Tr1, _, CBelief1),
    causes(ActionT2, Tr2, _, CBelief2),
    \+ rdf_equivalence(ActionT1,ActionT2), % but subsumption is allowed
    similar_transitions(Tr1, Tr2).


incompatibleWith(EventT1, EventT2, IBelief) :-
    rdfs_individual_of(EventT1, vocab:EventType),
    rdfs_individual_of(EventT2, vocab:EventType),
    rdf(EventT1, vocab:incompatibleWith, EventT2, IBelief).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RElationship between care actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Related if:
% - Same syntactically
% - Semantically equivalent
% - Related through other relationships


% Check if two actions are related by subsumption (drugCombinationTypes not yet fully applicable)
is_related_to(ActionT1, ActionT2) :-
    nonvar(ActionT1),
    nonvar(ActionT2),
    rdf(ActionT1, vocab:administrationOf, DrugType1),
    rdf(ActionT2, vocab:administrationOf, DrugType2),
    (   % case 1: Subsumption relation
        subsumes(ActionT1, ActionT2)     
    ;   % case 2: Direct component relationship
        hasComponentRelation(DrugType1, DrugType2)
    ).

%  relation between two care action types (symmetric search)
is_related_to_symm_search(ActionT1, ActionT2) :-
    (  
        is_related_to(ActionT1, ActionT2)
    ;
        is_related_to(ActionT2, ActionT1)
    ) .

has_relation_action_types(ActionT1, ActionT2) :-
    nonvar(ActionT1),
    nonvar(ActionT2),
    % Directly related or via subsumption of effects
    (   rdf_equivalence(ActionT1, ActionT2) 
    ;   is_related_to_symm_search(ActionT1, ActionT2)
    ).

/*

% Check if two actions are related by grouping criteria, symmetric logic optimized
related_to_by_grouping_criteria_symmetric_search(ActionT1, ActionT2, Norm1, Norm2) :-
    % Ensure both actions are linked to drug categories
    rdf(ActionT1, vocab:administrationOf, DrugType1),
    rdfs_individual_of(DrugType1, vocab:DrugCategory),
    rdf(ActionT2, vocab:administrationOf, DrugType2),
    rdfs_individual_of(DrugType2, vocab:DrugCategory),
    % Check active grouping criteria for symmetry
    symmetric_grouping_check(DrugType1, ActionT1, Norm1, DrugType2, ActionT2, Norm2).

% Symmetric grouping criteria check
symmetric_grouping_check(DrugType1, ActionT1, Norm1, DrugType2, ActionT2, Norm2) :-
    has_grouping_criteria(DrugType1, ActionT1, Norm1, Tr),
    has_grouping_criteria(DrugType2, ActionT2, Norm2, Tr), !. % Symmetric match found
symmetric_grouping_check(DrugType2, ActionT2, Norm2, DrugType1, ActionT1, Norm1) :-
    has_grouping_criteria(DrugType2, ActionT2, Norm2, Tr),
    has_grouping_criteria(DrugType1, ActionT1, Norm1, Tr). % Fallback to opposite direction

% Active grouping criterion based on transition and belief
has_grouping_criteria(DrugType, ActionT, Norm, Tr) :-
    rdf(DrugType, vocab:hasGroupingCriteria, Tr),
    rdf(Norm, vocab:basedOn, CBelief, Norm),
    causes(ActionT, Tr, _, CBelief).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* *********************************** */
%  EVENT_TYPE-BASED RULES
/* *********************************** */
%

% Define a transition and its core components
has_transition(PreSitT, Derivative, PostSitT, Tr) :-
    rdfs_individual_of(Tr, vocab:'TransitionType'),
    rdf(Tr, vocab:hasTransformableSituation, PreSitT),
    rdf(Tr, vocab:hasExpectedSituation, PostSitT),
    ( rdf(Tr, vocab:derivative, Derivative) ; true ). % Derivative is optional

% Define a transition with a Trope type (e.g., airflow limitation measurement)
has_transition(TropeT, PreSitT, Derivative, PostSitT, Tr) :-
    has_transition(PreSitT, Derivative, PostSitT, Tr),
    rdf(Tr, vocab:affects, TropeT).

/* *********************************** */

% Two-argument version for backward compatibility
similar_transitions(Tr1, Tr2) :-
    similar_transitions(Tr1, Tr2, _).

% Check if two transitions are similar and return the best answer
similar_transitions(Tr1, Tr2, ModVal) :-
    Tr1 \= Tr2, % Ensure they are distinct URIs
    (   % Case 1:  equal pre-situation (syntactically or semantically)
        has_transition(PreSitT1, Derivative1, _, Tr1), 
        has_transition(PreSitT2, Derivative1 , _, Tr2),
        rdf_equivalence(PreSitT1, PreSitT2), 
        ModVal = 1, !          
    ;   % Case 2:  equal derivative
        has_transition( _, Derivative1, _, Tr1), 
        has_transition( _, Derivative2, _, Tr2),
        nonvar(Derivative1),
        nonvar(Derivative2),
        Derivative1 = Derivative2,
        ModVal = 0.5
    ).

% Two-argument version for backward compatibility
inverse_transitions(Tr1, Tr2) :-
    similar_transitions(Tr1, Tr2, _).

inverse_transitions(Tr1, Tr2, ModVal):-
    \+ rdf_equivalence(Tr1, Tr2), % Ensure they are distinct URIs 
    hasTransition(PreSitT1, Derivative1, PostSitT1, Tr1),
    hasTransition(PreSitT2, Derivative2, PostSitT2, Tr2),
    (
        rdf_equivalence(PreSitT1, PostSitT2),
        rdf_equivalence(PreSitT2, PostSitT1),
        Derivative1 \= Derivative2,  % Ensure derivatives are distinct or not present
        ModVal = 1, !  
    ;
        rdf_equivalence(PreSitT1, PreSitT2),
        nonvar(Derivative1),
        nonvar(Derivative2),
        Derivative1 \= Derivative2,
        ModVal = 0.75, !    
    ;
        nonvar(Derivative1),
        nonvar(Derivative2),
        Derivative1 \= Derivative2,
        ModVal = 0.5, ! 
    ).


% Check if Modifier1 is greater than Modifier2
is_greater_modifier_than(Modifier1, Modifier2) :-
    ground(Modifier1),                           % Ensure Modifier1 is instantiated
    ground(Modifier2),                           % Ensure Modifier2 is instantiated
    rdfs_individual_of(Modifier1, vocab:'Modifier'),  % Verify Modifier1 is of the correct type
    rdfs_individual_of(Modifier2, vocab:'Modifier'),  % Verify Modifier2 is of the correct type
    dif(Modifier1, Modifier2),                   % Ensure they are distinct
    rdf_reachable(Modifier1, vocab:hasHigherOrderThan, Modifier2).  % Check the hierarchy

% Check if Modifier1 is greater than or equal to Modifier2
is_greater_or_eq_modifier_than(Modifier1, Modifier2) :-
    ground(Modifier1),                           % Ensure Modifier1 is instantiated
    ground(Modifier2),                           % Ensure Modifier2 is instantiated
    rdfs_individual_of(Modifier1, vocab:'Modifier'),  % Verify Modifier1 is of the correct type
    rdfs_individual_of(Modifier2, vocab:'Modifier'),  % Verify Modifier2 is of the correct type
    (   Modifier1 = Modifier2, !
    ;   rdf_reachable(Modifier1, vocab:hasEqOrder, Modifier2)   % Case 2: Equal order
    ;   rdf_reachable(Modifier1, vocab:hasHigherOrderThan, Modifier2)  % Case 1: Greater than  
    ).
