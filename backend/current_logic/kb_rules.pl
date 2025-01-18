:- use_module(library(semweb/turtle)).        % Turtle and TriG
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(dcg/basics)).
:- use_module(library(dif)).

rdf_db:rdf_reasoner(rdfs).
% rdf_db:rdf_reasoner(owl).


:- rdf_prefix(data, 'http://anonymous.org/data/').
:- rdf_prefix(vocab, 'http://anonymous.org/vocab/').
:- rdf_prefix(vocab4i, 'http://anonymous.org/vocab4i/').
:- rdf_prefix(oa, 'http://www.w3.org/ns/oa#'). 
:- rdf_prefix(prov, 'http://www.w3.org/ns/prov#').
:- rdf_prefix(nanopub, 'http://www.nanopub.org/nschema#').



% set RDF predicate properties
set_predicate_properties :-
    forall(member(Predicate, [
        vocab4i:'comparedWith'-[symmetric(true), transitive(false)],
        owl:'sameAs'-[symmetric(true), transitive(true)],
        vocab:'subsumes'-[transitive(true), symmetric(false)],
        vocab:'administrationOf'-[transitive(false), symmetric(false)],
        vocab:'hasComposition'-[transitive(true), symmetric(false)],
        vocab:'incompatibleWith'-[symmetric(true), transitive(false)],
        vocab:'hasEqOrder'-[symmetric(true), transitive(true)],
        vocab:'hasHigherOrderThan'-[transitive(true), symmetric(false)],
        vocab:'subsumedBy'-[transitive(true), symmetric(false), inverse_of(vocab:'subsumes')]
    ]),
    (   Predicate = Prop-[PropOptions],
        maplist(rdf_set_predicate(Prop), PropOptions)
    )).

:- set_predicate_properties.

% given an URI, create the predicate logic formula that represents 
% its precondition. 
create_formula(URI, Formula, Mapping) :-
    build_tree(URI, Tree), !,
    assign_prolog_vars(Tree, Formula, Mapping).

% if the URI is of type PredicateType, we are dealing with a leaf
% and we don't need to recurse any more
build_tree(URI, p(Label)) :-
    rdf(URI, rdf:type, vocab:'SituationType'),
    !,
    rdf(URI, rdfs:label, literal(Label)).
% if the URI has 'and' relationships, write a * in the tree and recurse
build_tree(URI, *(Parts)) :-
    rdf(URI, rdf:type, vocab:'PreconditionType'),
    rdf(URI, vocab:and, _),
    !,
    findall(Member, rdf(URI, vocab:and, Member), Members),
    maplist(build_tree, Members, Parts).
% if the URI has 'or' relationships, write a + in the tree and recurse
build_tree(URI, +(Parts)) :-
    rdf(URI, rdf:type, vocab:'PreconditionType'),
    rdf(URI, vocab:or, _),
    !,
    findall(Member, rdf(URI, vocab:or, Member), Members),
    maplist(build_tree, Members, Parts).
% if the URI has 'neg' relationships, write a ~ in the tree and recurse
build_tree(URI, ~(Part)) :-
    rdf(URI, rdf:type, vocab:'PreconditionType'),
    rdf(URI, vocab:neg, Member),
    !,
    build_tree(Member, Part).

%%%%%%%%%%
%TODO: Reg could be given by service to contextualize the interactions search

% this version of the predicate discards the CBs so no iteration of regulates occurs for each CB in Norm
regulates(Reg, Norm, ActionT, Strength):-
    rdfs_individual_of(Reg, vocab:'ClinicalGuideline'), %for each available CG. This must be filtered to the contextualized CG
    rdf(Norm, vocab:'isPartOf', Reg, Reg), % TODO: check this is true
    rdfs_individual_of(Norm, vocab:'ClinicalRecommendation'),
    rdf(Norm, vocab:'partOf', Reg, Norm), % potentially, this Reg could be distinct to the one above when combining guidelines by specifying a new isPartOf object
    rdf(Norm, vocab:'strength', Strength, Norm),
    rdf(Norm, vocab:'aboutExecutionOf', ActionT, Norm).

regulates(Reg, Norm, ActionT, Strength, CBelief):-
    regulates(Reg, Norm, ActionT, Strength),
    rdf(Norm, vocab:'basedOn', CBelief, Norm).

regulates(Reg, Norm, ActionT, Strength, CBelief, Contrib):-
    regulates(Reg, Norm, ActionT, Strength, CBelief),
    rdf(CBelief, vocab:contribution, Contrib, Norm).

/* *********************************** */

% a pair of care action types are related if they are the same syntactically or semantically, or if there is a subsumption relation
has_relation_action_types(Action1, Action2, Norm1, Norm2):-
    ( related_action_types(Action1, Action2) 
    ; subsumed_due_to_effect_symm(Action1, Action2, Norm1, Norm2) -> true 
    ).

related_action_types(Action1, Action2) :-
    nonvar(Action1),
    nonvar(Action2),
    ( rdf_equal(Action1, Action2) %T if same care action
    ; semanticallySameAs(Action1, Action2) 
    ; related_to_symm(Action1, Action2) 
    ).


subsumed_due_to_effect_symm(Action1, Action2, Norm1, Norm2) :-
    rdf(Action1, vocab:'administrationOf', DrugType1),
    rdfs_individual_of(DrugType1, vocab:'DrugCategory'),
    rdf(Action2, vocab:'administrationOf', DrugType2),
    rdfs_individual_of(DrugType2, vocab:'DrugCategory'),
    (   
        hasActiveGroupingCriterion(DrugType1,Action1,Norm1,Tr),
        hasActiveGroupingCriterion(DrugType2,Action2,Norm2,Tr)
        -> true
    ;
        hasActiveGroupingCriterion(DrugType2,Action2,Norm2,Tr),
        hasActiveGroupingCriterion(DrugType1,Action1,Norm1,Tr)
    ).

subsumed_due_to_effect(Action1, Action2, Norm1, Norm2) :-
        rdf(Action1, vocab:'administrationOf', DrugType1),
        rdfs_individual_of(DrugType1, vocab:'DrugCategory'),
        rdf(Action2, vocab:'administrationOf', DrugType2),
        rdfs_individual_of(DrugType2, vocab:'DrugCategory'),
        hasActiveGroupingCriterion(DrugType1,Action1,Norm1,Tr),
        hasActiveGroupingCriterion(DrugType2,Action2,Norm2,Tr).

%TODO: select CBs that are not side effects
hasActiveGroupingCriterion(DrugType,Action,Norm,Tr) :-
    rdf(DrugType, vocab:'hasGroupingCriteria', Tr),
    rdf(Norm, vocab:'basedOn', CBelief, Norm),
    causes(Action, Tr, _, CBelief).
    

%symmetric predicate to find out whether two given Action types have a relation (same, subsumes, hasComponent)
related_to_symm(Action1,Action2) :- 
    nonvar(Action1),
    nonvar(Action2),
    (   
       %if Action1 is related to Action2
        isRelatedTo(Action1, Action2) 
        -> true  %checks also for CareAction3 s.t. its owl:same as Action2
    ;   %if Action2 is related to Action1
        isRelatedTo(Action2, Action1) 
        -> true %checks also for CareAction3 s.t. its owl:same as Action1
    ;   %if Action1 is semantically the same as another careAction type 3 and the latter is related to careAction 2
        semanticallySameAs(Action1,CareAction3),
        isRelatedTo(CareAction3, Action2)
        -> true
    ;   %the symmetric version of above
        semanticallySameAs(Action2, CareAction3),
        isRelatedTo(CareAction3, Action1)
    ).

related_to(Action1,Action2) :- 
        nonvar(Action1),
        nonvar(Action2),
        (   
           %if Action1 is related to Action2
            isRelatedTo(Action1, Action2) 
            -> true  %checks also for CareAction3 s.t. its owl:same as Action2
        ;   
            %if Action1 is semantically the same as another careAction type 3 and the latter is related to careAction 2
            semanticallySameAs(Action1,CareAction3),
            isRelatedTo(CareAction3, Action2)
        ).

% check if a careAction is related, but not equal, to another via subsumption, hasComponent and the composition of both
isRelatedTo(Action1, Action2) :-
    nonvar(Action1),
    nonvar(Action2),
    rdf_not_equal(Action1, Action2), %related but not equal
    rdf(Action1, vocab:'administrationOf', DrugType1),
    rdf(Action2, vocab:'administrationOf', DrugType2),
    (   
        subsumes(Action1 , Action2) -> true  %by definition Action2 cannot be a Compound Drug Type so not followed by component property
    ;
        hasComponentRelation(DrugType1,DrugType2) -> true  %the property already handles owl:same
    ;   %below are the cases where some free variables are at play
        hasComponentRelation(DrugType1, DrugType3),
        rdf(CareAction3, vocab:'administrationOf', DrugType3),
            (
                subsumes(CareAction3, Action2) -> true %checks for owl:same on 2nd param
            ;
                semanticallySameAs(CareAction3, CareAction4),
                rdf_not_equal(CareAction4, Action1),
                subsumes(CareAction4, Action2)
            )
        
    ) .

% semanticallyDifferent(Resource1, Resource2) {CWA - Negation as failure}
% check if two resources are not the semantically the semanticallySameAs

/* *********************************** */
% semanticallySameAs(Resource1, Resource2)    {if nothing is said, they are not the semanticallySameAs}
% check if two resources are explicitly, and strictly, said to be the semanticallySameAs (semantic equivalence)
%owl:semanticallySameAs must be set as a symmetric predicate
semanticallySameAs(Resource1, Resource2) :-
    rdf_not_equal(Resource1, Resource2),
    rdf_reachable(Resource1, owl:sameAs, Resource2).

%TODO: rdf_not_equal must be removed since 2 identical resources are semantically different
semanticallyDifferent(Resource1, Resource2) :-
    \+ semanticallySameAs(Resource1, Resource2),
    rdf_not_equal(Resource1, Resource2). %This case is required for the transitive closure of owl:same as it succeeds with R1=R1.

rdf_not_equal(Resource1,Resource2) :-
    \+ rdf_equal(Resource1,Resource2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* *********************************** */
%  EVENT_TYPE-BASED RULES
/* *********************************** */
%

% TODO: improve with selection of reliable sources
%using the transitive closure of vocab:subsumes, find all objects that are subsumed.
%order of disjuncts matters
% subsumes(+SType, ?Type)
% SType subsumes Type if:
% 1. SType is non-variable (ground).
% 2. SType and Type are distinct.
% 3. Type is either directly reachable via vocab:subsumes,
%    semantically equivalent to a reachable type,
%    or can be unified through variable exploration.

subsumes(SType, Type) :-
    nonvar(SType),
    dif(SType, Type),
    (
        % Case 1: Direct subsumption
        nonvar(Type),
        rdf_reachable(SType, vocab:'subsumes', Type)
    ;
        % Case 2: Subsumption via semantic equivalence
        nonvar(Type),
        semanticallySameAs(Type, Type2),
        dif(SType, Type2),
        rdf_reachable(SType, vocab:'subsumes', Type2)
    ;
        % Case 3: Variable exploration to find all valid unifications
        var(Type),
        rdf_reachable(SType, vocab:'subsumes', Type2),
        dif(SType, Type2),
        (
            rdf_equal(Type2, Type)
        ;
            semanticallySameAs(Type2, Type)
        )
    ).


%Relation from combined Drug types to drug types
%apply sameAS to second parameter and sameAs for first parameter must be call from outside, as parameters of CType
hasComponentRelation(CType, Type):-
    nonvar(CType),
    rdfs_individual_of(CType, vocab:'CombinedDrugType'),
    (
        nonvar(Type),
        hasDirectComponent(CType,Type), ! % Cut to stop after direct match succeeds
    ;
        nonvar(Type), 
        semanticallySameAs(Type, AltType),
        hasDirectComponent(CType, AltType), ! % Cut to stop after direct match succeeds
    ;
        var(Type), %This case requires to find all possible unifications
        hasDirectComponent(CType, CandidateType),
        (
            rdf_equal(CandidateType, Type), ! 
        ;
            semanticallySameAs(CandidateType, Type)
        )     
    ).

%tests whether Type is a direct component of CType, or finds a direct component of CType
hasDirectComponent(CType,Type) :-
    rdf_reachable(CType, vocab:'hasComponent', Type),
    semanticallyDifferent(CType,Type).

similarOrEqTrTypes(Tr1,Tr2,_):-
       % var(ModVal),
        nonvar(Tr1),
        nonvar(Tr2),
        (
            rdf_equal(Tr1, Tr2) %,
           % ModVal = 1
            -> true
        ;
            semanticallySameAs(Tr1, Tr2) %,
            %ModVal = 1
            -> true 
        ;
            hasTransition(TropeT,PreSitT1,Derivative,PostSitT1,Tr1),
            hasTransition(TropeT,PreSitT2,Derivative,PostSitT2,Tr2),
                (
                rdf_equal(PreSit1, PreSit2)%,
                %ModVal = 1
                -> true
                ;
                semanticallySameAs(PreSit1, PreSit2) %,
                %ModVal = 1
                -> true
                %;
                %ModVal = 0.5
                %-> true
                ;
                fail
                )
            -> true
        ;
            fail
        ).

inverseTrTypesModal(Tr1, Tr2, ModVal):-
        var(ModVal),
        nonvar(Tr1),
        nonvar(Tr2),
        dif(Tr1, Tr2),
        semanticallyDifferent(Tr1, Tr2),
        hasTransition(TropeT,PreSitT1,Derivative1,PostSitT1,Tr1),
        hasTransition(TropeT,PreSitT2,Derivative2,PostSitT2,Tr2),
        \+rdf(Derivative1, vocab:hasEqOrder, Derivative2),
        (
            (
                rdf_equal(PreSitT1, PostSitT2)
                -> true
                ;
                semanticallySameAs(PreSitT1, PostSitT2)
                -> true
            ),
            (
                rdf_equal(PreSitT2, PostSitT1)
                -> true
                ;
                semanticallySameAs(PreSitT2, PostSitT1)
                -> true
            ),
            ModVal = 1
            -> true
        ;
            (
                rdf_equal(PreSitT1, PreSitT2)
                -> true
            ;
                semanticallySameAs(PreSitT1, PreSitT2)
                -> true
            ),
            ModVal = 1
            -> true
        ;
            %the most generic inverse Tr. Subsumes previous one
            ModVal = 0.5
            -> true
        ; 
            fail 
        ).

inverseTrTypes(Tr1, Tr2):-
    nonvar(Tr1),
    nonvar(Tr2),
    rdf_not_equal(Tr1, Tr2),
    semanticallyDifferent(Tr1, Tr2),
    hasTransition(TropeT,PreSitT1,Derivative1,PostSitT1,Tr1),
    hasTransition(TropeT,PreSitT2,Derivative2,PostSitT2,Tr2),
    %rdf_not_equal(Derivative1, Derivative2),
    Derivative1 \= Derivative2 ,
    (
        (
        rdf_equal(PreSitT1, PostSitT2)
        -> true
        ;
        semanticallySameAs(PreSitT1, PostSitT2)
        -> true
        ),
        (
        rdf_equal(PreSitT2, PostSitT1)
        -> true
        ;
        semanticallySameAs(PreSitT2, PostSitT1)
        -> true
        )
        -> true
    ;
            (
            rdf_equal(PreSitT1, PreSitT2)
            -> true
            ;
            semanticallySameAs(PreSitT1, PreSitT2)
            -> true
            ) 
            -> true
        %;
        %the most generic inverse Tr. Subsumes previous one
        %true
    ).

hasTransition(TropeT,PreSitT,Derivative,PostSitT,Tr):-
    rdfs_individual_of(Tr, vocab:'TransitionType'),
    rdf(Tr, vocab:affects, TropeT),
    rdf(Tr, vocab:derivative, Derivative),
    rdf(Tr, vocab:affects, TropeT),
    rdf(Tr, vocab:hasTransformableSituation, PreSitT),
    rdf(Tr, vocab:hasExpectedSituation, PostSitT).

is_greater_modifier_than(Modifier1, Modifier2):-
    nonvar(Modifier2),
    rdfs_individual_of(Modifier2, vocab:'Modifier'),
    dif(Modifier1, Modifier2),
    rdf_reachable(Modifier1, vocab:hasHigherOrderThan, Modifier2).

is_greater_or_eq_modifier_than(Modifier1, Modifier2):-
    ( 
      is_greater_modifier_than(Modifier1, Modifier2)
      -> true 
      ;
      nonvar(Modifier2),
      rdfs_individual_of(Modifier2, vocab:'Modifier'),
      rdf_reachable(Modifier1, vocab:hasEqOrder, Modifier2) 
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

%%	label(+Resource, -Label:atom)

label(R, Label) :-
    rdf(R, rdfs:label, literal(lang(_, Label))).



/* *********************************** */
%  BELIEF-BASED RULES
/* *********************************** */

causes(ActionT, Tr, Frequency, CBelief) :-
    rdfs_individual_of(CBelief, vocab:'CausationBelief'),
    rdf(CBelief, vocab:'frequency', Frequency, CBelief),
    rdf(ActionT, vocab:'causes', Tr, CBelief).
causes(ActionT, Tr, Frequency, CBelief, Source) :-
    causes(ActionT, Tr, Frequency, CBelief),
    rdf(CBelief, prov:'wasDerivedFrom', Source).

basedOn(Norm, CB, Cntrb):-
        nonvar(Norm),
        rdf(Norm, vocab:basedOn, CB, Norm),
        rdf(CB, vocab:contribution, Cntrb, Norm).

similarTo(CBelief1, CBelief2) :-
    rdfs_individual_of(CBelief1, vocab:'CausationBelief'),
    rdfs_individual_of(CBelief2, vocab:'CausationBelief'),
    causes(Action1, EventT3, Frequency1, CBelief1),
    %rdf_reachable(Frequency1, vocab:hasHigherOrderThan, vocab:'Rarely'),
    dif(Action1,Action2),
    causes(Action2, EventT4, Frequency2, CBelief2),
    %rdf_reachable(Frequency2, vocab:hasHigherOrderThan, vocab:'Rarely'),
    semanticallyDifferent(Action1, Action2),
    %not related by subsumption.
    \+subsumes(Action1,Action2),
    \+subsumes(Action2,Action1),
    \+inverseTrTypes(EventT3, EventT4).

incompatibleWith(EventT1, EventT2, IBelief) :-
    rdfs_individual_of(EventT1, vocab:'EventType'),
    rdfs_individual_of(EventT2, vocab:'EventType'),
    rdf(EventT1, vocab:'incompatibleWith', EventT2, IBelief).



interaction_(I, Label, Elements) :-
    rdfs_individual_of(I, vocab4i:'Interaction'),
    rdf(I, rdf:type, Class),
    label(Class, Label),
    findall(E, interaction_element(I, E), Elements0),
    sort(Elements0, Elements).

interaction_element(I, E) :-
    ( rdf_reachable(I, owl:sameAs, I2)
    ; 
    rdf_reachable(I2, owl:sameAs, I)),  
    rdf(I2, vocab4i:relates, E).

interaction(I, Label, Elements) :-
        distinct(Label-Elements, interaction_(I, Label, Elements)).


/* *********************************** */
%  INTERACTION-BASED RULES
/* *********************************** */ 

% Check if an interaction exist among two norms
interacts(IntTypeUri, Norm1, Norm2, Interaction) :-
    rdfs_individual_of(Interaction, IntTypeUri),
    rdf(Interaction, vocab4i:relates, Norm1),
    rdf(Interaction, vocab4i:relates, Norm2).

% Check if an interaction exist among two norms, caused by 2 CBs with a modal strength
interacts(IntTypeUri, Norm1, Norm2, CB1, CB2, Interaction) :-
    rdfs_individual_of(Interaction, IntTypeUri),
    rdf(Interaction, vocab4i:relates, Norm1),
    rdf(Interaction, vocab4i:relates, Norm2),
    rdf(Interaction, vocab4i:identifies, CB1),
    rdf(Interaction, vocab4i:identifies, CB2).


/* *********************************** */
% ** Assert an interaction of a certain type between the two recommendations
% just in case the interaction does not already exist
existsInteraction(IntTypeURI, Norm1, Norm2) :-
    (   interacts(IntTypeURI, Norm1, Norm2, _) 
        -> true
    ;
        % composing new URI for interaction
        rdfs_label(IntTypeURI, Label),
        split_string(Label, " ", "\s\t\n", [PrefixLbl|_]),
        atom_concat(PrefixLbl, #, Atom3),
        gensym(Atom3, NewID),
        rdf_global_id(data:NewID, NewURI),
        % asserting the interaction
        rdf_assert(NewURI, rdf:type, IntTypeURI, my_entailments),
        rdf_assert(NewURI, vocab4i:relates, Norm1, my_entailments),
        rdf_assert(NewURI, vocab4i:relates, Norm2, my_entailments)
    ).


/* *********************************** */
% ** Assert an interaction of a certain type between the two recommendations
% just in case the interaction does not already exist
existsInteraction(IntTypeURI, Norm1, Norm2, CB1, CB2, ModVal) :-
    (   interacts(IntTypeURI, Norm1, Norm2, CB1, CB2, _) 
        -> true
    ;
        % composing new URI for interaction
        rdfs_label(IntTypeURI, Label),
        split_string(Label, " ", "\s\t\n", [PrefixLbl|_]),
        atom_concat(PrefixLbl, #, Atom3),
        gensym(Atom3, NewID),
        rdf_global_id(data:NewID, NewURI),
        % asserting the interaction
        rdf_assert(NewURI, rdf:type, IntTypeURI, my_entailments),
        rdf_assert(NewURI, vocab4i:relates, Norm1, my_entailments),
        rdf_assert(NewURI, vocab4i:relates, Norm2, my_entailments),
        rdf_assert(NewURI, vocab4i:identifies, CB1, my_entailments),
        rdf_assert(NewURI, vocab4i:identifies, CB2, my_entailments)
        %rdf_assert(NewURI, vocab4i:hasModalStregth, ModVal, my_entailments)
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Interaction formulae
is_safe_contribution_with_modal_str(Tr1,Cntrb2,Freq2,Tr2, ModVal):-
    is_greater_or_eq_modifier_than(Freq2, vocab:'Sometimes'),
    (
        rdf(Cntrb2, vocab:hasEqOrder, vocab:'Negative'),
        similarOrEqTrTypes(Tr1,Tr2,ModVal)
        -> true
    ;
        \+rdf(Cntrb2, vocab:hasEqOrder, vocab:'Negative'),
        inverseTrTypesModal(Tr1,Tr2,ModVal)
        -> true
    ;
    fail
    ).
        
% check Contradiction
detect_contradiction :-
    %rdf_equal(vocab:'Should', DeonticValUri1), %on top level it automatically converts prexi:local to fullURI 
    %rdf_equal(vocab:'Should_not', DeonticValUri2),
    rdf_equal(vocab4i:'Contradiction', IntTypeFullUri),
    %rdf_equal(vocab:'Always', VarName),
    forall(
        ( 
          regulates(Reg, Norm1, Action1, 'should', CB1),
          dif(Norm1,Norm2),
     	  regulates(Reg, Norm2, Action2, Strength, CB2),
     	  causes(Action1, Tr1, 'always', CB1),
     	  causes(Action2, Tr2, 'always', CB2),
     	  ( 
            Strength = 'should',
            semanticallyDifferent(Action1,Action2),
            inverseTrTypes(Tr1,Tr2)
            -> true
            ;
            %shouldnot
            Strength = 'should-not',
            (
                related_action_types(Action1, Action2)
                -> true
 	        ;
                semanticallyDifferent(Action1,Action2),
                \+inverseTrTypes(Tr1,Tr2)
                -> true
            )
            -> true
          )
        ),
            existsInteraction(IntTypeFullUri, Norm1, Norm2)
        ).

% check RepeatedAction
detect_repetition :-
        %rdf_equal(vocab:'Should', DeonticValUriThreshold), %on top level it converts internally prexi:local to fullURI
        rdf_equal(vocab4i:'RepeatedAction', IntTypeFullUri),
        forall(
                ( 
                dif(Norm1,Norm2),
                regulates(Reg, Norm1, Action1, 'should'),
                regulates(Reg, Norm2, Action2, 'should'),
                %split search space
                \+rdf(Norm2,vocab4i:comparedWith,Norm1,IntTypeFullUri),
                rdf_assert(Norm1,vocab4i:comparedWith,Norm2,IntTypeFullUri),
                %endOf split search space
                %assert deontic values are positive
                %is_greater_or_eq_modifier_than(should, DeonticValUriThreshold),
                %is_greater_or_eq_modifier_than(DeonticValUri2, DeonticValUriThreshold),
                once(has_relation_action_types(Action1, Action2, Norm1, Norm2))
                ), 
                existsInteraction(IntTypeFullUri, Norm1, Norm2)
            ),
            % accumulate RepeatedAction
            forall(
                (
                interacts(IntTypeFullUri, Norm1, Norm2, I1),
                dif(Norm2,Norm3),
                interacts(IntTypeFullUri, Norm3, Norm1, I2)
                ),
                rdf_assert(I1, owl:sameAs, I2, my_entailments)
                ),
                %remove temp assertion for this int type
            rdf_retractall(_, vocab4i:comparedWith, _, IntTypeFullUri).
    
detect_alternative_actions :-
        %on top level it automatically converts prexi:local to fullURI
        rdf_equal(vocab4i:'AlternativeActions', IntTypeURI),
        %rdf_equal(vocab:'Should', should),
        forall((
             regulates(Reg, Norm1, _, 'should', CB1),
             dif(Norm1,Norm2),
             regulates(Reg, Norm2, _, 'should', CB2),
             similarTo(CB1, CB2) %includes diff(a1, a2) and diff(n1, n2).
         ),
         existsInteraction(IntTypeURI, Norm1, Norm2)),
        % accumulate AlternativeActions
        forall( (
            interacts(IntTypeURI, Norm1, Norm2, I1),
            dif(Norm1, Norm3),
            interacts(IntTypeURI, Norm2, Norm3, I2)
         ),
         rdf_assert(I1, owl:sameAs, I2, my_entailments) ).
    
detect_repairable_transition :-
        %on top level it automatically converts prexi:local to fullURI
        rdf_equal(vocab4i:'RepairableTransition', IntTypeFullUri),
        forall(
                (
                dif(Norm1, Norm2),
                regulates(Reg, Norm1, Action1, 'should-not'),
                regulates(Reg, Norm2, Action2, 'should'),
                %split search space
                %\+rdf(Norm2,vocab4i:comparedWith,Norm1,IntTypeFullUri),
                %rdf_assert(Norm1,vocab4i:comparedWith,Norm2,IntTypeFullUri),
                %endOf split search space
                semanticallyDifferent(Action1,Action2),
                %is_greater_or_eq_modifier_than(should, vocab:'Should'),
                %\+is_greater_or_eq_modifier_than(DeonticValUri2, vocab:'Should'),
                \+has_relation_action_types(Action1, Action2, Norm1, Norm2),
                basedOn(Norm1, CB1, Cntrb1),
                basedOn(Norm2, CB2, Cntrb2),
                causes(Action1, Tr1, 'always', CB1),
                causes(Action2, Tr2, 'always', CB2),
                inverseTrTypes(Tr1, Tr2)
                ),
                    existsInteraction(IntTypeURI, Norm1, Norm2)
            ) .
    
    % infer the internal interactions for all the norms in a regulation


inferInternalInteractions :-
    detect_contradiction,
    detect_repetition,
    detect_alternative_actions,
    detect_repairable_transition.
