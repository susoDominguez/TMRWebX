
:- use_module(core_rules).
:- use_module(norm_rules).

:- use_module(rdf_prefixes).

:- use_module(library(semweb/rdf11)).        % Enhanced RDF querying and typed literals
:- use_module(library(semweb/rdf_db)).       % Core RDF database management
:- use_module(library(semweb/rdfs)).         % RDFS reasoning
:- use_module(library(dif)).                 % Declarative inequality constraints

rdf_db:rdf_reasoner(rdfs).
% rdf_db:rdf_reasoner(owl).  

%%	label(+Resource, -Label:atom)
label(R, Label) :-
    rdf(R, rdfs:label, literal(lang(_, Label))).

interaction_(I, Label, Elements) :-
    rdfs_individual_of(I, vocab4i:'Interaction'),
    rdf(I, rdf:type, Class),
    label(Class, Label),
    findall(E, interaction_element(I, E), Elements0),
    sort(Elements0, Elements).

% interaction_element(+Interaction, -Element)
% True if Element is related to Interaction or any semantically equivalent Interaction.
interaction_element(I, E) :-
    % Find a semantically equivalent interaction (including I itself).
    rdf_reachable(I, owl:sameAs, EquivalentI),
    % Check if the equivalent interaction relates to the element.
    rdf(EquivalentI, vocab4i:relates, E).

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

/*
is_safe_contribution_with_modal_str(Tr1,Cntrb2,Freq2,Tr2, ModVal):-
    is_greater_or_eq_modifier_than(Freq2, vocab:Sometimes),
    (
        rdf(Cntrb2, vocab:hasEqOrder, vocab:Negative),
        similarOrEqTrTypes(Tr1,Tr2,ModVal)
        -> true
    ;
        \+rdf(Cntrb2, vocab:hasEqOrder, vocab:Negative),
        inverse_transitionsModal(Tr1,Tr2,ModVal)
        -> true
    ;
    fail
    ).
       */


% check Contradiction
detect_contradiction :-
    %rdf_equal(vocab:Should, DeonticValUri1), %on top level it automatically converts prexi:local to fullURI 
    %rdf_equal(vocab:Should_not, DeonticValUri2),
    rdf_global_id(vocab4i:'Contradiction', IntTypeFullUri),
    %rdf_equal(vocab:Always, VarName),
    forall(
        ( 
          regulates(Reg, Norm1, Action1, Strength1, CB1),
     	  regulates(Reg, Norm2, Action2, Strength2, CB2),
          dif(Norm1,Norm2),
     	  causes(Action1, Tr1, ProbVal1, CB1),
     	  causes(Action2, Tr2, ProbVal2, CB2),
          is_greater_or_eq_modifier_than(Strength1, vocab:should),
          is_greater_or_eq_modifier_than(ProbVal2, vocab:sometimes),
     	  ( 
            (   is_greater_or_eq_modifier_than(Strength2, vocab:should),
                \+ has_relation_action_types(Action1, Action2),
                inverse_transitions(Tr1,Tr2)
            )   
            ;
            % negative contributions
            (   \+ is_greater_or_eq_modifier_than(Strength2, vocab:should),
                ( has_relation_action_types(Action1, Action2)
 	            ;
                 \+rdf_equivalence(Action1,Action2),
                 (rdf_equivalence(Tr1, Tr2) ; similar_transitions(Tr1, Tr2))
                )
            )
          )
        ),
            existsInteraction(IntTypeFullUri, Norm1, Norm2)
        ).

% check RepeatedAction
detect_repetition :-
        %rdf_equal(vocab:Should, DeonticValUriThreshold), %on top level it converts internally prexi:local to fullURI
        rdf_global_id(vocab4i:'RepeatedAction', IntTypeFullUri),
        forall(
                ( 
                dif(Norm1,Norm2),
                regulates(Reg, Norm1, Action1, Strength1),
                regulates(Reg, Norm2, Action2, Strength2),
                %split search space
                \+rdf(Norm2,vocab4i:comparedWith,Norm1,IntTypeFullUri),
                rdf_assert(Norm1,vocab4i:comparedWith,Norm2,IntTypeFullUri),
                %endOf split search space
                %assert deontic values are positive
                %is_greater_or_eq_modifier_than(should, DeonticValUriThreshold),
                %is_greater_or_eq_modifier_than(DeonticValUri2, DeonticValUriThreshold),
                once(has_relation_action_types(Action1, Action2))
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
        rdf_global_id(vocab4i:'AlternativeActions', IntTypeURI),
        %rdf_equal(vocab:Should, should),
        forall((
             regulates(Reg, Norm1, _, Strength1, CB1),
             regulates(Reg, Norm2, _, Strength2, CB2),
             dif(Norm1,Norm2),
             Strength1 \= vocab:never,
             Strength2 \= vocab:never,
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
        rdf_global_id(vocab4i:RepairableTransition, IntTypeFullUri),
        forall(
                (
                regulates(Reg, Norm1, Action1, Strength1),
                regulates(Reg, Norm2, Action2, should),
                dif(Norm1, Norm2),
                \+ is_greater_or_eq_modifier_than(Strength1, vocab:should),
                is_greater_or_eq_modifier_than(Strength1, vocab:should),
                \+ has_relation_action_types(Action1, Action2),
                basedOn(Norm1, CB1, Cntrb1),
                basedOn(Norm2, CB2, Cntrb2),
                causes(Action1, Tr1, ProbVal1, CB1),
                causes(Action2, Tr2, ProbVal2, CB2),
                is_greater_or_eq_modifier_than(ProbVal2, vocab:sometimes),
                inverse_transitions(Tr1, Tr2)
                ),
                    existsInteraction(IntTypeURI, Norm1, Norm2)
            ) .
    
    % infer the internal interactions for all the norms in a regulation


inferInternalInteractions :-
    detect_contradiction,
    detect_repetition,
    detect_alternative_actions,
    detect_repairable_transition.
