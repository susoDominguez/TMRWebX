%:-module(setup,[load_base_ontologies/0, load_ontologies/0, unload_ontologies/0]).

:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

% Base Ontologies: Local Ontologies Loaded at Startup
load_base_ontologies :-
    forall(
        member(File-Graph, [
            ('current_logic/tmr_concepts.trig'-'http://anonymous.org/vocab'),
            ('current_logic/tmr4i.trig'-'http://anonymous.org/vocab')
        ]),
        (
            rdf_load(File, [
                format('trig'),
                register_namespaces(false),
                base_uri('http://anonymous.org/vocab/'),
                graph(Graph)
            ])
        )
    ).

% Load User Ontologies from Fuseki Endpoint
load_ontologies :-
    getenv("FUSEKI_HOST_PORT", FUSEKI_HOST_PORT), % Ensure the environment variable is set
    forall(
        member(Service-Graph, [
            ("careActions"-'http://anonymous.org/CareAction&DrugTypes'),
            ("transitions"-'http://anonymous.org/Transition&SituationTypes'),
            ("beliefs"-'http://anonymous.org/CausationBeliefs-Nanopub')
            % Uncomment if needed
            % ("statements"-'http://anonymous.org/Statements-Nanopub')
        ]),
        (
            atom_concat(FUSEKI_HOST_PORT, Service, FusekiURL),
            rdf_load(FusekiURL, [
                format('nquads'),
                register_namespaces(false),
                base_uri('http://anonymous.org/data/'),
                graph(Graph)
            ])
        )
    ).

% Unload User Ontologies
unload_ontologies :-
    forall(
        member(Graph, [
            'http://anonymous.org/CareAction&DrugTypes',
            'http://anonymous.org/Transition&SituationTypes',
            'http://anonymous.org/CausationBeliefs-Nanopub'
            % Uncomment if needed
            % 'http://anonymous.org/Statements-Nanopub'
        ]),
        rdf_unload_graph(Graph)
    ).

% Logic Libraries
%:- include('current_logic/main').
% :- include('TMR-CIG-COPD/logic/interactionRules').
% :- include('TMR-CIG-COPD/logic/interaction_graph').
