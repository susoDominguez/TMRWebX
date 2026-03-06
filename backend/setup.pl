%:-module(setup,[load_base_ontologies/0, load_ontologies/0, unload_ontologies/0]).

:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- ensure_loaded(util).

:- use_module(current_logic/rdf_tmr_prefixes).

resolve_backend_file(Relative, Absolute) :-
    absolute_file_name(Relative, Absolute, [
        access(read),
        file_errors(fail)
    ]).

log_backend_error(Message, Args) :-
    format(user_error, '[TMR backend] ERROR: ', []),
    format(user_error, Message, Args),
    nl(user_error).

log_backend_info(Message, Args) :-
    format(user_error, '[TMR backend] INFO: ', []),
    format(user_error, Message, Args),
    nl(user_error).

load_local_ontology(File, Graph) :-
    resolve_backend_file(File, Absolute),
    log_backend_info('Loading ontology ~w into graph ~w', [Absolute, Graph]),
    rdf_load(Absolute, [
        format('trig'),
        register_namespaces(false),
        base_uri('http://anonymous.org/vocab/'),
        graph(Graph)
    ]).

% Base Ontologies: Local Ontologies Loaded at Startup
load_base_ontologies :-
    forall(
        member(File-Graph, [
            ('current_logic/tmr_concepts.trig'-'http://anonymous.org/vocab'),
            ('current_logic/tmr4i.trig'-'http://anonymous.org/vocab')
        ]),
        catch(
            load_local_ontology(File, Graph),
            Error,
            (
                log_backend_error('Failed to load local ontology ~w: ~w', [File, Error]),
                throw(Error)
            )
        )
    ).

% Load User Ontologies from Fuseki Endpoint
load_ontologies :-
    getenv_or_default("FUSEKI_HOST_PORT", "http://localhost:3030/", RawHost),
    ensure_trailing_slash(RawHost, FUSEKI_HOST_PORT),
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
            log_backend_info('Loading remote ontology from ~w into graph ~w', [FusekiURL, Graph]),
            catch(
                rdf_load(FusekiURL, [
                    format('nquads'),
                    register_namespaces(false),
                    base_uri('http://anonymous.org/data/'),
                    graph(Graph)
                ]),
                Error,
                (
                    log_backend_error('Failed to load remote ontology (~w): ~w', [FusekiURL, Error]),
                    throw(Error)
                )
            )
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
