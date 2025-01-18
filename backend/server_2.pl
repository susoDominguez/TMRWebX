:- module(server,
      [ server/1            % ?Port
      ]).

:- use_module(library(http/thread_httpd)). % Multi-threaded server
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_error)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(thread_pool)).
:- use_module(library(http/json)).

% Prefixes
:- rdf_prefix(data, 'http://anonymous.org/data/').
:- rdf_prefix(vocab, 'http://anonymous.org/vocab/').
:- rdf_prefix(vocab4i, 'http://anonymous.org/vocab4i/').
:- rdf_prefix(oa, 'http://www.w3.org/ns/oa#').
:- rdf_prefix(prov, 'http://www.w3.org/ns/prov#').
:- rdf_prefix(nanopub, 'http://www.nanopub.org/nschema#').

:- http_handler(root(interactions), show_interactions, []).
:- http_handler(root(interactions_json), show_interactions_json, []).

:- set_prolog_flag(color_term, false).

:- consult(setup).
:- consult(util).

% Preload ontologies at startup
:- initialization(preload_ontologies).

preload_ontologies :-
    rdf_reset_db, % Clear the database
    loadBaseOntologies,
    loadOntologies.

% Start the HTTP server
server(Port) :-
    http_server(http_dispatch, [port(Port), workers(16)]). % Use 16 workers for concurrency

% Load a specific guideline group into memory
load_guideline_group(Dataset_id, Dataset_graph_id) :-
    getenv("FUSEKI_HOST_PORT", FUSEKI_HOST_PORT),
    atom_concat(FUSEKI_HOST_PORT, Dataset_id, MainGuidelinesPath),
    atom_concat('http://anonymous.org/', Dataset_id, Dataset_graph_id),
    rdf_load(MainGuidelinesPath, [format('nquads'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph(Dataset_graph_id)]).

% Show interactions as plain text
show_interactions(Request) :-
    rdf_transaction(
        (
            % Parse request parameters
            http_parameters(Request, [guideline_id(Dataset_id, [string])]),
            load_guideline_group(Dataset_id, Dataset_graph_id),
            inferInternalInteractions, % Temporary writes happen here for reasoning
            format('Content-type: text/plain~n~n'),
            atom_concat('http://anonymous.org/data/', Dataset_id, CIG_URI),
            guideline_recommendations(CIG_URI, Recommendations),
            findall(
                interaction(Interaction, Label, Elems, External),
                interaction(Recommendations, Interaction, Label, Elems, External),
                Interactions
            ),
            print_list(Interactions)
        ),
        [snapshot(true)] % Ensure all writes are temporary and read-only behavior is enforced
    ).

% Show interactions as JSON
show_interactions_json(Request) :-
    rdf_transaction(
        (
            % Parse request parameters
            http_parameters(Request, [guideline_id(Dataset_id, [string])]),
            load_guideline_group(Dataset_id, Dataset_graph_id),
            inferInternalInteractions, % Temporary writes happen here for reasoning
            format('Content-type: application/json; charset=UTF-8~n~n'),
            atom_concat('http://anonymous.org/data/', Dataset_id, CIG_URI),
            guideline_recommendations(CIG_URI, Recommendations),
            findall(
                _{
                    interaction: Interaction,
                    label: Label,
                    elements: Elems,
                    external: External
                },
                interaction(Recommendations, Interaction, Label, Elems, External),
                InteractionList
            ),
            reply_json_dict(_{interactions: InteractionList})
        ),
        [snapshot(true)] % Ensure all writes are temporary and read-only behavior is enforced
    ).

