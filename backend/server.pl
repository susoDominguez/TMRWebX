:- module(server, [ server/1 ]).

:- use_module(library(http/thread_httpd)). % Multi-threaded server
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(thread_pool)).
:- use_module(library(http/json)).
:- use_module(library(strings)).

% Prefixes
:- use_module(current_logic/rdf_tmr_prefixes).

:- http_handler(root(interactions), show_interactions, []).
:- http_handler(root(interactions_json), show_interactions_json, []).
:- http_handler('/v1/interactions', show_interactions_json, []).

:- set_prolog_flag(color_term, false).

:- consult(util).
:- consult(setup).

% Preload ontologies at startup
:- initialization(preload_ontologies).

preload_ontologies :-
    rdf_reset_db, % Clear the database
    load_base_ontologies,
    load_ontologies.


% Start the HTTP server
server(Port) :-
    http_server(http_dispatch, [port(Port), workers(16)]). % Use 16 workers for concurrency

% Load a specific guideline group into memory
load_guideline_group(DatasetId, DatasetGraphId) :-
    getenv_or_default("FUSEKI_HOST_PORT", "http://localhost:3030/", RawHost),
    ensure_trailing_slash(RawHost, HostPort),
    atom_concat(HostPort, DatasetId, MainGuidelinesPath),
    atom_concat('http://anonymous.org/', DatasetId, DatasetGraphId),
    catch(
        rdf_load(MainGuidelinesPath, [
            format('nquads'),
            register_namespaces(false),
            base_uri('http://anonymous.org/data/'),
            graph(DatasetGraphId)
        ]),
        Error,
        (
            message_to_string(Error, Message),
            format(string(ResponseText), 'Failed to load guideline ~w (~w): ~w', [DatasetId, MainGuidelinesPath, Message]),
            throw(http_reply(not_found(ResponseText)))
        )
    ).

extract_guideline_id(Request, GuidelineId) :-
    (   catch(http_parameters(Request, [guideline_id(Value, [atom])]), _, fail)
    ->  normalize_guideline_id(Value, GuidelineId)
    ;   (   memberchk(method(Method), Request)
        ->  true
        ;   Method = get
        ),
        memberchk(Method, [post, put, patch]),
        extract_guideline_id_from_body(Request, GuidelineId)
    ;   throw(http_reply(bad_request('guideline_id parameter is required')))
    ).

extract_guideline_id_from_body(Request, GuidelineId) :-
    (   memberchk(content_type(ContentType), Request),
        sub_atom(ContentType, 0, _, _, 'application/json')
    ->  http_read_json_dict(Request, Dict, [json_object(dict)]),
        extract_guideline_from_json(Dict, RawValue)
    ;   http_read_data(Request, Data, [form_data(true)]),
        (   memberchk(guideline_id=RawValue, Data)
        ->  true
        ;   memberchk(data=RawValue, Data)
        )
    ),
    !,
    normalize_guideline_id(RawValue, GuidelineId).
extract_guideline_id_from_body(_, _) :-
    throw(http_reply(bad_request('Missing guideline identifier in request body'))).

extract_guideline_from_json(Dict, Value) :-
    (   get_dict(guideline_id, Dict, GuidelineValue)
    ->  Value = GuidelineValue
    ;   get_dict(data, Dict, DataField)
    ->  (   is_dict(DataField)
        ->  get_dict(guideline_id, DataField, Value)
        ;   Value = DataField
        )
    ;   throw(http_reply(bad_request('Missing guideline identifier in JSON payload')))
    ).

normalize_guideline_id(Value, GuidelineId) :-
    (   string(Value)
    ->  RawString = Value
    ;   atom(Value)
    ->  atom_string(Value, RawString)
    ;   number(Value)
    ->  number_string(Value, RawString)
    ;   throw(http_reply(bad_request('Invalid guideline_id value type')))
    ),
    normalize_space(string(Trimmed), RawString),
    (   Trimmed == ""
    ->  throw(http_reply(bad_request('guideline_id cannot be empty')))
    ;   atom_string(GuidelineId, Trimmed)
    ).

handle_interaction_error(json, Error) :-
    message_to_string(Error, Message),
    reply_json_dict(_{status: error, message: Message}, [status(500)]).
handle_interaction_error(text, Error) :-
    message_to_string(Error, Message),
    format('Status: 500 Internal Server Error~n'),
    format('Content-type: text/plain; charset=UTF-8~n~n'),
    format('~s', [Message]).

fetch_interactions(DatasetId, Interactions) :-
    rdf_transaction(
        (
            load_guideline_group(DatasetId, _DatasetGraphId),
            inferInternalInteractions,
            atom_concat('http://anonymous.org/data/', DatasetId, CigUri),
            guideline_recommendations(CigUri, Recommendations),
            findall(
                interaction(Interaction, Label, Elems, External),
                interaction(Recommendations, Interaction, Label, Elems, External),
                Interactions
            )
        ),
        [snapshot(true)]
    ).

interactions_to_json([], []).
interactions_to_json(
    [interaction(Interaction, Label, Elems, External) | Rest],
    [_{interaction: Interaction, label: Label, elements: Elems, external: External} | JsonRest]
) :-
    interactions_to_json(Rest, JsonRest).

show_interactions(Request) :-
    (   catch(
            (
                extract_guideline_id(Request, DatasetId),
                fetch_interactions(DatasetId, Interactions),
                format('Content-type: text/plain~n~n'),
                print_list(Interactions)
            ),
            Error,
            (handle_interaction_error(text, Error), fail)
        )
    ->  true
    ;   true
    ).

show_interactions_json(Request) :-
    (   catch(
            (
                extract_guideline_id(Request, DatasetId),
                fetch_interactions(DatasetId, Interactions),
                interactions_to_json(Interactions, JsonInteractions),
                format('Content-type: application/json; charset=UTF-8~n~n'),
                reply_json_dict(_{interactions: JsonInteractions})
            ),
            Error,
            (handle_interaction_error(json, Error), fail)
        )
    ->  true
    ;   true
    ).

