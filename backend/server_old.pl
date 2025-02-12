:- module(server,
      [ server/1            % ?Port
      ]).

:- use_module(library(http/thread_httpd)). %multi-threaded server
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

:- use_module(current_logic/rdf_tmr_prefixes).

:- http_handler(root(interactions), show_interactions, []). %[spawn(compute) ]).

:- set_prolog_flag(color_term,false).

:- consult(setup).
:- consult(util).

server(Port) :-
  http_server(http_dispatch, [port(Port), workers(16)]).

load_guideline_group(Dataset_id, Dataset_graph_id) :-
  getenv("FUSEKI_HOST_PORT", FUSEKI_HOST_PORT),
  atom_concat(FUSEKI_HOST_PORT, Dataset_id, MainGuidelinesPath),
  atom_concat('http://anonymous.org/', Dataset_id, Dataset_graph_id),
  rdf_load(MainGuidelinesPath, [format('nquads'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph(Dataset_graph_id)]).

show_interactions(Request) :-
  rdf_reset_db,
  load_base_ontologies,
  load_ontologies,
  http_parameters(Request, [ guideline_id(Dataset_id, [ string ]) ]),
  load_guideline_group(Dataset_id, Dataset_graph_id),
  inferInternalInteractions,
  format('Content-type: text/plain~n~n'),
  atom_concat('http://anonymous.org/data/', Dataset_id, CIG_URI), %TODO: switch to rdf_global_id.
  guideline_recommendations(CIG_URI, Recommendations),
  maplist(recommendation_term, Recommendations, _Terms),
  findall(interaction(Interaction,Label,Elems,External), interaction(Recommendations, Interaction, Label, Elems, External), Interactions),
  print_list(Interactions).

  show_interactions_json(Request) :-
    rdf_reset_db,
    load_base_ontologies,
    load_ontologies,
    http_parameters(Request, [ guideline_id(Dataset_id, [ string ]) ]),
    load_guideline_group(Dataset_id, Dataset_graph_id),
    inferInternalInteractions,
    format('Content-type: application/json; charset=UTF-8~n~n'),
    atom_concat('http://anonymous.org/data/', Dataset_id, CIG_URI), % TODO: switch to rdf_global_id.
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
    reply_json_dict(_{ interactions: InteractionList }).

  
  % These functions below dont work becuase they only remove the named graph not the whole dataset
  % unload_ontologies,     
  % rdf_unload_graph(Dataset_graph_id).
