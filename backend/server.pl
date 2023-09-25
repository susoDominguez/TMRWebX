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
              

% Prefixes
:- rdf_prefix(data, 'http://anonymous.org/tmr/data/').
:- rdf_prefix(tmr, 'http://anonymous.org/tmr/').
:- rdf_prefix(tmr4i, 'http://anonymous.org/tmr4i/').
:- rdf_prefix(oa, 'http,//www.w3.org/ns/oa#').
:- rdf_prefix(prov, 'http://www.w3.org/ns/prov#').
:- rdf_prefix(nanopub, 'http://www.nanopub.org/nschema#').

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
  rdf_load(MainGuidelinesPath, [format('nquads'), register_namespaces(false), base_uri('http://anonymous.org/tmr/data/'), graph(Dataset_graph_id)]).

show_interactions(Request) :-
  rdf_reset_db,
  loadBaseOntologies,
  loadOntologies,
  http_parameters(Request, [ guideline_id(Dataset_id, [ string ]) ]),
  load_guideline_group(Dataset_id, Dataset_graph_id),
  inferInternalInteractions,
  format('Content-type: text/plain~n~n'),
  atom_concat('http://anonymous.org/tmr/data/', Dataset_id, CIG_URI), %TODO: switch to rdf_global_id.
  guideline_recommendations(CIG_URI, Recommendations),
  maplist(recommendation_term, Recommendations, _Terms),
  findall(interaction(Interaction,Label,Elems,External), interaction(Recommendations, Interaction, Label, Elems, External), Interactions),
  print_list(Interactions).
  
  % These functions below dont work becuase they only remove the named graph not the whole dataset
  % unloadOntologies,     
  % rdf_unload_graph(Dataset_graph_id).
