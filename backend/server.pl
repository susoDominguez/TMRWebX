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
:- use_module(library(debug)).

/*:- thread_pool_create(compute, 5,
                      [ local(20000), global(100000), trail(50000),
                        backlog(5)
                      ]).
                    */

% Prefixes
:- rdf_prefix(data, 'http://anonymous.org/data/').
:- rdf_prefix(vocab, 'http://anonymous.org/vocab/').
:- rdf_prefix(vocab4i, 'http://anonymous.org/vocab4i/').
:- rdf_prefix(oa, 'http,//www.w3.org/ns/oa#').
:- rdf_prefix(prov, 'http://www.w3.org/ns/prov#').
:- rdf_prefix(nanopub, 'http://www.nanopub.org/nschema#').

%:- http_handler(root(guidelines), get_available_guidelines, []).
:- http_handler(root(interactions), show_interactions, []). %[spawn(compute) ]).
:- http_handler(root(drug), show_drug, []).
:- http_handler(root(drugeffects), show_drug_effects, []).

:- set_prolog_flag(color_term,false).

:- consult(setup).
:- consult(util).

server(Port) :-
  http_server(http_dispatch, [port(Port)]).

load_guideline_group(Dataset_id, Dataset_graph_id) :-
  getenv("FUSEKI_HOST_PORT", FUSEKI_HOST_PORT),
  atom_concat(FUSEKI_HOST_PORT, Dataset_id, MainGuidelinesPath),
  atom_concat('http://anonymous.org/', Dataset_id, Dataset_graph_id),
  rdf_load(MainGuidelinesPath, [format('nquads'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph(Dataset_graph_id)]).

show_interactions(Request) :-
  %debug(hello),
  loadOntologies,
  %debug(hello, 'Ontologies have been loaded', []),
  http_parameters(Request, [ guideline_id(Dataset_id, [ string ]) ]),
  %debug(hello, 'http_parameters ~p', [Dataset_id]),
  load_guideline_group(Dataset_id, Dataset_graph_id),
  %debug(hello, 'load_guideline_group ~p', [Dataset_graph_id]),
  inferInternalInteractions,
  format('Content-type: text/plain~n~n'),
  atom_concat('http://anonymous.org/data/', Dataset_id, CIG_URI), %TODO: switch to rdf_global_id.
  %debug(hello, 'inferInternalInteractions ~p', [CIG_URI]),
  guideline_recommendations(CIG_URI, Recommendations),
  %debug(hello, 'guideline_recommendations ~p', [Recommendations]),
  maplist(recommendation_term, Recommendations, _Terms),
  %debug(hello, 'recommendation_term ~p', _Terms),
  findall(interaction(Interaction,Label,Elems,External), interaction(Recommendations, Interaction, Label, Elems, External), Interactions),
  %debug(hello, 'findall interactions ~p', [Interactions]),
  print_list(Interactions),
  unloadOntologies,
  rdf_unload_graph(Dataset_graph_id).

/*
get_available_guidelines(_Request) :-
  directory_files('tmr/ontologies/guidelines', Files),
  format('Content-type: text/plain~n~n'),
  sort(Files, SortedFiles),
  remove_head(SortedFiles, SortedFilesWithoutHead),
  remove_head(SortedFilesWithoutHead, Guidelines),
  maplist(remove_file_extension, Guidelines, GuidelineNames),
  print_list(GuidelineNames).
*/

 show_drug(Request) :-
  loadOntologies(),
  http_parameters(Request, [ recommendation_uri(Rec_URI, [ string ]), s(Dataset_id, [ string ]) ]),
  load_guideline_group(Dataset_id, Dataset_graph_id),
  format('Content-type: text/plain~n~n', []),
  atom_string(Rec_URI_atom, Rec_URI),
  rdf(Rec_URI_atom, vocab:'aboutExecutionOf', DrugAdministration),
  rdf(DrugAdministration, vocab:'administrationOf', Drug),
  format(Drug),
  rdf_unload_graph(Dataset_graph_id),
  unloadOntologies().

show_drug_effects(Request) :-
  loadOntologies(),
  http_parameters(Request, [ drug_URI(DrugID, [ string ]) ]),
  format('Content-type: text/plain~n~n', []),
  atom_string(DrugID_atom, DrugID),
  rdf(DrugAdministration, vocab:administrationOf, DrugID_atom),
  rdf(DrugAdministration, vocab:'causes', Transition),
  string_concat(' causes ', Transition, Join1), % `causes Tr`
  string_concat(DrugAdministration, Join1, Join2), %`DrugAdmin causes Tr`
  format(Join2),
  unloadOntologies().
