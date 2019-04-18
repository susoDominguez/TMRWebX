:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

:- rdf_prefix(vocab, 'http://anonymous.org/vocab/').

:- http_handler(root(guidelines), get_available_guidelines, []).
:- http_handler(root(interactions), show_interactions, []).
:- http_handler(root(drug), show_drug, []).
:- http_handler(root(drugeffects), show_drug_effects, []).

:- set_prolog_flag(color_term,false).

:- consult(setup).
:- consult(util).

server(Port) :-
  http_server(http_dispatch, [port(Port)]).

get_available_guidelines(_Request) :-
  directory_files('tmr/ontologies/guidelines', Files),
  format('Content-type: text/plain~n~n'),
  sort(Files, SortedFiles),
  remove_head(SortedFiles, SortedFilesWithoutHead),
  remove_head(SortedFilesWithoutHead, Guidelines),
  maplist(remove_file_extension, Guidelines, GuidelineNames),
  print_list(GuidelineNames).

load_guideline_group(GuidelineGroupID, GuidelinesGraphPath) :-
  getenv("FUSEKI_HOST_PORT", FUSEKI_HOST_PORT),
  atom_concat(FUSEKI_HOST_PORT, GuidelineGroupID, MainGuidelinesPath),
  atom_concat('http://anonymous.org/', GuidelineGroupID, GuidelinesGraphPath),
  rdf_load(MainGuidelinesPath, [format('nquads'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph(GuidelinesGraphPath)]).

show_interactions(Request) :-
  loadOntologies(),
  http_parameters(Request, [ guideline_group_id(GuidelineGroupID, [ string ]) ]),
  load_guideline_group(GuidelineGroupID, GuidelinesGraphPath),
  inferInternalInteractions,
  format('Content-type: text/plain~n~n'),
  atom_concat('http://anonymous.org/data/', GuidelineGroupID, DataGuidelineGroupID), %TODO: switch to rdf_global_id.
  guideline_recommendations(DataGuidelineGroupID, Recommendations),
  maplist(recommendation_term, Recommendations, Terms),
  findall(interaction(Interaction,Label,Elems,External), interaction(Recommendations, Interaction, Label, Elems, External), Interactions),
  print_list(Interactions),
  unloadOntologies(),
  rdf_unload_graph(GuidelinesGraphPath).


show_drug(Request) :-
  loadOntologies(),
  http_parameters(Request, [ guideline_id(GuidelineID, [ string ]), guideline_group_id(GuidelineGroupID, [ string ]) ]),
  load_guideline_group(GuidelineGroupID, GuidelinesGraphPath),
  format('Content-type: text/plain~n~n', []),
  atom_string(GuidelineID_atom, GuidelineID),
  rdf(GuidelineID_atom, vocab:'aboutExecutionOf', DrugAdministration),
  rdf(DrugAdministration, vocab:'administrationOf', Drug),
  format(Drug),
  rdf_unload_graph(GuidelinesGraphPath),
  unloadOntologies().

show_drug_effects(Request) :-
  loadOntologies(),
  http_parameters(Request, [ drug_full_id(DrugID, [ string ]) ]),
  format('Content-type: text/plain~n~n', []),
  atom_string(DrugID_atom, DrugID),
  rdf(DrugAdministration, vocab:administrationOf, DrugID_atom),
  rdf(DrugAdministration, vocab:'causes', Transition),
  string_concat(' causes ', Transition, Join1),
  string_concat(DrugAdministration, Join1, Join2),
  format(Join2),
  unloadOntologies().
