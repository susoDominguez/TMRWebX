:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(current_logic/rdf_tmr_prefixes).


% Base ontologies.

load_base_ontologies :-
  rdf_load('current_logic/tmr_concepts.trig', [format('trig'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]),
  rdf_load('current_logic/interactions_tmr4i.trig', [format('trig'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]).

% User ontologies, at Jena endpoint.
load_ontologies :-
  getenv("FUSEKI_HOST_PORT", FUSEKI_HOST_PORT),
  atom_concat(FUSEKI_HOST_PORT, "careActions", FUSEKI_DRUGS_URL),
  rdf_load(FUSEKI_DRUGS_URL, [format('nquads'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CareAction&DrugTypes')]),
  atom_concat(FUSEKI_HOST_PORT, "transitions", FUSEKI_TRANSITIONS_URL),
  rdf_load(FUSEKI_TRANSITIONS_URL, [format('nquads'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Transition&SituationTypes')]),
  atom_concat(FUSEKI_HOST_PORT, "beliefs", FUSEKI_BELIEFS_URL),
  rdf_load(FUSEKI_BELIEFS_URL, [format('nquads'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CausationBeliefs-Nanopub')]).
 % atom_concat(FUSEKI_HOST_PORT, "statements", FUSEKI_STATEMENTS_URL),
  %rdf_load(FUSEKI_STATEMENTS_URL, [format('nquads'), register_namespaces(false), base_uri('http://anonymous.org/tmr/data/'), graph('http://anonymous.org/Statements-Nanopub')]).

unload_ontologies :-
  rdf_unload_graph('http://anonymous.org/CareAction&DrugTypes'),
  rdf_unload_graph('http://anonymous.org/Transition&SituationTypes'),
  rdf_unload_graph('http://anonymous.org/CausationBeliefs-Nanopub').
  %rdf_unload_graph('http://anonymous.org/Statements-Nanopub').


% Logic libraries
- include('current_logic/main').
%:- include('TMR-CIG-COPD/logic/interactionRules').
%:- include('TMR-CIG-COPD/logic/interaction_graph').
