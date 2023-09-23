:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

% Prefixes
:- rdf_prefix(data, 'http://anonymous.org/data/').
:- rdf_prefix(vocab, 'http://anonymous.org/vocab/').
:- rdf_prefix(vocab4i, 'http://anonymous.org/vocab4i/').
:- rdf_prefix(oa, 'http,//www.w3.org/ns/oa#').
:- rdf_prefix(prov, 'http://www.w3.org/ns/prov#').
:- rdf_prefix(nanopub, 'http://www.nanopub.org/nschema#').

% Base ontologies.

loadBaseOntologies :-
  rdf_load('TMR-CIG-COPD/schema/model.ttl', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]),
  rdf_load('TMR-CIG-COPD/schema/model4I3.0.ttl', [format('turtle'), register_namespaces(false),base_uri('http://anonymous.org/vocab4i/'), graph('http://anonymous.org/vocab4i')]).


% User ontologies, at Jena endpoint.

loadOntologies :-
  getenv("FUSEKI_HOST_PORT", FUSEKI_HOST_PORT),
  atom_concat(FUSEKI_HOST_PORT, "careActions", FUSEKI_DRUGS_URL),
  rdf_load(FUSEKI_DRUGS_URL, [format('nquads'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CareAction&DrugTypes')]),
  atom_concat(FUSEKI_HOST_PORT, "transitions", FUSEKI_TRANSITIONS_URL),
  rdf_load(FUSEKI_TRANSITIONS_URL, [format('nquads'), register_namespaces(false),base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Transition&SituationTypes')]),
  atom_concat(FUSEKI_HOST_PORT, "beliefs", FUSEKI_BELIEFS_URL),
  rdf_load(FUSEKI_BELIEFS_URL, [format('nquads'), register_namespaces(false), base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CausationBeliefs-Nanopub')]).
  
unloadOntologies :-
  rdf_unload_graph('http://anonymous.org/CareAction&DrugTypes'),
  rdf_unload_graph('http://anonymous.org/Transition&SituationTypes'),
  rdf_unload_graph('http://anonymous.org/CausationBeliefs-Nanopub').


% Logic libraries
:- include('TMR-CIG-COPD/logic/interactionRules').
:- include('TMR-CIG-COPD/logic/interaction_graph').
