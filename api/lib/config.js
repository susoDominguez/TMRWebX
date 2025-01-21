module.exports = {
  JENA_HOST: process.env.JENA_HOST || "localhost",
  JENA_PORT: process.env.JENA_PORT || "3030",
  PROLOG_HOST: process.env.PROLOG_HOST || "localhost",
  PROLOG_PORT: process.env.PROLOG_PORT || "1234",
  INSERT: "INSERT",
  DELETE: "DELETE",
  FUSEKI_PASSWORD: process.env.FUSEKI_PASSWORD || "road2h",
  FUSEKI_USER: process.env.FUSEKI_USER || "admin",
  PORT: process.env.PORT || "8888",
};

/*
SERVICE `+ actUrl + ` {
	?actAdmin a owl:NamedIndividual .
	?actAdmin a ?adminT .
	?actAdmin	?Of ?actId .
	?actAdmin rdfs:label ?adminLabel .
	?actId a owl:NamedIndividual .
	?actId a ?actType .
	?actId rdfs:label ?actLabel .
	FILTER (?actType != owl:NamedIndividual &&
		 (?Of = tmr:administrationOf || ?Of = tmr:applicationOf || ?Of = tmr:inoculationOf) &&
		 ?adminT != owl:NamedIndividual) .
}*/
