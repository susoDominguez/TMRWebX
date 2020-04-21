main -> "[" _ (intResultList):? _ "]" {% d => {return (Array.isArray(d[2])? d[2][0] : d[2]);} %}
intResultList ->  intResult 
	| intResultList comma intResult {% d => { return d[0].concat(d[2]); }%}
intResult -> "interaction(" _ intIdentifier comma intTypeSentence comma recList ",[])" 
	{% d => { return { 'type': d[4], 'interactionNorms': d[6] }; } %}
recList -> "[" _ recItem _ "]" {% d => { return d[2]; } %}
recItem ->  recUriJson
	| recItem comma recUriJson {% d => { return d[0].concat(d[2]); }%}
intIdentifier -> uriStart intType action recString recString {% d => { return d.join(""); }%}
recUriJson -> recUri  {% ([d]) => { return { 'recId': d , 'type': 'primary' }; } %}
recUri -> uriStart recString {% d => { return d.join(""); } %}
recString ->  "Rec" string "-" string {% d => { return d.join(""); } %}
string ->   [\w]:+ {% ([d]) => {return d.join("");} %}
intTypeSentence -> intType " " action {% d => { return d[0]; } %}
intType -> "Repeated" {% _ => "repetition"%}
	| "Alternative" {% _ => "alternative"%}
	| "Contradiction" {% _ => "contradiction"%}
	| "Reparable" {% _ => "repairable"%}
action -> "Action" [s]:? {% function(data) { return null } %}
uriStart -> "http://anonymous.org/data/" {% id %}
comma -> "," _
_ -> [\s]:*  {% function(data) { return null; } %}