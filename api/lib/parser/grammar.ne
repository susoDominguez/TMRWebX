@{% const moo = require('moo')

    let lexer = moo.compile({
	  comma: ',',
      lbracket:  '[',
      rbracket:  ']',
	  lparenthesis: '(',
	  rparenthesis: ')',
      keyword: ["http://anonymous.org/data/RepeatedActionRec", 'http://anonymous.org/data/AlternativeActionsRec', 'http://anonymous.org/data/ReparableTransitionRec', 'http://anonymous.org/data/ContradictionRec'],
	  repeated: {match: 'Repeated Action', value: _ => 'repetition'},
	  alternative: {match: 'Alternative Actions', value: _ => 'alternative'},
	  repairable: {match: 'Repairable Transition', value: _ => 'repairable'},
	  contradictory: {match: 'Contradictory Norms', value: _ => 'contradiction'},
	  recURI: 'http://anonymous.org/data/Rec',
	  startStr: 'interaction',
      stringPlus: /[A-Z]?[a-zA-Z0-9-]+/
    });
%}

@lexer lexer

main -> %lbracket  [\s]:? %rbracket {% function(d) { return '[]';  } %}
       | %lbracket  intResultList  %rbracket  {% function(d) { return d[1]; } %}
intResultList ->  intResult %comma intResultList  {% d => { return [].concat(d[0],d[2]);}%} 
               |  intResult 		 		
intResult -> begin %comma intType %comma %lbracket recList %rbracket end {% d => { return { 'type': d[2], 'interactionNorms': d[5] }; } %}
intType ->  %repeated  {% _ => { return 'repetition';} %}
        | %alternative {% _ => { return 'alternative';} %}
        | %contradictory {% _ => { return 'contradiction';} %}
        | %repairable {% _ => { return 'repairable';} %}
recList ->  recURI %comma recList  {% d => { return [].concat(d[0],d[2]);} %} 
        | recURI 		  
recURI -> %recURI  %stringPlus  {% d => { return { 'recId': d.join('') , 'type': 'primary' }; } %}
begin -> %startStr %lparenthesis %keyword %stringPlus {% _ => { return null; } %}
end -> %comma %lbracket %rbracket %rparenthesis  {% _ => { return null; } %}