@{% const moo = require('moo')

    let lexer = moo.compile({
	  comma: ',',
      lbracket:  '[',
      rbracket:  ']',
	  lparenthesis: '(',
	  rparenthesis: ')',
      repeated: {match: 'Repeated Action', value: _ => 'repetition'},
	  alternative: {match: 'Alternative Actions', value: _ => 'alternative'},
	  repairable: {match: 'Repairable Transition', value: _ => 'repairable'},
	  contradictory: {match: 'Contradictory Norms', value: _ => 'contradiction'},
	  recUriPrefix: 'http://anonymous.org/data/',
	  startStr: 'interaction',
      stringPlus: /[A-Z]?[a-zA-Z0-9-]+/
    });
%}

@lexer lexer

main -> %lbracket [\s]:? %rbracket {% function(arr) { return '[]';  } %}
       | %lbracket  interactionList  %rbracket  {% function(arr) { return arr[1]; } %}
interactionList ->  interaction %comma interactionList  {% arr => { return [].concat(arr[0],arr[2]);}%} 
               |  interaction 		 		
interaction -> %startStr %lparenthesis  content %rparenthesis {% arr => { return arr[2]; } %}
content -> interactionIdentifier %comma intType %comma  %lbracket recList %rbracket end {% arr => { return { 'type': arr[2], 'interactionNorms': arr[5] }; } %}
interactionIdentifier ->  %recUriPrefix %stringPlus {% _ => { return null; } %}
intType ->  %repeated  {% _ => { return 'repetition';} %}
        | %alternative {% _ => { return 'alternative';} %}
        | %contradictory {% _ => { return 'contradiction';} %}
        | %repairable {% _ => { return 'repairable';} %}
recList ->  recURi %comma recList  {% arr => { return [].concat(arr[0],arr[2]);} %} 
        | recURi
recURi -> %recUriPrefix  %stringPlus  {% arr => { return { 'recId': arr.join('') , 'type': 'primary' }; } %}
end -> %comma %lbracket %rbracket {% _ => { return null; } %}