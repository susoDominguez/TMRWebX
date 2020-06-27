// Generated automatically by nearley, version 2.19.4
// http://github.com/Hardmath123/nearley
(function () {
function id(x) { return x[0]; }
 const moo = require('moo')

    let lexer = moo.compile({
	  comma: ',',
      lbracket:  '[',
      rbracket:  ']',
	  lparenthesis: '(',
	  rparenthesis: ')',
      keyword: ["http://anonymous.org/data/RepeatedActionRec", 'http://anonymous.org/data/AlternativeActionsRec', 'http://anonymous.org/data/ReparableTransitionRec', 'http://anonymous.org/data/ContradictionRec'],
	  repeated: {match: 'Repeated Action', value: _ => 'repeated'},
	  alternative: {match: 'Alternative Actions', value: _ => 'alternative'},
	  repairable: {match: 'Repairable Transition', value: _ => 'repairable'},
	  contradictory: {match: 'Contradictory Norms', value: _ => 'contradiction'},
	  recURI: 'http://anonymous.org/data/Rec',
	  startStr: 'interaction',
      stringPlus: /[A-Z]?[a-zA-Z0-9-]+/
    });
var grammar = {
    Lexer: lexer,
    ParserRules: [
    {"name": "main$ebnf$1", "symbols": [/[\s]/], "postprocess": id},
    {"name": "main$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "main", "symbols": [(lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "main$ebnf$1", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(d) { return '[]';  }},
    {"name": "main", "symbols": [(lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "intResultList", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(d) { return d[1]; }},
    {"name": "intResultList", "symbols": ["intResult", (lexer.has("comma") ? {type: "comma"} : comma), "intResultList"], "postprocess": d => { return [].concat(d[0],d[2]);}},
    {"name": "intResultList", "symbols": ["intResult"]},
    {"name": "intResult", "symbols": ["begin", (lexer.has("comma") ? {type: "comma"} : comma), "intType", (lexer.has("comma") ? {type: "comma"} : comma), (lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "recList", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket), "end"], "postprocess": d => { return { 'type': d[2], 'interactionNorms': d[5] }; }},
    {"name": "intType", "symbols": [(lexer.has("repeated") ? {type: "repeated"} : repeated)], "postprocess": _ => { return 'repeated';}},
    {"name": "intType", "symbols": [(lexer.has("alternative") ? {type: "alternative"} : alternative)], "postprocess": _ => { return 'alternative';}},
    {"name": "intType", "symbols": [(lexer.has("contradictory") ? {type: "contradictory"} : contradictory)], "postprocess": _ => { return 'repairable';}},
    {"name": "intType", "symbols": [(lexer.has("repairable") ? {type: "repairable"} : repairable)], "postprocess": _ => { return 'contradiction';}},
    {"name": "recList", "symbols": ["recURI", (lexer.has("comma") ? {type: "comma"} : comma), "recList"], "postprocess": d => { return [].concat(d[0],d[2]);}},
    {"name": "recList", "symbols": ["recURI"]},
    {"name": "recURI", "symbols": [(lexer.has("recURI") ? {type: "recURI"} : recURI), (lexer.has("stringPlus") ? {type: "stringPlus"} : stringPlus)], "postprocess": d => { return { 'recId': d.join('') , 'type': 'primary' }; }},
    {"name": "begin", "symbols": [(lexer.has("startStr") ? {type: "startStr"} : startStr), (lexer.has("lparenthesis") ? {type: "lparenthesis"} : lparenthesis), (lexer.has("keyword") ? {type: "keyword"} : keyword), (lexer.has("stringPlus") ? {type: "stringPlus"} : stringPlus)], "postprocess": _ => { return null; }},
    {"name": "end", "symbols": [(lexer.has("comma") ? {type: "comma"} : comma), (lexer.has("lbracket") ? {type: "lbracket"} : lbracket), (lexer.has("rbracket") ? {type: "rbracket"} : rbracket), (lexer.has("rparenthesis") ? {type: "rparenthesis"} : rparenthesis)], "postprocess": _ => { return null; }}
]
  , ParserStart: "main"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();
