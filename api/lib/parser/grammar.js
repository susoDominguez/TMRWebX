// Generated automatically by nearley, version 2.19.7
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
      repeated: {match: 'Repeated Action', value: _ => 'repetition'},
	  alternative: {match: 'Alternative Actions', value: _ => 'alternative'},
	  repairable: {match: 'Repairable Transition', value: _ => 'repairable'},
	  contradictory: {match: 'Contradictory Norms', value: _ => 'contradiction'},
	  recUriPrefix: 'http://anonymous.org/data/',
	  startStr: 'interaction',
      stringPlus: /[A-Z]?[a-zA-Z0-9-]+/
    });
var grammar = {
    Lexer: lexer,
    ParserRules: [
    {"name": "main$ebnf$1", "symbols": [/[\s]/], "postprocess": id},
    {"name": "main$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "main", "symbols": [(lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "main$ebnf$1", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(arr) { return '[]';  }},
    {"name": "main", "symbols": [(lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "interactionList", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": function(arr) { return arr[1]; }},
    {"name": "interactionList", "symbols": ["interaction", (lexer.has("comma") ? {type: "comma"} : comma), "interactionList"], "postprocess": arr => { return [].concat(arr[0],arr[2]);}},
    {"name": "interactionList", "symbols": ["interaction"]},
    {"name": "interaction", "symbols": [(lexer.has("startStr") ? {type: "startStr"} : startStr), (lexer.has("lparenthesis") ? {type: "lparenthesis"} : lparenthesis), "content", (lexer.has("rparenthesis") ? {type: "rparenthesis"} : rparenthesis)], "postprocess": arr => { return arr[2]; }},
    {"name": "content", "symbols": ["interactionIdentifier", (lexer.has("comma") ? {type: "comma"} : comma), "intType", (lexer.has("comma") ? {type: "comma"} : comma), (lexer.has("lbracket") ? {type: "lbracket"} : lbracket), "recList", (lexer.has("rbracket") ? {type: "rbracket"} : rbracket), "end"], "postprocess": arr => { return { 'type': arr[2], 'interactionNorms': arr[5] }; }},
    {"name": "interactionIdentifier", "symbols": [(lexer.has("recUriPrefix") ? {type: "recUriPrefix"} : recUriPrefix), (lexer.has("stringPlus") ? {type: "stringPlus"} : stringPlus)], "postprocess": _ => { return null; }},
    {"name": "intType", "symbols": [(lexer.has("repeated") ? {type: "repeated"} : repeated)], "postprocess": _ => { return 'repetition';}},
    {"name": "intType", "symbols": [(lexer.has("alternative") ? {type: "alternative"} : alternative)], "postprocess": _ => { return 'alternative';}},
    {"name": "intType", "symbols": [(lexer.has("contradictory") ? {type: "contradictory"} : contradictory)], "postprocess": _ => { return 'contradiction';}},
    {"name": "intType", "symbols": [(lexer.has("repairable") ? {type: "repairable"} : repairable)], "postprocess": _ => { return 'repairable';}},
    {"name": "recList", "symbols": ["recURi", (lexer.has("comma") ? {type: "comma"} : comma), "recList"], "postprocess": arr => { return [].concat(arr[0],arr[2]);}},
    {"name": "recList", "symbols": ["recURi"]},
    {"name": "recURi", "symbols": [(lexer.has("recUriPrefix") ? {type: "recUriPrefix"} : recUriPrefix), (lexer.has("stringPlus") ? {type: "stringPlus"} : stringPlus)], "postprocess": arr => { return { 'recId': arr.join('') , 'type': 'primary' }; }},
    {"name": "end", "symbols": [(lexer.has("comma") ? {type: "comma"} : comma), (lexer.has("lbracket") ? {type: "lbracket"} : lbracket), (lexer.has("rbracket") ? {type: "rbracket"} : rbracket)], "postprocess": _ => { return null; }}
]
  , ParserStart: "main"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();
