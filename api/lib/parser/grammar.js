// Generated automatically by nearley, version 2.19.2
// http://github.com/Hardmath123/nearley
(function () {
function id(x) { return x[0]; }
var grammar = {
    Lexer: undefined,
    ParserRules: [
    {"name": "main$ebnf$1$subexpression$1", "symbols": ["intResultList"]},
    {"name": "main$ebnf$1", "symbols": ["main$ebnf$1$subexpression$1"], "postprocess": id},
    {"name": "main$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "main", "symbols": [{"literal":"["}, "_", "main$ebnf$1", "_", {"literal":"]"}], "postprocess": d => {return (Array.isArray(d[2])? d[2][0] : d[2]);}},
    {"name": "intResultList", "symbols": ["intResult"]},
    {"name": "intResultList", "symbols": ["intResultList", "comma", "intResult"], "postprocess": d => { return d[0].concat(d[2]); }},
    {"name": "intResult$string$1", "symbols": [{"literal":"i"}, {"literal":"n"}, {"literal":"t"}, {"literal":"e"}, {"literal":"r"}, {"literal":"a"}, {"literal":"c"}, {"literal":"t"}, {"literal":"i"}, {"literal":"o"}, {"literal":"n"}, {"literal":"("}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intResult$string$2", "symbols": [{"literal":","}, {"literal":"["}, {"literal":"]"}, {"literal":")"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intResult", "symbols": ["intResult$string$1", "_", "intIdentifier", "comma", "intTypeSentence", "comma", "recList", "intResult$string$2"], "postprocess": d => { return { 'type': d[4], 'interactionNorms': d[6] }; }},
    {"name": "recList", "symbols": [{"literal":"["}, "_", "recItem", "_", {"literal":"]"}], "postprocess": d => { return d[2]; }},
    {"name": "recItem", "symbols": ["recUriJson"]},
    {"name": "recItem", "symbols": ["recItem", "comma", "recUriJson"], "postprocess": d => { return d[0].concat(d[2]); }},
    {"name": "intIdentifier", "symbols": ["uriStart", "intType", "action", "recString", "recString"], "postprocess": d => { return d.join(""); }},
    {"name": "recUriJson", "symbols": ["recUri"], "postprocess": ([d]) => { return { 'recId': d , 'type': 'primary' }; }},
    {"name": "recUri", "symbols": ["uriStart", "recString"], "postprocess": d => { return d.join(""); }},
    {"name": "recString$string$1", "symbols": [{"literal":"R"}, {"literal":"e"}, {"literal":"c"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "recString", "symbols": ["recString$string$1", "string", {"literal":"-"}, "string"], "postprocess": d => { return d.join(""); }},
    {"name": "string$ebnf$1", "symbols": [/[\w]/]},
    {"name": "string$ebnf$1", "symbols": ["string$ebnf$1", /[\w]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "string", "symbols": ["string$ebnf$1"], "postprocess": ([d]) => {return d.join("");}},
    {"name": "intTypeSentence", "symbols": ["intType", {"literal":" "}, "action"], "postprocess": d => { return d[0]; }},
    {"name": "intType$string$1", "symbols": [{"literal":"R"}, {"literal":"e"}, {"literal":"p"}, {"literal":"e"}, {"literal":"a"}, {"literal":"t"}, {"literal":"e"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intType", "symbols": ["intType$string$1"], "postprocess": _ => "repetition"},
    {"name": "intType$string$2", "symbols": [{"literal":"A"}, {"literal":"l"}, {"literal":"t"}, {"literal":"e"}, {"literal":"r"}, {"literal":"n"}, {"literal":"a"}, {"literal":"t"}, {"literal":"i"}, {"literal":"v"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intType", "symbols": ["intType$string$2"], "postprocess": _ => "alternative"},
    {"name": "intType$string$3", "symbols": [{"literal":"C"}, {"literal":"o"}, {"literal":"n"}, {"literal":"t"}, {"literal":"r"}, {"literal":"a"}, {"literal":"d"}, {"literal":"i"}, {"literal":"c"}, {"literal":"t"}, {"literal":"i"}, {"literal":"o"}, {"literal":"n"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intType", "symbols": ["intType$string$3"], "postprocess": _ => "contradiction"},
    {"name": "intType$string$4", "symbols": [{"literal":"R"}, {"literal":"e"}, {"literal":"p"}, {"literal":"a"}, {"literal":"r"}, {"literal":"a"}, {"literal":"b"}, {"literal":"l"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intType", "symbols": ["intType$string$4"], "postprocess": _ => "repairable"},
    {"name": "action$string$1", "symbols": [{"literal":"A"}, {"literal":"c"}, {"literal":"t"}, {"literal":"i"}, {"literal":"o"}, {"literal":"n"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "action$ebnf$1", "symbols": [/[s]/], "postprocess": id},
    {"name": "action$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "action", "symbols": ["action$string$1", "action$ebnf$1"], "postprocess": function(data) { return null }},
    {"name": "uriStart$string$1", "symbols": [{"literal":"h"}, {"literal":"t"}, {"literal":"t"}, {"literal":"p"}, {"literal":":"}, {"literal":"/"}, {"literal":"/"}, {"literal":"a"}, {"literal":"n"}, {"literal":"o"}, {"literal":"n"}, {"literal":"y"}, {"literal":"m"}, {"literal":"o"}, {"literal":"u"}, {"literal":"s"}, {"literal":"."}, {"literal":"o"}, {"literal":"r"}, {"literal":"g"}, {"literal":"/"}, {"literal":"d"}, {"literal":"a"}, {"literal":"t"}, {"literal":"a"}, {"literal":"/"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "uriStart", "symbols": ["uriStart$string$1"], "postprocess": id},
    {"name": "comma", "symbols": [{"literal":","}, "_"]},
    {"name": "_$ebnf$1", "symbols": []},
    {"name": "_$ebnf$1", "symbols": ["_$ebnf$1", /[\s]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "_", "symbols": ["_$ebnf$1"], "postprocess": function(data) { return null; }}
]
  , ParserStart: "main"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();
