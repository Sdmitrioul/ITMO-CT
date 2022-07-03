grammar Grammar;

@header{
import grammar.*;
}

start returns [Grammar gr]
        : NTERM { $gr = new Grammar($NTERM.text); }
          (imports[$gr])?
          (rule_[$gr] END)+
          EOF
        ;

imports[Grammar gr]
        : IMP
          SOPENBR
          CODE { $gr.getImports().add($CODE.text.substring(1, $CODE.text.length() - 1)); }
          (COMMA CODE { $gr.getImports().add($CODE.text.substring(1, $CODE.text.length() - 1)); })*
          SCLOSEBR
        ;


rule_[Grammar gr]
        : TERM EQUALS STRING    { $gr.addTerminalRule(new TerminalRuleImpl($TERM.text, $STRING.text)); }
        | TERM COLON STRING     { $gr.addTerminalRule(new RegexRuleImpl($TERM.text, $STRING.text)); }
        | not_terminal_rule     { $gr.addNotTerminalRule($not_terminal_rule.v); }
        ;

not_terminal_rule returns [NotTerminalRule v]
        : NTERM args not_terminal_returns EQUALS    { $v = new NotTerminalRule($NTERM.text, $args.v, $not_terminal_returns.v); }
          rightPart                                 { $v.addRule($rightPart.v); }
          (OR rightPart                             { $v.addRule($rightPart.v); })*
        ;

args returns [List<Argument> v]
        : SOPENBR               { $v = new ArrayList<>(); }
          arg                   { $v.add($arg.v); }
          (COMMA arg            { $v.add($arg.v); })*
          SCLOSEBR
        |                       { $v = new ArrayList<>(); }
        ;

not_terminal_returns returns [List<Argument> v]
        : RETURN SOPENBR        { $v = new ArrayList<>(); }
          arg                   { $v.add($arg.v); }
          (COMMA arg            { $v.add($arg.v); })*
          SCLOSEBR
        |                       { $v = new ArrayList<>(); }
        ;

arg returns [Argument v]
        : l = var ':' r = var    { $v = new Argument($l.v, $r.v); }
        ;

var returns [String v]
        : TERM          { $v = $TERM.text; }
        | NTERM         { $v = $NTERM.text; }
        ;

rightPart returns [List<RuleToken> v]
        :               { $v = new ArrayList<>(); }
        (ruleToken      { $v.add($ruleToken.v); })+
        ;

ruleToken returns [RuleToken v]
        : TERM          { $v = new Terminal($TERM.text); }
        | NTERM         { NotTerminal t = new NotTerminal($NTERM.text); }
          ('(' param    { t.addArg($param.v); }
          (COMMA param  { t.addArg($param.v); })*
          ')')?         { $v = t; }
        | CODE          { $v = new Code($CODE.text); }
        ;

param returns [String v]
        : CODE  { $v = $CODE.text.substring(1, $CODE.text.length() - 1); }
        | var   { $v = $var.v; }
        ;


COMMA    : ','                  ;
COLON    : ':'                  ;
EQUALS   : '='                  ;
SOPENBR  : '['                  ;
SCLOSEBR : ']'                  ;
OR       : '|'                  ;
END      : ';'                  ;
RETURN   : 'returns'            ;
IMP      : '@IMP'               ;

TERM   : [A-Z][a-zA-Z0-9_]*;
NTERM  : [a-z][a-zA-Z0-9_]*;

STRING : '"' (~('"'))* '"';
CODE   : '{' (~[{}]+ CODE?)* '}';

WS : [ \t\r\n] -> skip;