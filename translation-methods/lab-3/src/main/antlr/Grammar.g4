grammar Grammar;

file
    : package_? imports* p_class class_* EOF
    ;

package_
    : PACKAGE LONGNAME SEMICOLON
    ;

imports
    : IMPORT class_name SEMICOLON
    ;

p_class
    : PUBLIC class_
    ;

class_
    : CLASS SHORTNAME extend? implementings? OPENPARENTESIS class_body* CLOSEPARENTESIS
    ;

extend
    : EXTEND class_name
    ;

implementings
    : IMPLEMENT class_name implement_interfaces*
    ;

implement_interfaces
                : ',' class_name
                ;

class_body
        : field
        | method
        ;

field_decorators
                : access_modifiers? modifications
                ;

field
    : field_decorators type SHORTNAME equal_part_of_field? SEMICOLON
    ;

additional_names
                : (COMMA SHORTNAME)+
                ;

string_id
        : STRINGVALUE (PLUS string_id)?
        | SHORTNAME (PLUS string_id)?
        ;

boolean_id
        : TRUE
        | FALSE
        | SHORTNAME
        | condition
        | LP boolean_id RP
        | NOT boolean_id
        | boolean_id (AND | ANDA) boolean_id
        | boolean_id (OR | ORA) boolean_id
        ;

equal_part_of_field
                : EQUALITY value
                ;

value
    : expression
    | string_id
    | boolean_id
    | new_object
    ;

new_object
        : NEW class_name LP (value (COMMA value)*)? RP
        ;

method
    : field_decorators (VOID | type) SHORTNAME args_list method_body
    ;

args_list
        : LP (type SHORTNAME (COMMA type SHORTNAME)*)? RP
        ;

method_body
        : OPENPARENTESIS code_line* CLOSEPARENTESIS
        ;

block_of_code
        : while_block
        | do_while_block
        | for_block
        | if_block
        | try_block
        | switch_block
        ;

switch_block
        : SWITCH LP SHORTNAME RP case_block
        ;

case_block
        : OPENPARENTESIS case_condition* default_condition? CLOSEPARENTESIS
        ;

case_condition
            : CASE value COLON code_line*
            ;

default_condition
                : DEFAULT COLON code_line*
                ;

code_line
        : RETURN (value | statement) SEMICOLON
        | block_of_code
        | statement SEMICOLON
        | SHORTNAME equal_part_of_field SEMICOLON
        | BREAK SEMICOLON
        ;

statement
        : create_variable
        | (INC | DEC) SHORTNAME
        | SHORTNAME (INC | DEC)
        | call_method
        | (SHORTNAME | LP new_object RP) (POINT (SHORTNAME | call_method | LONGNAME (LP (value (COMMA value)*)? RP)?))+
        ;

create_variable
            : FINAL type SHORTNAME additional_names? equal_part_of_field?
            | type SHORTNAME additional_names? equal_part_of_field?
            ;

call_method
        : SHORTNAME LP (value (COMMA value)*)? RP
        ;

if_block
        : IF LP boolean_id RP method_body else_if* else_block?
        ;

else_if
        : ELSE IF LP boolean_id RP method_body
        ;

else_block
        : ELSE method_body
        ;

try_block
        : TRY (LP create_variable RP)? method_body catch_block? finally_block?
        ;

finally_block
            : FINALLY method_body
            ;

catch_block
        : CATCH LP class_name SHORTNAME RP method_body
        ;


for_block
        : FOR LP statement? SEMICOLON boolean_id SEMICOLON statement? RP method_body
        | FOR LP FINAL? type? SHORTNAME COLON SHORTNAME RP method_body
        ;

while_block
        : WHILE LP boolean_id RP method_body
        ;

do_while_block
            : DO method_body WHILE LP boolean_id RP
            ;

modifications
            : STATIC? FINAL?
            | FINAL? STATIC?
            ;

access_modifiers
            : PUBLIC
            | PRIVATE
            | PROTECTED
            ;

class_name
        : SHORTNAME
        | LONGNAME
        ;

expression
        : SHORTNAME
        | NUMBER
        | LP expression RP
        | (INC | DEC) expression
        | expression (INC | DEC)
        | (PLUS | MINUS) expression
        | expression (MUL | DIVIDE) expression
        | expression (PLUS | MINUS) expression
        ;

condition
        : string_id NOTEQUAL string_id
        | string_id EQUAL string_id
        | expression EQUAL expression
        | expression NOTEQUAL expression
        | expression compare_operation expression
        ;

compare_operation
                : SMALLER
                | SMALLEREQ
                | BIGGEREQ
                | BIGGER
                ;

type
    : INT
    | DOUBLE
    | BOOLEAN
    | STRING
    | class_name
    ;

INT             : 'int'                         ;
DOUBLE          : 'double'                      ;
BOOLEAN         : 'boolean'                     ;
STRING          : 'String'                      ;

TRUE            : 'true'                        ;
FALSE           : 'false'                       ;

PACKAGE         : 'package'                     ;
IF              : 'if'                          ;
ELSE            : 'else'                        ;
IMPORT          : 'import'                      ;
PUBLIC          : 'public'                      ;
PRIVATE         : 'private'                     ;
PROTECTED       : 'protected'                   ;
CLASS           : 'class'                       ;
EXTEND          : 'extends'                     ;
IMPLEMENT       : 'implements'                  ;
ABSTRACT        : 'abstract'                    ;
STATIC          : 'static'                      ;
FINAL           : 'final'                       ;
NEW             : 'new'                         ;
BREAK           : 'break'                       ;
VOID            : 'void'                        ;
SWITCH          : 'switch'                      ;
CASE            : 'case'                        ;
DEFAULT         : 'default'                     ;
RETURN          : 'return'                      ;
WHILE           : 'while'                       ;
DO              : 'do'                          ;
FOR             : 'for'                         ;
TRY             : 'try'                         ;
CATCH           : 'catch'                       ;
FINALLY         : 'finally'                     ;
OPENPARENTESIS  : '{'                           ;
CLOSEPARENTESIS : '}'                           ;
SEMICOLON       : ';'                           ;
COMMA           : ','                           ;
POINT           : '.'                           ;
COLON           : ':'                           ;

INC             : '++'                          ;
DEC             : '--'                          ;

MINUS           : '-'                           ;
PLUS            : '+'                           ;
DIVIDE          : '/'                           ;
MUL             : '*'                           ;
LP              : '('                           ;
RP              : ')'                           ;
SLB             : '['                           ;
SRB             : ']'                           ;


SMALLEREQ       : '<='                          ;
BIGGEREQ        : '>='                          ;
SMALLER         : '<'                           ;
BIGGER          : '>'                           ;

EQUAL           : '=='                          ;
NOTEQUAL        : '!='                          ;
EQUALITY        : '='                           ;

AND             : '&&'                          ;
ANDA            : '&'                           ;
OR              : '||'                          ;
ORA             : '|'                           ;
NOT             : '!'                           ;

fragment NAME   : [a-zA-Z] [a-zA-Z0-9_]*        ;
fragment DIGIT  : [0-9]                         ;
fragment Schar  : ~ ["\\\r\n]                   ;

STRINGVALUE     : '"' Schar* '"'                ;
LONGNAME        : NAME (POINT NAME)+            ;
SHORTNAME       : NAME                          ;
NUMBER          : (DIGIT+ | DIGIT+ '.' DIGIT*)  ;

WS              : [ \t]+  -> skip               ;
NL              : [\n\r]+ -> skip               ;
