%{
    open Parsetree 
%}

%token          TOK_KW_PARSER   "parser" 
%token          TOK_KW_TOKEN    "token" 
%token <string> TOK_HEADER 
%token <string> TOK_SEM_ACTION 
%token <string> TOK_NAME 
%token <string> TOK_TYPE
%token          TOK_DOUBLE_DOTS ":" 
%token          TOK_CHOICE      "/"
%token          TOK_BAR         "|" 
%token          TOK_EQUALS      "=" 
%token          TOK_OPTION      "?" 
%token          TOK_STAR        "*" 
%token          TOK_PLUS        "+" 
%token          TOK_SEC_DIVIDE  "%%" 
%token          TOK_EOF 

%start <Parsetree.t> parse 

%% 

parse: 
    | hd = TOK_HEADER? "token" ":" 
      tok_l = list(tok) "parser" name = TOK_NAME
      start_deriv = TOK_NAME "%%" rule_l = list(rule) 
      TOK_EOF 
      {
        {
            header = hd; 
            parser_name = name; 
            start_deriv = start_deriv; 
            tokens = tok_l; 
            rules = rule_l; 
        }
      }

tok: 
    | "|" name = TOK_NAME t = TOK_TYPE? short = TOK_NAME? { (name, t, short) }

rule: 
    | name = TOK_NAME _type = TOK_TYPE ":" deriv_l = list(deriv) {
        (name, _type, deriv_l) 
    }

deriv: 
    | "/" sym_l = list(symbol) act = TOK_SEM_ACTION { (sym_l, act) }

symbol:
    | syn = ioption(terminated(TOK_NAME, "=")) name = TOK_NAME suff = suffix? { (name, syn, suff) } 

suffix: 
    | "?" { Optional }
    | "+" { Plus }
    | "*" { Star }

