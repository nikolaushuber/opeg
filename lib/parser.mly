%{
    open Grammar 
%}

%token          TOK_KW_PARSER   "parser" 
%token          TOK_KW_TOKEN    "token" 
%token <string> TOK_HEADER 
%token <string> TOK_SEM_ACTION 
%token <string> TOK_NAME 
%token          TOK_DOUBLE_DOTS ":" 
%token          TOK_CHOICE      "/"
%token          TOK_BAR         "|" 
%token          TOK_EQUALS      "=" 
%token          TOK_SEC_DIVIDE  "%%" 
%token          TOK_RET_ARROW   "->" 
%token          TOK_LEFT_ARROW  "<" 
%token          TOK_RIGHT_ARROW ">"
%token          TOK_EOF 


%start <Grammar.t> parse 

%% 

parse: 
    | hd = TOK_HEADER? "token" ":" 
      tok_l = list(tok) "parser" name = TOK_NAME? 
      "<" start_deriv = TOK_NAME ">" "%%" rule_l = list(rule) 
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
    | "|" name = TOK_NAME "<" t = TOK_NAME ">" short = TOK_NAME? { (name, Some t, short) }
    | "|" name = TOK_NAME short = TOK_NAME? { (name, None, short) }

rule: 
    | name = TOK_NAME "->" _type = TOK_NAME ":" deriv_l = list(deriv) {
        (name, _type, deriv_l) 
    }

deriv: 
    | "/" sym_l = list(symbol) act = TOK_SEM_ACTION { (sym_l, act) }

symbol:
    | syn = TOK_NAME "=" name = TOK_NAME { Nonterminal (name, syn) } 
    | name = TOK_NAME { Terminal (name, None) }
    | name = TOK_NAME "<" lex = TOK_NAME ">" { Terminal (name, Some lex) }
