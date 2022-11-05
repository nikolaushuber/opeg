%{
    open Grammar
    (* exception Parsing_error of string *)
%}

%token <string> TK_STRING 
%token <string> TK_REGEX 
%token <string> TK_IDENTIFIER 
%token <string> TK_SEM_ACTION  
%token <string> TK_HEADER 
%token TK_COLON ":" 
%token TK_CHOICE "/" 
%token TK_EQUAL "=" 
%token TK_OPTION "?" 
%token TK_NOT "!"
%token TK_AND "&"
%token TK_PLUS "+" 
%token TK_STAR "*" 
%token TK_EOF "eof" 
%token TK_GRAMMAR "grammar" 
%token TK_DEF_START "<{"
%token TK_DEF_END "}>"

%nonassoc "?"
%left "+"
%nonassoc "*"
%nonassoc "!"
%nonassoc "&"

%start <(string * Grammar.t) list> parse 

%% 

parse: gl = grammar_def* "eof" { gl }

grammar_def: 
    | "grammar" name = TK_IDENTIFIER "=" "<{" g = grammar "}>" { (name, g) }
    | "grammar" name = TK_IDENTIFIER "=" l = separated_list("+", TK_IDENTIFIER) { 
            (name, {Grammar.empty with parts = l}) 
    }

grammar: 
    | hd = TK_HEADER? rules = rule* 
    {
        Grammar.{
            rules = rules; 
            header = hd; 
            parts = [];
        }
    }

rule: 
    | name = TK_IDENTIFIER ":" cl = choices
    { 
        (name, cl)
    }

choices: 
   | c = choice { [c] }
   | c = choice "/" cl = choices { c :: cl } 

choice: 
    | sl = expr* act = TK_SEM_ACTION
    {
        Choice.{
            symbols = sl;  
            action = act;
        }
    }

expr: 
    | p = expr_type { (None, p) } 
    | n = TK_IDENTIFIER "=" p = expr_type { (Some n, p) }

expr_type: 
    | rx = TK_REGEX { Parse_expr.Match (Match_expr.Regex rx) }
    | q = TK_STRING { Parse_expr.Match (Match_expr.Quote q) }
    | "!" e = expr_type { Parse_expr.Predicate (Predicate_expr.Not e) }
    | "&" e = expr_type { Parse_expr.Predicate (Predicate_expr.And e) }
    | e = expr_type "?" { Parse_expr.Repetition (Repetition_expr.Zero_or_one e) }
    | e = expr_type "*" { Parse_expr.Repetition (Repetition_expr.Zero_or_more e) }
    | e = expr_type "+" { Parse_expr.Repetition (Repetition_expr.One_or_more e) }
    | n = TK_IDENTIFIER { Parse_expr.Reference n }
