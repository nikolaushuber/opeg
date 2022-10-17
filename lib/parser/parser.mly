%{
    (* exception Parsing_error of string *)

    type header_elem = 
        | Description of string 
        | Whitespace of string list 
        | Start of string 

    let get_description (l : header_elem list) : string = 
        let descr = List.find_opt (fun e -> match e with 
            | Description _ -> true 
            | _ -> false) l 
        in match descr with 
        | Some (Description d) -> d 
        | _ -> "" 
    
    let get_whitespace (l : header_elem list) : string list = 
        let wl = List.find_opt (fun e -> match e with 
                | Whitespace _ -> true 
                | _ -> false) l 
            in match wl with 
            | Some (Whitespace ws) -> ws 
            | _ -> []
    
    let get_start (l : header_elem list) : string = 
        let start = List.find_opt (fun e -> match e with 
            | Start _ -> true 
            | _ -> false) l 
        in match start with 
        | Some (Start s) -> s
        | _ -> "" 
%}

%token <string> TK_STRING 
%token <string> TK_REGEX 
%token TK_DESCRIPTION "description"
%token TK_WHITESPACE "whitespace" 
%token TK_START "start" 
%token <string> TK_NAME 
%token TK_COLON ":" 
%token TK_CHOICE "/" 
%token TK_EQUAL "=" 
%token TK_LBRACK "["
%token TK_RBRACK "]"
%token TK_COMMA ","
%token TK_OPTION "?" 
%token TK_PLUS "+" 
%token TK_STAR "*" 
%token <string> TK_SEM_ACTION 
%token TK_SEC_DIVIDE "%%" 
%token <string> TK_HEADER 
%token TK_EOF "eof" 

%start <Grammar.t> start 

%% 

start: 
    | TK_HEADER? hd_list = header_elem* "%%" lex_l = rule* "%%" rules = rule* "eof" 
    {
        {
            name = get_description hd_list; 
            whitespace = get_whitespace hd_list; 
            start = get_start hd_list; 
            lexing = lex_l; 
            rules = rules; 
        } : Grammar.t 
    }

header_elem: 
    | "description" "=" str = TK_STRING { Description str }
    | "whitespace" "=" "[" strl = separated_list(",", TK_STRING) "]" { Whitespace strl }
    | "start" "=" name = TK_NAME { Start name }
    | "start" "=" name = TK_STRING { Start name }

rule: 
    | name = TK_NAME ":" cl = separated_nonempty_list("/", choice) 
    {
        let choices = List.mapi (fun idx (c : Grammar.choice) -> 
            if String.equal c.name "" then 
                ({
                    name = "alt" ^ string_of_int idx; 
                    patterns = c.patterns; 
                } : Grammar.choice)
            else c 
        ) cl in 
        ({
            name = name; 
            choices = choices;
            pos = (0, 0, 0, 0); (* TODO *)
        } : Grammar.rule) 
    }

choice: 
    | name = TK_NAME ":" sl = pattern* TK_SEM_ACTION
    {
        {
            name = name;
            patterns = sl;  
        } : Grammar.choice 
    }
    | sl = pattern* TK_SEM_ACTION
    {
        {
            name = "";
            patterns = sl;  
        } : Grammar.choice 
    }

pattern: 
    | n = ioption(terminated(TK_NAME, "=")) p = pattern_type 
    {
        {
            ty = p; 
            ref = n;
        } : Grammar.pattern
    } 

pattern_type: 
    | ref = TK_NAME m = modifier { Reference (ref, m) : Grammar.pattern_type }
    | rx = TK_REGEX { Regex rx : Grammar.pattern_type }
    | quote = TK_STRING { Quote quote : Grammar.pattern_type }

modifier: 
    | "?" { Optional }
    | "*" { List }
    | "+" { NonEmptyList }
    | { NoMod }
