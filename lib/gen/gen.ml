open Grammar 

let rec gen_grammar ppf (g : t) = 
  Format.pp_open_vbox ppf 0; 
  begin match g.header with 
  | None -> () 
  | Some hd -> Format.fprintf ppf "%s@;%s@;%s@;@;" "(* Begin header *)" hd "(* End header *)"
  end; 
  Format.fprintf ppf "open OpegLib@;@;"; 
  List.iteri (fun idx (name, rule) ->  
    if (idx == 0) then begin
    Format.fprintf ppf "@[<v 1>let%s %s input =@;%s@;@;%a"  
      (if List.length g.rules > 1 then " rec" else "")
      name 
      "let state = make_state input in"
      gen_rule rule
      ;
    Format.fprintf ppf "match try_in_seq [%a] state with@;" 
      (Format.pp_print_list 
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
        (Format.pp_print_string)
      )
      (List.map (fun x -> "alt_" ^ string_of_int x) 
        (List.init (List.length rule) Fun.id)
      )
      ;
    Format.fprintf ppf "| Parse p -> p@;";
    Format.fprintf ppf "| No_parse -> failwith \"Parsing failed\"@]@;@;"
    end else begin 
      Format.fprintf ppf "@[<v 1>and %s state =@;%a"  
        name 
        gen_rule rule
        ;
      Format.fprintf ppf "try_in_seq [%a] state@]@;@;" 
        (Format.pp_print_list 
          ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
          (Format.pp_print_string)
        )
        (List.map (fun x -> "alt_" ^ string_of_int x) 
          (List.init (List.length rule) Fun.id)
        )
    end
  ) g.rules; 
  Format.pp_print_newline ppf (); 
  Format.pp_close_box ppf () 

and gen_rule ppf (r : Rule.t) =
  List.iteri ( fun idx choice -> 
    Format.fprintf ppf "@[<v 1>let %s state =@;%a@]@;in@;@;" 
      ("alt_" ^ string_of_int idx) 
      gen_choice choice
  ) r 

and gen_choice ppf (c : Choice.t) = 
  Format.pp_open_vbox ppf 0; 
  List.iter ( fun (no, symb) -> 
    Format.fprintf ppf "let* %s = %a in@;" 
    (match no with 
    | Some name -> name 
    | None -> "_"
    ) 
    gen_expr symb
  ) c.symbols; 
  Format.fprintf ppf "Parse ( %s )" c.action; 
  Format.pp_close_box ppf ()

and gen_expr ppf (e : Parse_expr.t) = match e with 
  | Match m -> gen_match_expr ppf m 
  | Predicate p -> gen_pred_expr ppf p 
  | Repetition r -> gen_rep_expr ppf r 
  | Reference r -> gen_ref_expr ppf r 
  | Eof -> gen_eof_expr ppf  

and gen_match_expr ppf (m : Match_expr.t) : unit = match m with 
  | Quote q -> Format.fprintf ppf "parse_string state \"%s\"" q 
  | Regex r -> Format.fprintf ppf "parse_regex state \"%s\"" r

and gen_ref_expr ppf (r : Reference_expr.t) = 
  Format.fprintf ppf "%s state" r 

and gen_eof_expr ppf = Format.fprintf ppf "expect_eof state" 

and gen_pred_expr _ _ = failwith "Not yet implemented" 
and gen_rep_expr _ _ = failwith "Not yet implemented" 

