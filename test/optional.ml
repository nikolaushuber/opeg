let grammar_str = {|
grammar optional = <{
    start: 
          "a"? "c" $$ { }
        / "c"* $$ { }
}>
|}

let grammar = Lib.Grammar_utils.string_to_grammar grammar_str 

let parse_string str = 
  let ptree = Lib.Interpreter.interpret grammar str in 
  let res = Yojson.Safe.prettify (Lib.Parsetree.Json.string_of_t (Lib.Parsetree.simplify ptree)) in 
  print_endline res

let expect_no_parse str = 
  try 
    parse_string str; false 
  with
    Lib.Interpreter.No_parse -> true  

let%expect_test "ac" = 
    parse_string "ac";
    [%expect {|
      {
        "name": "None",
        "rule": "start",
        "choice": 0,
        "pos": [ 0, 2 ],
        "node": [
          "Tree",
          [
            {
              "name": "None",
              "rule": "start",
              "choice": 0,
              "pos": [ 0, 1 ],
              "node": [
                "Option",
                [
                  "Some",
                  {
                    "name": "None",
                    "rule": "start",
                    "choice": 0,
                    "pos": [ 0, 1 ],
                    "node": [ "Lexeme", "a" ]
                  }
                ]
              ]
            },
            {
              "name": "None",
              "rule": "start",
              "choice": 0,
              "pos": [ 1, 2 ],
              "node": [ "Lexeme", "c" ]
            },
            {
              "name": "None",
              "rule": "start",
              "choice": 0,
              "pos": [ 2, 2 ],
              "node": "Eof"
            }
          ]
        ]
      } |}]
  
let%expect_test "c" = 
    parse_string "c"; 
    [%expect {|
      {
        "name": "None",
        "rule": "start",
        "choice": 0,
        "pos": [ 0, 1 ],
        "node": [
          "Tree",
          [
            {
              "name": "None",
              "rule": "start",
              "choice": 0,
              "pos": [ 0, 0 ],
              "node": [ "Option", "None" ]
            },
            {
              "name": "None",
              "rule": "start",
              "choice": 0,
              "pos": [ 0, 1 ],
              "node": [ "Lexeme", "c" ]
            },
            {
              "name": "None",
              "rule": "start",
              "choice": 0,
              "pos": [ 1, 1 ],
              "node": "Eof"
            }
          ]
        ]
      } |}]

let%expect_test "cccc" = 
    parse_string "cccc"; 
    [%expect {|
      {
        "name": "None",
        "rule": "start",
        "choice": 1,
        "pos": [ 0, 4 ],
        "node": [
          "Tree",
          [
            {
              "name": "None",
              "rule": "start",
              "choice": 1,
              "pos": [ 0, 4 ],
              "node": [
                "Tree",
                [
                  {
                    "name": "None",
                    "rule": "start",
                    "choice": 1,
                    "pos": [ 0, 1 ],
                    "node": [ "Lexeme", "c" ]
                  },
                  {
                    "name": "None",
                    "rule": "start",
                    "choice": 1,
                    "pos": [ 1, 2 ],
                    "node": [ "Lexeme", "c" ]
                  },
                  {
                    "name": "None",
                    "rule": "start",
                    "choice": 1,
                    "pos": [ 2, 3 ],
                    "node": [ "Lexeme", "c" ]
                  },
                  {
                    "name": "None",
                    "rule": "start",
                    "choice": 1,
                    "pos": [ 3, 4 ],
                    "node": [ "Lexeme", "c" ]
                  }
                ]
              ]
            },
            {
              "name": "None",
              "rule": "start",
              "choice": 1,
              "pos": [ 4, 4 ],
              "node": "Eof"
            }
          ]
        ]
      } |}]

let%expect_test "" = 
    parse_string ""; 
    [%expect {|
      {
        "name": "None",
        "rule": "start",
        "choice": 1,
        "pos": [ 0, 0 ],
        "node": [
          "Tree",
          [
            {
              "name": "None",
              "rule": "start",
              "choice": 1,
              "pos": [ 0, 0 ],
              "node": [ "Tree", [] ]
            },
            {
              "name": "None",
              "rule": "start",
              "choice": 1,
              "pos": [ 0, 0 ],
              "node": "Eof"
            }
          ]
        ]
      } |}]

let%test _ = expect_no_parse "aa"
let%test _ = expect_no_parse "a" 
let%test _ = expect_no_parse "b" 
let%test _ = expect_no_parse "acca"

