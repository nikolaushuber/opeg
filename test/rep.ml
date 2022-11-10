let grammar_str = {|
grammar rep = <{
  start: "a"+ "b"? "c"* { }
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

let%expect_test "abc" = 
  parse_string "abc";
  [%expect {|
    {
      "name": "None",
      "rule": "start",
      "choice": 0,
      "pos": [ 0, 3 ],
      "node": [
        "Tree",
        [
          {
            "name": "None",
            "rule": "start",
            "choice": 0,
            "pos": [ 0, 1 ],
            "node": [ "Lexeme", "a" ]
          },
          {
            "name": "None",
            "rule": "start",
            "choice": 0,
            "pos": [ 1, 2 ],
            "node": [
              "Option",
              [
                "Some",
                {
                  "name": "None",
                  "rule": "start",
                  "choice": 0,
                  "pos": [ 1, 2 ],
                  "node": [ "Lexeme", "b" ]
                }
              ]
            ]
          },
          {
            "name": "None",
            "rule": "start",
            "choice": 0,
            "pos": [ 2, 3 ],
            "node": [ "Lexeme", "c" ]
          }
        ]
      ]
    } |}] 

let%expect_test "aacc" = 
  parse_string "aacc";
  [%expect {|
    {
      "name": "None",
      "rule": "start",
      "choice": 0,
      "pos": [ 0, 4 ],
      "node": [
        "Tree",
        [
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
                  "node": [ "Lexeme", "a" ]
                },
                {
                  "name": "None",
                  "rule": "start",
                  "choice": 0,
                  "pos": [ 1, 2 ],
                  "node": [ "Lexeme", "a" ]
                }
              ]
            ]
          },
          {
            "name": "None",
            "rule": "start",
            "choice": 0,
            "pos": [ 2, 2 ],
            "node": [ "Option", "None" ]
          },
          {
            "name": "None",
            "rule": "start",
            "choice": 0,
            "pos": [ 2, 4 ],
            "node": [
              "Tree",
              [
                {
                  "name": "None",
                  "rule": "start",
                  "choice": 0,
                  "pos": [ 2, 3 ],
                  "node": [ "Lexeme", "c" ]
                },
                {
                  "name": "None",
                  "rule": "start",
                  "choice": 0,
                  "pos": [ 3, 4 ],
                  "node": [ "Lexeme", "c" ]
                }
              ]
            ]
          }
        ]
      ]
    } |}] 

let%expect_test "abcc" = 
  parse_string "abcc";
  [%expect {|
    {
      "name": "None",
      "rule": "start",
      "choice": 0,
      "pos": [ 0, 4 ],
      "node": [
        "Tree",
        [
          {
            "name": "None",
            "rule": "start",
            "choice": 0,
            "pos": [ 0, 1 ],
            "node": [ "Lexeme", "a" ]
          },
          {
            "name": "None",
            "rule": "start",
            "choice": 0,
            "pos": [ 1, 2 ],
            "node": [
              "Option",
              [
                "Some",
                {
                  "name": "None",
                  "rule": "start",
                  "choice": 0,
                  "pos": [ 1, 2 ],
                  "node": [ "Lexeme", "b" ]
                }
              ]
            ]
          },
          {
            "name": "None",
            "rule": "start",
            "choice": 0,
            "pos": [ 2, 4 ],
            "node": [
              "Tree",
              [
                {
                  "name": "None",
                  "rule": "start",
                  "choice": 0,
                  "pos": [ 2, 3 ],
                  "node": [ "Lexeme", "c" ]
                },
                {
                  "name": "None",
                  "rule": "start",
                  "choice": 0,
                  "pos": [ 3, 4 ],
                  "node": [ "Lexeme", "c" ]
                }
              ]
            ]
          }
        ]
      ]
    } |}] 

let%expect_test "ab" = 
  parse_string "ab";
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
            "node": [ "Lexeme", "a" ]
          },
          {
            "name": "None",
            "rule": "start",
            "choice": 0,
            "pos": [ 1, 2 ],
            "node": [
              "Option",
              [
                "Some",
                {
                  "name": "None",
                  "rule": "start",
                  "choice": 0,
                  "pos": [ 1, 2 ],
                  "node": [ "Lexeme", "b" ]
                }
              ]
            ]
          },
          {
            "name": "None",
            "rule": "start",
            "choice": 0,
            "pos": [ 2, 2 ],
            "node": [ "Tree", [] ]
          }
        ]
      ]
    } |}] 

let%expect_test "a" = 
  parse_string "a";
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
            "pos": [ 0, 1 ],
            "node": [ "Lexeme", "a" ]
          },
          {
            "name": "None",
            "rule": "start",
            "choice": 0,
            "pos": [ 1, 1 ],
            "node": [ "Option", "None" ]
          },
          {
            "name": "None",
            "rule": "start",
            "choice": 0,
            "pos": [ 1, 1 ],
            "node": [ "Tree", [] ]
          }
        ]
      ]
    } |}]

let%test _ = expect_no_parse "bc" 
let%test _ = expect_no_parse "abbc" 
let%test _ = expect_no_parse "d" 
let%test _ = expect_no_parse "abca" 
