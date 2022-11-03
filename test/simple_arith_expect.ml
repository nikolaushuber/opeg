let simple_arith = Test_grammars.simple_arith_grammar

let parse_string str = 
  let ptree = Lib.Interpreter.interpret simple_arith str in 
  let res = Yojson.Safe.prettify (Lib.Parsetree.Json.string_of_t (Lib.Parsetree.simplify ptree)) in 
  print_endline res

let%expect_test "1+2+3" = 
  parse_string "1+2+3"; 
  [%expect {|
    {
      "name": "None",
      "rule": "expr",
      "choice": 0,
      "pos": [ 0, 5 ],
      "node": [
        "Tree",
        [
          {
            "name": [ "Some", "t" ],
            "rule": "term",
            "choice": 1,
            "pos": [ 0, 1 ],
            "node": [
              "Tree",
              [
                {
                  "name": [ "Some", "a" ],
                  "rule": "atom",
                  "choice": 0,
                  "pos": [ 0, 1 ],
                  "node": [
                    "Tree",
                    [
                      {
                        "name": [ "Some", "n" ],
                        "rule": "number",
                        "choice": 0,
                        "pos": [ 0, 1 ],
                        "node": [ "Lexeme", "1" ]
                      }
                    ]
                  ]
                }
              ]
            ]
          },
          {
            "name": "None",
            "rule": "expr",
            "choice": 0,
            "pos": [ 1, 2 ],
            "node": [ "Lexeme", "+" ]
          },
          {
            "name": [ "Some", "e" ],
            "rule": "expr",
            "choice": 0,
            "pos": [ 2, 5 ],
            "node": [
              "Tree",
              [
                {
                  "name": [ "Some", "t" ],
                  "rule": "term",
                  "choice": 1,
                  "pos": [ 2, 3 ],
                  "node": [
                    "Tree",
                    [
                      {
                        "name": [ "Some", "a" ],
                        "rule": "atom",
                        "choice": 0,
                        "pos": [ 2, 3 ],
                        "node": [
                          "Tree",
                          [
                            {
                              "name": [ "Some", "n" ],
                              "rule": "number",
                              "choice": 0,
                              "pos": [ 2, 3 ],
                              "node": [ "Lexeme", "2" ]
                            }
                          ]
                        ]
                      }
                    ]
                  ]
                },
                {
                  "name": "None",
                  "rule": "expr",
                  "choice": 0,
                  "pos": [ 3, 4 ],
                  "node": [ "Lexeme", "+" ]
                },
                {
                  "name": [ "Some", "e" ],
                  "rule": "expr",
                  "choice": 1,
                  "pos": [ 4, 5 ],
                  "node": [
                    "Tree",
                    [
                      {
                        "name": [ "Some", "t" ],
                        "rule": "term",
                        "choice": 1,
                        "pos": [ 4, 5 ],
                        "node": [
                          "Tree",
                          [
                            {
                              "name": [ "Some", "a" ],
                              "rule": "atom",
                              "choice": 0,
                              "pos": [ 4, 5 ],
                              "node": [
                                "Tree",
                                [
                                  {
                                    "name": [ "Some", "n" ],
                                    "rule": "number",
                                    "choice": 0,
                                    "pos": [ 4, 5 ],
                                    "node": [ "Lexeme", "3" ]
                                  }
                                ]
                              ]
                            }
                          ]
                        ]
                      }
                    ]
                  ]
                }
              ]
            ]
          }
        ]
      ]
    } |}] 

let%expect_test "(1+2)*3" = 
  parse_string "(1+2)*3";
  [%expect {|
    {
      "name": "None",
      "rule": "expr",
      "choice": 1,
      "pos": [ 0, 7 ],
      "node": [
        "Tree",
        [
          {
            "name": [ "Some", "t" ],
            "rule": "term",
            "choice": 0,
            "pos": [ 0, 7 ],
            "node": [
              "Tree",
              [
                {
                  "name": [ "Some", "a" ],
                  "rule": "atom",
                  "choice": 1,
                  "pos": [ 0, 5 ],
                  "node": [
                    "Tree",
                    [
                      {
                        "name": "None",
                        "rule": "atom",
                        "choice": 1,
                        "pos": [ 0, 1 ],
                        "node": [ "Lexeme", "(" ]
                      },
                      {
                        "name": [ "Some", "e" ],
                        "rule": "expr",
                        "choice": 0,
                        "pos": [ 1, 4 ],
                        "node": [
                          "Tree",
                          [
                            {
                              "name": [ "Some", "t" ],
                              "rule": "term",
                              "choice": 1,
                              "pos": [ 1, 2 ],
                              "node": [
                                "Tree",
                                [
                                  {
                                    "name": [ "Some", "a" ],
                                    "rule": "atom",
                                    "choice": 0,
                                    "pos": [ 1, 2 ],
                                    "node": [
                                      "Tree",
                                      [
                                        {
                                          "name": [ "Some", "n" ],
                                          "rule": "number",
                                          "choice": 0,
                                          "pos": [ 1, 2 ],
                                          "node": [ "Lexeme", "1" ]
                                        }
                                      ]
                                    ]
                                  }
                                ]
                              ]
                            },
                            {
                              "name": "None",
                              "rule": "expr",
                              "choice": 0,
                              "pos": [ 2, 3 ],
                              "node": [ "Lexeme", "+" ]
                            },
                            {
                              "name": [ "Some", "e" ],
                              "rule": "expr",
                              "choice": 1,
                              "pos": [ 3, 4 ],
                              "node": [
                                "Tree",
                                [
                                  {
                                    "name": [ "Some", "t" ],
                                    "rule": "term",
                                    "choice": 1,
                                    "pos": [ 3, 4 ],
                                    "node": [
                                      "Tree",
                                      [
                                        {
                                          "name": [ "Some", "a" ],
                                          "rule": "atom",
                                          "choice": 0,
                                          "pos": [ 3, 4 ],
                                          "node": [
                                            "Tree",
                                            [
                                              {
                                                "name": [ "Some", "n" ],
                                                "rule": "number",
                                                "choice": 0,
                                                "pos": [ 3, 4 ],
                                                "node": [ "Lexeme", "2" ]
                                              }
                                            ]
                                          ]
                                        }
                                      ]
                                    ]
                                  }
                                ]
                              ]
                            }
                          ]
                        ]
                      },
                      {
                        "name": "None",
                        "rule": "atom",
                        "choice": 1,
                        "pos": [ 4, 5 ],
                        "node": [ "Lexeme", ")" ]
                      }
                    ]
                  ]
                },
                {
                  "name": "None",
                  "rule": "term",
                  "choice": 0,
                  "pos": [ 5, 6 ],
                  "node": [ "Lexeme", "*" ]
                },
                {
                  "name": [ "Some", "t" ],
                  "rule": "term",
                  "choice": 1,
                  "pos": [ 6, 7 ],
                  "node": [
                    "Tree",
                    [
                      {
                        "name": [ "Some", "a" ],
                        "rule": "atom",
                        "choice": 0,
                        "pos": [ 6, 7 ],
                        "node": [
                          "Tree",
                          [
                            {
                              "name": [ "Some", "n" ],
                              "rule": "number",
                              "choice": 0,
                              "pos": [ 6, 7 ],
                              "node": [ "Lexeme", "3" ]
                            }
                          ]
                        ]
                      }
                    ]
                  ]
                }
              ]
            ]
          }
        ]
      ]
    } |}]

let expect_no_parse str = 
  try 
    parse_string str; false 
  with
    Lib.Interpreter.No_parse -> true  

let%test _ = expect_no_parse "1+("
let%test _ = expect_no_parse "(1+2"
let%test _ = expect_no_parse "1/2" 
let%test _ = expect_no_parse "1.2"
let%test _ = expect_no_parse "1+2.0"

let expect_not_closed grammar = Bool.not (Lib.Grammar.is_closed grammar)
let expect_closed grammar = Lib.Grammar.is_closed grammar 

let%test _ = expect_not_closed Test_grammars.fragment1 
let%test _ = expect_not_closed Test_grammars.fragment2 
let%test _ = expect_closed (Lib.Grammar.(++) Test_grammars.fragment1 Test_grammars.fragment2)
