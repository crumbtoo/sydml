/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC =
  {
  }

module.exports = grammar
  ({ name: "sydml"
   , rules:
     { source_file: $ => "hello"
     }
   // , extras: $ =>
     // [ /\p{Zs}/
     // , /\n/
     // , /\r/
     // , $.comment
     // ]
   })
