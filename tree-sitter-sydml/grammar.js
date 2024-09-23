/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const common =
  { symbol_chars: /[^ \r\n\t\f\v\p{Zs}\p{Zl}\p{Zp}#;"'`,\(\)\{\}\[\]\\\|\/]+/
  , keyword_chars: /:[^ \r\n\t\f\v\p{Zs}\p{Zl}\p{Zp}#;"'`,\(\)\{\}\[\]\\\|\/]+/
  };

module.exports = grammar
  ({ name: "sydml"
   , rules:
     { source_file: $ => repeat($._anything)
     , _anything: $ => choice
       ( $.symbol
       , $.keyword
       , $.list
       , $._comment
       )
     , list: $ => seq
       ( "("
       , repeat($._anything)
       , ")"
       )
     , _comment: $ => seq(";", /.*/)
     , symbol: $ => $._symbol
     , keyword: $ => common.keyword_chars
     , _symbol: $ => common.symbol_chars
     }
   , extras: $ =>
     [ /\s/
     ]
   })
