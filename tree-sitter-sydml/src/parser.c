#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 12
#define LARGE_STATE_COUNT 6
#define SYMBOL_COUNT 13
#define ALIAS_COUNT 0
#define TOKEN_COUNT 7
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 3
#define PRODUCTION_ID_COUNT 1

enum ts_symbol_identifiers {
  anon_sym_LPAREN = 1,
  anon_sym_RPAREN = 2,
  anon_sym_SEMI = 3,
  aux_sym__comment_token1 = 4,
  sym_keyword = 5,
  sym__symbol = 6,
  sym_source_file = 7,
  sym__anything = 8,
  sym_list = 9,
  sym__comment = 10,
  sym_symbol = 11,
  aux_sym_source_file_repeat1 = 12,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_SEMI] = ";",
  [aux_sym__comment_token1] = "_comment_token1",
  [sym_keyword] = "keyword",
  [sym__symbol] = "_symbol",
  [sym_source_file] = "source_file",
  [sym__anything] = "_anything",
  [sym_list] = "list",
  [sym__comment] = "_comment",
  [sym_symbol] = "symbol",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [aux_sym__comment_token1] = aux_sym__comment_token1,
  [sym_keyword] = sym_keyword,
  [sym__symbol] = sym__symbol,
  [sym_source_file] = sym_source_file,
  [sym__anything] = sym__anything,
  [sym_list] = sym_list,
  [sym__comment] = sym__comment,
  [sym_symbol] = sym_symbol,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [aux_sym__comment_token1] = {
    .visible = false,
    .named = false,
  },
  [sym_keyword] = {
    .visible = true,
    .named = true,
  },
  [sym__symbol] = {
    .visible = false,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym__anything] = {
    .visible = false,
    .named = true,
  },
  [sym_list] = {
    .visible = true,
    .named = true,
  },
  [sym__comment] = {
    .visible = false,
    .named = true,
  },
  [sym_symbol] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
};

static TSCharacterRange sym_keyword_character_set_1[] = {
  {0, 0x08}, {0x0e, 0x1f}, {'!', '!'}, {'$', '&'}, {'*', '+'}, {'-', '.'}, {'0', ':'}, {'<', 'Z'},
  {'^', '_'}, {'a', 'z'}, {'~', 0x9f}, {0xa1, 0x167f}, {0x1681, 0x1fff}, {0x200b, 0x2027}, {0x202a, 0x202e}, {0x2030, 0x205e},
  {0x2060, 0x2fff}, {0x3001, 0x10ffff},
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(1);
      if (lookahead == '(') ADVANCE(2);
      if (lookahead == ')') ADVANCE(3);
      if (lookahead == ':') ADVANCE(8);
      if (lookahead == ';') ADVANCE(4);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(0);
      if ((!eof && set_contains(sym_keyword_character_set_1, 18, lookahead))) ADVANCE(9);
      END_STATE();
    case 1:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 2:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 3:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 4:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 5:
      ACCEPT_TOKEN(aux_sym__comment_token1);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(5);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(6);
      END_STATE();
    case 6:
      ACCEPT_TOKEN(aux_sym__comment_token1);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(6);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(sym_keyword);
      if ((!eof && set_contains(sym_keyword_character_set_1, 18, lookahead))) ADVANCE(7);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(sym__symbol);
      if ((!eof && set_contains(sym_keyword_character_set_1, 18, lookahead))) ADVANCE(7);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(sym__symbol);
      if ((!eof && set_contains(sym_keyword_character_set_1, 18, lookahead))) ADVANCE(9);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 0},
  [3] = {.lex_state = 0},
  [4] = {.lex_state = 0},
  [5] = {.lex_state = 0},
  [6] = {.lex_state = 0},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 5},
  [11] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [sym_keyword] = ACTIONS(1),
    [sym__symbol] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(11),
    [sym__anything] = STATE(4),
    [sym_list] = STATE(4),
    [sym__comment] = STATE(4),
    [sym_symbol] = STATE(4),
    [aux_sym_source_file_repeat1] = STATE(4),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(5),
    [anon_sym_SEMI] = ACTIONS(7),
    [sym_keyword] = ACTIONS(9),
    [sym__symbol] = ACTIONS(11),
  },
  [2] = {
    [sym__anything] = STATE(2),
    [sym_list] = STATE(2),
    [sym__comment] = STATE(2),
    [sym_symbol] = STATE(2),
    [aux_sym_source_file_repeat1] = STATE(2),
    [ts_builtin_sym_end] = ACTIONS(13),
    [anon_sym_LPAREN] = ACTIONS(15),
    [anon_sym_RPAREN] = ACTIONS(13),
    [anon_sym_SEMI] = ACTIONS(18),
    [sym_keyword] = ACTIONS(21),
    [sym__symbol] = ACTIONS(24),
  },
  [3] = {
    [sym__anything] = STATE(5),
    [sym_list] = STATE(5),
    [sym__comment] = STATE(5),
    [sym_symbol] = STATE(5),
    [aux_sym_source_file_repeat1] = STATE(5),
    [anon_sym_LPAREN] = ACTIONS(5),
    [anon_sym_RPAREN] = ACTIONS(27),
    [anon_sym_SEMI] = ACTIONS(7),
    [sym_keyword] = ACTIONS(29),
    [sym__symbol] = ACTIONS(11),
  },
  [4] = {
    [sym__anything] = STATE(2),
    [sym_list] = STATE(2),
    [sym__comment] = STATE(2),
    [sym_symbol] = STATE(2),
    [aux_sym_source_file_repeat1] = STATE(2),
    [ts_builtin_sym_end] = ACTIONS(31),
    [anon_sym_LPAREN] = ACTIONS(5),
    [anon_sym_SEMI] = ACTIONS(7),
    [sym_keyword] = ACTIONS(33),
    [sym__symbol] = ACTIONS(11),
  },
  [5] = {
    [sym__anything] = STATE(2),
    [sym_list] = STATE(2),
    [sym__comment] = STATE(2),
    [sym_symbol] = STATE(2),
    [aux_sym_source_file_repeat1] = STATE(2),
    [anon_sym_LPAREN] = ACTIONS(5),
    [anon_sym_RPAREN] = ACTIONS(35),
    [anon_sym_SEMI] = ACTIONS(7),
    [sym_keyword] = ACTIONS(33),
    [sym__symbol] = ACTIONS(11),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 2,
    ACTIONS(39), 1,
      sym__symbol,
    ACTIONS(37), 5,
      ts_builtin_sym_end,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SEMI,
      sym_keyword,
  [11] = 2,
    ACTIONS(43), 1,
      sym__symbol,
    ACTIONS(41), 5,
      ts_builtin_sym_end,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SEMI,
      sym_keyword,
  [22] = 2,
    ACTIONS(47), 1,
      sym__symbol,
    ACTIONS(45), 5,
      ts_builtin_sym_end,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SEMI,
      sym_keyword,
  [33] = 2,
    ACTIONS(51), 1,
      sym__symbol,
    ACTIONS(49), 5,
      ts_builtin_sym_end,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SEMI,
      sym_keyword,
  [44] = 1,
    ACTIONS(53), 1,
      aux_sym__comment_token1,
  [48] = 1,
    ACTIONS(55), 1,
      ts_builtin_sym_end,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(6)] = 0,
  [SMALL_STATE(7)] = 11,
  [SMALL_STATE(8)] = 22,
  [SMALL_STATE(9)] = 33,
  [SMALL_STATE(10)] = 44,
  [SMALL_STATE(11)] = 48,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0, 0, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [11] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [13] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0),
  [15] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(3),
  [18] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(10),
  [21] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(2),
  [24] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(6),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [31] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1, 0, 0),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [37] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_symbol, 1, 0, 0),
  [39] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_symbol, 1, 0, 0),
  [41] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list, 2, 0, 0),
  [43] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_list, 2, 0, 0),
  [45] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__comment, 2, 0, 0),
  [47] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__comment, 2, 0, 0),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list, 3, 0, 0),
  [51] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_list, 3, 0, 0),
  [53] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [55] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef TREE_SITTER_HIDE_SYMBOLS
#define TS_PUBLIC
#elif defined(_WIN32)
#define TS_PUBLIC __declspec(dllexport)
#else
#define TS_PUBLIC __attribute__((visibility("default")))
#endif

TS_PUBLIC const TSLanguage *tree_sitter_sydml(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
