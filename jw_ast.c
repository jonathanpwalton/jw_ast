/*

MIT License

Copyright (c) 2024 Jonathan Walton

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*/

#include "./jw_ast.h"
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#define jw_error(...) do { printf("error: "); printf(__VA_ARGS__); printf("\n"); exit(1); } while(0)
#define jw_assert(X, ...) if (!(X)) { jw_error(__VA_ARGS__); }
#define jw_lexer_error(lexeme, ...) do { fprintf(stderr, JW_LOC_FMT ": error: ", JW_LOC_ARG(lexeme.location)); fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\n"); exit(1); } while(0)
#define jw_lexer_warn(path, ...) do { fprintf(stderr, "%s: warning: ", path); fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\n"); } while(0)

typedef struct jw_char_limits       jw_char_limits;
typedef struct jw_lexeme_rule       jw_lexeme_rule;
typedef struct jw_lexeme_definition jw_lexeme_definition;
typedef struct jw_lexeme            jw_lexeme;
typedef struct jw_lexer*            jw_lexer;

jw_array_typedef(jw_regex, jw_char_limits);
jw_array_typedef(jw_lexeme_rules, jw_lexeme_rule);
jw_array_typedef(jw_lexeme_definitions, jw_lexeme_definition);
jw_array_typedef(jw_lexemes, jw_lexeme);

typedef struct jw_grammar_rule        jw_grammar_rule;
typedef struct jw_grammar_definition  jw_grammar_definition;
typedef struct jw_grammar*            jw_grammar;

jw_array_typedef(jw_grammar_rules, jw_grammar_rule);
jw_array_typedef(jw_grammar_ruleset, jw_grammar_rules);
jw_array_typedef(jw_grammar_definitions, jw_grammar_definition);

jw_array_typedef(jw_asns, jw_asn);
jw_array_typedef(jw_svs, jw_sv);

typedef struct jw_error_message jw_error_message;

jw_array_typedef(jw_error_messages, jw_error_message);

typedef enum
{
  eLexemeRuleUndefined = 0,
  eLexemeRuleString,
  eLexemeRuleRegex,
  eLexemeRuleRegexMore,
  eLexemeRuleRegexMany,
  eLexemeRuleRegexMaybe,
} jw_lexeme_rule_kind;

typedef enum
{
  eGrammarRuleUndefined = 0,
  eGrammarRuleReference,
  eGrammarRuleLexemeKind,
  eGrammarRuleLexemeValue
} jw_grammar_rule_kind;

typedef enum
{
  eGrammarRuleUnmodified = 0,
  eGrammarRuleModMore,
  eGrammarRuleModMany,
  eGrammarRuleModMaybe
} jw_grammar_rule_mod;

struct jw_char_limits
{
  char min;
  char max;
};

struct jw_lexeme_rule
{
  jw_lexeme_rule_kind kind;
  jw_sv               string;
  jw_regex            regex;
  jw_lexeme_rules     subrules;
};

struct jw_lexeme_definition
{
  jw_sv           name;
  jw_lexeme_rules rules;
};

struct jw_lexer
{
  const char*           file;
  char*                 data;
  jw_lexeme_definitions definitions;
};

struct jw_grammar_rule
{
  jw_grammar_rule_kind  kind;
  jw_grammar_rule_mod   mod;
  jw_sv                 string;
};

struct jw_grammar_definition
{
  jw_sv               name;
  jw_grammar_ruleset  ruleset;
};

struct jw_grammar
{
  const char*             file;
  char*                   data;
  jw_grammar_definitions  definitions;
};

struct jw_parser
{
  jw_lexer    lexer;
  jw_grammar  grammar;
  size_t      options;
};

struct jw_lexeme
{
  jw_sv   kind;
  jw_sv   value;
  jw_loc  location;
};

struct jw_asn
{
  char*   data;
  jw_sv   kind;
  jw_sv   value;
  jw_asns children;
  jw_loc  location;
};

struct jw_error_message
{
  jw_loc location;
  char* message;
};

static char*        jw_file_read(const char* path);

static jw_regex     jw_parse_regex(jw_sv regex);
static size_t       jw_regex_matches(jw_regex regex, jw_sv test);

static jw_lexemes   jw_tokenize_lexer(const char* path, const char* content);
static bool         jw_is_lexeme(jw_lexeme lexeme, jw_sv value);
static bool         jw_is_lexeme_kind(jw_lexeme lexeme, jw_sv kind);

static jw_lexer     jw_lexer_new(const char* lexerPath);
static void         jw_lexer_free(jw_lexer lexer);
static jw_lexemes   jw_lexer_use(jw_lexer lexer, const char* path, const char* content, size_t options);

static jw_lexemes   jw_tokenize_grammar(const char* path, const char* data);
static jw_grammar   jw_grammar_new(const char* path);
static void         jw_grammar_free(jw_grammar grammar);

static bool         jw_grammar_definition_use(jw_lexemes lexemes, size_t* currentLexeme, jw_grammar_definitions defs, jw_grammar_definition def, jw_asn* result, size_t level, size_t options, jw_error_messages* errors);

jw_parser jw_parser_new(const char* lexerPath, const char* grammarPath, size_t options)
{
  jw_parser result = calloc(1, sizeof(*result));
  result->lexer = jw_lexer_new(lexerPath);
  result->grammar = jw_grammar_new(grammarPath);
  result->options = options;
  return result;
}

void jw_parser_free(jw_parser parser)
{
  jw_grammar_free(parser->grammar);
  jw_lexer_free(parser->lexer);
  free(parser);
}

jw_asn* jw_ast_new(jw_parser parser, const char* inputPath)
{
  char* inputData = jw_file_read(inputPath);
  jw_lexemes lexemes = jw_lexer_use(parser->lexer, inputPath, inputData, parser->options);
  jw_grammar_definitions defs = parser->grammar->definitions;
  jw_assert(lexemes.length > 0, "cannot create syntax tree from 0 lexemes");
  jw_assert(defs.length > 0, "cannot create syntax tree with a parser that has no definitions");
  
  if (parser->options & JW_AST_DEBUG)
  {
    for (size_t i = 0; i < lexemes.length; i++)
    {
      printf(JW_SV_FMT, JW_SV_ARG(lexemes.data[i].kind));
      if (!jw_sv_eq_sv(lexemes.data[i].kind, jw_csv("newline")))
      {
        printf(" ("JW_SV_FMT")", JW_SV_ARG(lexemes.data[i].value));
      }
      printf("\n");
    }
  }
  
  jw_error_messages errors = {0};
  jw_error_messages furthestFailures = {0};
  jw_asn* result = calloc(1, sizeof(*result));
  result->data = inputData;

  bool failure = false;
  size_t currentLexeme = 0;
  if (!jw_grammar_definition_use(lexemes, &currentLexeme, defs, defs.data[0], result, 0, parser->options, &errors) || currentLexeme != lexemes.length)
  {
    size_t maxRow = 0, maxCol = 0;
    for (size_t i = 0; i < errors.length; i++)
    {
      jw_error_message msg = errors.data[i];

      if (msg.location.row == maxRow && msg.location.col == maxCol)
      {
        jw_array_append(furthestFailures, msg);
      }
      else if (msg.location.row > maxRow || (msg.location.row == maxRow && msg.location.col > maxCol))
      {
        maxRow = msg.location.row;
        maxCol = msg.location.col;
        furthestFailures.length = 0;
        jw_array_append(furthestFailures, msg);
      }
    }

    if (parser->options & JW_AST_FURTHEST_FAILURES)
    {
      for (size_t i = 0; i < furthestFailures.length; i++)
      {
        fprintf(stderr, JW_LOC_FMT": error: %s\n", JW_LOC_ARG(furthestFailures.data[i].location), furthestFailures.data[i].message);
      }
      fprintf(stderr, "\n");
    }
    else if (parser->options & JW_AST_LAST_FURTHEST_FAILURE && furthestFailures.length >= 1)
    {
      fprintf(stderr, JW_LOC_FMT": error: %s\n\n", JW_LOC_ARG(furthestFailures.data[furthestFailures.length - 1].location), furthestFailures.data[furthestFailures.length - 1].message);
    }

    failure = true;
  }

  for (size_t i = 0; i < errors.length; i++)
  {
    free(errors.data[i].message);
  }
  jw_array_free(errors);
  jw_array_free(furthestFailures);

  if (failure)
  {
    fprintf(stderr, JW_LOC_FMT": error: failed to create ast for %s\n", JW_LOC_ARG(lexemes.data[currentLexeme].location), inputPath);
    exit(1);
  }

  return result;
}

void jw_asn_print(jw_asn* node, size_t level)
{
  for (size_t i = 0; i < level; i++)
  {
    printf("  ");
  }

  printf(JW_SV_FMT, JW_SV_ARG(jw_asn_kind(node)));
  if (jw_asn_value(node).data != NULL)
  {
    if (jw_sv_eq(jw_asn_value(node), "\n"))
    {
      printf(" (\\n)");
    }
    else
    {
      printf(" (" JW_SV_FMT ")", JW_SV_ARG(jw_asn_value(node)));
    }
  }
  printf("\n");

  for (size_t i = 0; i < jw_asn_children_count(node); i++)
  {
    jw_asn_print(jw_asn_child(node, i), level + 1);
  }
}

static bool jw_grammar_rule_use(jw_lexemes lexemes, size_t* currentLexeme, jw_grammar_definitions defs, jw_grammar_rule rule, jw_asn* result, size_t level, size_t options, jw_error_messages* errors);
static bool jw_grammar_definition_use(jw_lexemes lexemes, size_t* currentLexeme, jw_grammar_definitions defs, jw_grammar_definition def, jw_asn* result, size_t level, size_t options, jw_error_messages* errors)
{
  if (options & JW_AST_DEBUG)
  {
    for (size_t i = 0; i < level; i++)
    {
      printf("  ");
    }
    printf(JW_SV_FMT": ATTEMPTING\n", JW_SV_ARG(def.name));
  }

  if (*currentLexeme >= lexemes.length)
  {
    if (options & JW_AST_DEBUG)
    {
      for (size_t i = 0; i < level; i++)
      {
        printf("  ");
      }
      printf(JW_SV_FMT": FAILURE\n", JW_SV_ARG(def.name));
    }
    
    return false;
  }

  jw_asn node =
  {
    .data = result->data,
    .kind = def.name,
    .value = {NULL, 0},
    .children = {NULL, 0, 0},
    .location = lexemes.data[*currentLexeme].location
  };

  bool success = false;
  for (size_t i = 0; i < def.ruleset.length; i++)
  {
    success = true;
    jw_grammar_rules rules = def.ruleset.data[i];
    size_t startLexeme = *currentLexeme;

    for (size_t j = 0; j < rules.length; j++)
    {
      jw_grammar_rule rule = rules.data[j];
      jw_asn child = {0};

      switch (rule.mod)
      {
        case eGrammarRuleModMaybe:
        case eGrammarRuleUnmodified:
        {
          if (jw_grammar_rule_use(lexemes, currentLexeme, defs, rule, &child, level + 1, options, errors))
          {
            startLexeme = *currentLexeme;
            jw_array_append(node.children, child);
          }
          else
          {
            *currentLexeme = startLexeme;

            if (rule.mod == eGrammarRuleUnmodified)
            {
              success = false;
            }
          }
          break;
        }

        case eGrammarRuleModMore:
        case eGrammarRuleModMany:
        {
          size_t iterations = 0;
          while (true)
          {
            if (jw_grammar_rule_use(lexemes, currentLexeme, defs, rule, &child, level + 1, options, errors))
            {
              iterations++;
              startLexeme = *currentLexeme;
              jw_array_append(node.children, child);
            }
            else
            {
              *currentLexeme = startLexeme;
              break;
            }
          }

          if (iterations == 0 && rule.mod == eGrammarRuleModMore)
          {
            success = false;
          }
          break;
        }

        default:
          jw_assert(false, "unreachable, %u is not a valid modifier", rule.mod);
          break;
      }

      if (!success)
      {
        break;
      }
    }

    if (success)
    {
      break;
    }
  }

  if (!success)
  {
    if (options & JW_AST_DEBUG)
    {
      for (size_t i = 0; i < level; i++)
      {
        printf("  ");
      }
      printf(JW_SV_FMT": FAILURE\n", JW_SV_ARG(def.name));
    }
    return false;
  }

  if (!jw_sv_eq_sv(def.name, defs.data[0].name) && (options & JW_AST_CHILDLESS_COMPRESSION || options & JW_AST_TOTAL_COMPRESSION))
  {
    if (options & JW_AST_TOTAL_COMPRESSION)
    {
      if (node.children.length == 1)
      {
        if
        (node.children.data[0].value.data != NULL &&
          (
           jw_sv_eq(node.children.data[0].kind, "lexeme-kind") ||
           jw_sv_eq(node.children.data[0].kind, "lexeme-value")
          )
        )
        {
          node.children.data[0].kind = node.kind;
        }
        
        node = node.children.data[0];
      }
    }
    else
    {
      if (node.children.length == 1 && node.children.data[0].children.length == 0)
      {
        node = node.children.data[0];
      }
    }
  }
  *result = node;

  if (options & JW_AST_DEBUG)
  {
    for (size_t i = 0; i < level; i++)
    {
      printf("  ");
    }
    printf(JW_SV_FMT": SUCCESS\n", JW_SV_ARG(def.name));
  }

  return true;
}

static bool jw_grammar_rule_use(jw_lexemes lexemes, size_t* currentLexemeIndex, jw_grammar_definitions defs, jw_grammar_rule rule, jw_asn* result, size_t level, size_t options, jw_error_messages* errors)
{
  if (*currentLexemeIndex >= lexemes.length)
  {
    return false;
  }

  jw_lexeme currentLexeme = lexemes.data[*currentLexemeIndex];
  
  if (options & JW_AST_DEBUG)
  {
    for (size_t i = 0; i < level; i++)
    {
      printf("  ");
    }

    if (!jw_sv_eq(currentLexeme.kind, "newline"))
    {
      printf("%u.%u, "JW_SV_FMT" (current lexeme: "JW_SV_FMT"): ATTEMPTING ... ", rule.kind, rule.mod, JW_SV_ARG(rule.string), JW_SV_ARG(currentLexeme.value));
    }
    else
    {
      printf("%u.%u, "JW_SV_FMT" (current lexeme: newline): ATTEMPTING ... ", rule.kind, rule.mod, JW_SV_ARG(rule.string));
    }
  }

  switch (rule.kind)
  {
    case eGrammarRuleReference:
    {
      for (size_t i = 0; i < defs.length; i++)
      {
        if (jw_sv_eq_sv(rule.string, defs.data[i].name))
        {
          if (options & JW_AST_DEBUG)
          {
            printf("FOUND DEFINITION\n");
          }
          return jw_grammar_definition_use(lexemes, currentLexemeIndex, defs, defs.data[i], result, level + 1, options, errors);
        }
      }

      printf("\n");
      jw_error("failed to find definition for ["JW_SV_FMT"]", JW_SV_ARG(rule.string));
      return false;
    }

    case eGrammarRuleLexemeValue:
    {
      if (!jw_sv_eq_sv(currentLexeme.value, rule.string))
      {
        if (options & JW_AST_DEBUG)
        {
          printf("FAILURE\n");
        }

        char* buffer = calloc(100, sizeof(char));
        sprintf(buffer, "expected '"JW_SV_FMT"' but found '"JW_SV_FMT"'", JW_SV_ARG(rule.string), JW_SV_ARG(currentLexeme.value));
        jw_error_message error = {
          .location = currentLexeme.location,
          .message = buffer
        };
        jw_array_append(*errors, error);

        return false;
      }

      if (options & JW_AST_DEBUG)
      {
        printf("SUCCESS\n");
      }

      result->kind = jw_csv("lexeme-value");
      result->location = currentLexeme.location;
      result->value = currentLexeme.value;
      *currentLexemeIndex = *currentLexemeIndex + 1;
      return true;
    }

    case eGrammarRuleLexemeKind:
    {
      if (!jw_sv_eq_sv(currentLexeme.kind, rule.string))
      {
        if (options & JW_AST_DEBUG)
        {
          printf("FAILURE\n");
        }

        char* buffer = calloc(100, sizeof(char));
        sprintf(buffer, "expected '"JW_SV_FMT"' but found '"JW_SV_FMT"'", JW_SV_ARG(rule.string), JW_SV_ARG(currentLexeme.kind));
        jw_error_message error = {
          .location = currentLexeme.location,
          .message = buffer
        };
        jw_array_append(*errors, error);

        return false;
      }

      if (options & JW_AST_DEBUG)
      {
        printf("SUCCESS\n");
      }

      result->kind = jw_csv("lexeme-kind");
      result->location = currentLexeme.location;
      result->value = currentLexeme.value;
      *currentLexemeIndex = *currentLexemeIndex + 1;
      return true;
    }

    case eGrammarRuleUndefined:
    default:
      jw_assert(false, "unreachable, %u is not a valid grammar rule type", rule.kind);
      return false;
  }
}

static void jw_asn_free(jw_asn* asn)
{
  for (size_t i = 0; i < asn->children.length; i++)
  {
    jw_asn_free(&asn->children.data[i]);
  }

  jw_array_free(asn->children);
}

void jw_ast_free(jw_asn* ast)
{
  jw_assert(ast->data != NULL, "only the root node of the abstract syntax tree may be freed");

  free(ast->data);
  jw_asn_free(ast);
}

size_t jw_asn_children_count(jw_asn* asn)
{
  return asn->children.length;
}

jw_asn* jw_asn_child(jw_asn* asn, size_t index)
{
  jw_assert(index < asn->children.length, "array index out of bounds: %zu is greater than array length of %zu", index, asn->children.length);
  return &asn->children.data[index];
}

jw_sv jw_asn_kind(jw_asn* asn)
{
  return asn->kind;
}

jw_sv jw_asn_value(jw_asn* asn)
{
  return asn->value;
}

jw_loc jw_asn_location(jw_asn* asn)
{
  return asn->location;
}

static char* jw_file_read(const char* path)
{
  FILE* file = fopen(path, "r");
  jw_assert(file != NULL, "failed to open file '%s'", path);

  static const size_t INITIAL_SIZE = 2048;
  char* data = calloc(INITIAL_SIZE, sizeof(char));
  size_t size = 0;
  size_t capacity = INITIAL_SIZE;

  char line[INITIAL_SIZE];
  while (fgets(line, INITIAL_SIZE, file))
  {
    size_t lineSize = strlen(line);

    if (lineSize > 0)
    {
      while (size + lineSize > capacity)
      {
        capacity *= 2;
        data = realloc(data, capacity);
      }

      memcpy(&data[size], line, lineSize);
      size += lineSize;
    }
  }
  fclose(file);
  data = realloc(data, size + 2);
  data[size] = '\n';
  data[size + 1] = 0;
  return data;
}

static jw_regex jw_parse_regex(jw_sv regex)
{
  jw_regex result = {0};

  for (size_t i = 0; i < regex.length; i++)
  {
    char a = regex.data[i];

    if (i + 2 < regex.length && regex.data[i + 1] == '-')
    {
      char b = regex.data[i + 2];
      jw_assert(b > a, "regular expressions of type /a-z/ must have a second character whose ASCII value is greater than the first's. %c is not greater than %c", b, a);

      jw_char_limits limits =
      {
        .min = a,
        .max = b
      };
      jw_array_append(result, limits);

      i += 2;
    }
    else
    {
      jw_char_limits limits =
      {
        .min = a,
        .max = a
      };
      jw_array_append(result, limits);
    }
  }

  return result;
}

static size_t jw_regex_matches(jw_regex regex, jw_sv test)
{
  for (size_t i = 0; i < test.length; i++)
  {
    bool match = false;
    char testChar = test.data[i];

    for (size_t j = 0; j < regex.length; j++)
    {
      jw_char_limits limits = regex.data[j];

      if (limits.min <= testChar && testChar <= limits.max)
      {
        match = true;
        break;
      }
    }

    if (!match)
    {
      return 0;
    }
  }

  return test.length;
}

static jw_lexemes jw_tokenize_lexer(const char* path, const char* file)
{
  jw_lexemes result = {0};

  size_t pos = 0, eof = strlen(file), row = 1, col = 1;
  while (pos != eof)
  {
    if (file[pos] == '\n')
    {
      if (!jw_sv_eq_sv(result.data[result.length - 1].kind, jw_csv("newline")))
      {
        jw_lexeme l =
        {
          .kind = jw_csv("newline"),
          .value = {
            .data = "\n",
            .length = 1
          },
          .location = {
            .file = path,
            .row = row,
            .col = col
          }
        };

        jw_array_append(result, l);
      }

      pos++;
      row++;
      col = 1;
    }
    else if (file[pos] == ' ' || file[pos] == '\t')
    {
      pos++;
      col++;
    }
    else if (file[pos] == '[')
    {
      pos++;
      col++;

      jw_lexeme l =
      {
        .kind = jw_csv("identifier"),
        .value = {
          .data = &file[pos],
          .length = 0
        },
        .location = {
          .file = path,
          .row = row,
          .col = col
        }
      };

      while (file[pos] != ']' && file[pos] != '\n')
      {
        l.value.length++;
        pos++;
        col++;
      }

      if (file[pos] != ']')
      {
        jw_lexer_error(l, "failed to find closing ] for lexeme identifier");
      }

      pos++;
      col++;
      jw_array_append(result, l);
    }
    else if (file[pos] == '\"')
    {
      pos++;
      col++;

      jw_lexeme l =
      {
        .kind = jw_csv("string"),
        .value = {
          .data = &file[pos],
          .length = 0
        },
        .location = {
          .file = path,
          .row = row,
          .col = col
        }
      };

      while (file[pos] != '\"' && file[pos] != '\n')
      {
        l.value.length++;
        pos++;
        col++;
      }

      if (file[pos] != '\"')
      {
        jw_lexer_error(l, "failed to find closing \" for string");
      }

      pos++;
      jw_array_append(result, l);
    }
    else if (file[pos] == '/')
    {
      pos++;
      col++;

      jw_lexeme l =
      {
        .kind = jw_csv("regex"),
        .value = {
          .data = &file[pos],
          .length = 0
        },
        .location = {
          .file = path,
          .row = row,
          .col = col
        }
      };

      while (file[pos] != '/' && file[pos] != '\n')
      {
        l.value.length++;
        pos++;
        col++;
      }

      if (file[pos] != '/')
      {
        jw_lexer_error(l, "failed to find closing / for regular expression");
      }

      pos++;
      col++;

      if (file[pos] == '+')
      {
        l.kind = jw_csv("regex-more");
        pos++;
        col++;
      }
      else if (file[pos] == '?')
      {
        l.kind = jw_csv("regex-maybe");
        pos++;
        col++;
      }
      else if (file[pos] == '*')
      {
        l.kind = jw_csv("regex-many");
        pos++;
        col++;
      }

      jw_array_append(result, l);
    }
    else
    {
      fprintf(stderr, "%s:%zu:%zu: error: out-of-context character '%c'\n", path, row, col, file[pos]);
      exit(1);
    }
  }

  return result;
}

static bool jw_is_lexeme(jw_lexeme lexeme, jw_sv value)
{
  return jw_sv_eq_sv(lexeme.value, value);
}

static bool jw_is_lexeme_kind(jw_lexeme lexeme, jw_sv kind)
{
  return jw_sv_eq_sv(lexeme.kind, kind);
}

static jw_lexeme jw_lexer_expect(jw_lexeme l, const char* value)
{
  if (!jw_is_lexeme(l, jw_csv(value)))
  {
    jw_lexer_error(l, "expected %s but got "JW_SV_FMT, value, JW_SV_ARG(l.value));
  }
  return l;
}

static jw_lexer jw_lexer_new(const char* path)
{
  jw_lexer result = calloc(1, sizeof(*result));
  result->file    = path;
  result->data    = jw_file_read(path);

  jw_lexemes tokens = jw_tokenize_lexer(path, result->data);

  jw_lexeme* file = tokens.data;
  size_t pos = 0, eof = tokens.length;

  jw_lexeme_definition def = {0};

  while (pos != eof)
  {
    if (jw_is_lexeme_kind(file[pos], jw_csv("identifier")))
    {
      if (def.name.data != NULL)
      {
        if (def.rules.length == 0)
        {
          jw_lexer_warn(path, "definition for "JW_SV_FMT" contains no rules", JW_SV_ARG(def.name));
        }

        jw_array_append(result->definitions, def);
        memset(&def, 0, sizeof(def));
      }

      def.name = file[pos].value;

      for (size_t i = 0; i < result->definitions.length; i++)
      {
        if (jw_sv_eq_sv(result->definitions.data[i].name, def.name))
        {
          jw_lexer_error(file[pos], "each lexeme must be uniquely identifiable, multiple definitions found for "JW_SV_FMT, JW_SV_ARG(def.name));
        }
      }

      pos++;

      jw_lexer_expect(file[pos], "\n");
      pos++;
      continue;
    }
    else if (def.name.data != NULL)
    {
      jw_lexeme_rule rule = {0};
      rule.kind = eLexemeRuleUndefined;

      while (!jw_is_lexeme_kind(file[pos], jw_csv("newline")))
      {
        jw_lexeme_rule subrule = {0};

        if (jw_is_lexeme_kind(file[pos], jw_csv("string")))
        {
          subrule.kind = eLexemeRuleString;
          subrule.string = file[pos].value;
          pos++;

          jw_array_append(rule.subrules, subrule);
          continue;
        }
        else if (jw_is_lexeme_kind(file[pos], jw_csv("regex-more")))
        {
          subrule.kind = eLexemeRuleRegexMore;
          subrule.regex = jw_parse_regex(file[pos].value);
          pos++;

          jw_array_append(rule.subrules, subrule);
          continue;
        }
        else if (jw_is_lexeme_kind(file[pos], jw_csv("regex-many")))
        {
          subrule.kind = eLexemeRuleRegexMany;
          subrule.regex = jw_parse_regex(file[pos].value);
          pos++;

          jw_array_append(rule.subrules, subrule);
          continue;
        }
        else if (jw_is_lexeme_kind(file[pos], jw_csv("regex-maybe")))
        {
          subrule.kind = eLexemeRuleRegexMaybe;
          subrule.regex = jw_parse_regex(file[pos].value);
          pos++;

          jw_array_append(rule.subrules, subrule);
          continue;
        }
        else if (jw_is_lexeme_kind(file[pos], jw_csv("regex")))
        {
          subrule.kind = eLexemeRuleRegex;
          subrule.regex = jw_parse_regex(file[pos].value);
          pos++;

          jw_array_append(rule.subrules, subrule);
          continue;
        }
        else
        {
          jw_lexer_error(file[pos], "expected string or regular expression but got "JW_SV_FMT" ("JW_SV_FMT")", JW_SV_ARG(file[pos].kind), JW_SV_ARG(file[pos].value));
        }
      }

      jw_lexer_expect(file[pos], "\n");
      pos++;

      if (rule.subrules.length == 1)
      {
        jw_array_append(def.rules, rule.subrules.data[0]);
      }
      else
      {
        jw_array_append(def.rules, rule);
      }

      continue;
    }

    jw_lexer_error(file[pos], "expected identifier but got "JW_SV_FMT" ("JW_SV_FMT")\n", JW_SV_ARG(file[pos].kind), JW_SV_ARG(file[pos].value));
  }

  if (def.name.data != NULL)
  {
    if (def.rules.length == 0)
    {
      jw_lexer_warn(path, "definition for "JW_SV_FMT" contains no rules", JW_SV_ARG(def.name));
    }

    jw_array_append(result->definitions, def);
  }

  return result;
}

static bool jw_char_delimits(char c)
{
  return !isalpha(c) && c != '_';
}

static size_t jw_lexeme_rule_try(jw_lexeme_rule rule, const char* str, size_t len)
{
  switch (rule.kind)
  {
    case eLexemeRuleString:
    {
      bool matches = strncmp(rule.string.data, str, rule.string.length) == 0;

      if (matches)
      {
        if (jw_char_delimits(rule.string.data[rule.string.length - 1]))
        {
          return rule.string.length;
        }

        if (len == rule.string.length || jw_char_delimits(str[rule.string.length]))
        {
          return rule.string.length;
        }
      }

      return 0;
    }

    case eLexemeRuleRegex:
    case eLexemeRuleRegexMany:
    case eLexemeRuleRegexMore:
    case eLexemeRuleRegexMaybe:
    {
      size_t length = 0;

      while (true)
      {
        if (length == len)
        {
          break;
        }

        jw_sv test =
        {
          .data = str,
          .length = length + 1
        };

        if (jw_regex_matches(rule.regex, test) > 0)
        {
          length++;
        }
        else
        {
          break;
        }
      }


      if (len == length || jw_char_delimits(str[length - 1]) || jw_char_delimits(str[length]))
      {
        return length;
      }

      return 0;
    }

    case eLexemeRuleUndefined:
    {
      size_t used = 0;

      for (size_t i = 0; i < rule.subrules.length; i++)
      {
        jw_lexeme_rule subrule = rule.subrules.data[i];
        size_t subused = jw_lexeme_rule_try(subrule, &str[used], len - used);
        used += subused;

        if (subused == 0)
        {
          if (subrule.kind == eLexemeRuleRegexMany || subrule.kind == eLexemeRuleRegexMaybe)
          {
            break;
          }

          return 0;
        }
      }

      return used;
    }

    default:
      assert(false && "unreachable");
      return 0;
  }
}

static jw_lexemes jw_lexer_use(jw_lexer lexer, const char* path, const char* content, size_t options)
{
  jw_lexemes result = {0};

  size_t pos = 0, eof = strlen(content), row = 1, col = 1;
  while (pos < eof)
  {
    if (content[pos] == '\n')
    {
      if ((options & JW_AST_NEWLINE) && !jw_sv_eq_sv(result.data[result.length - 1].kind, jw_csv("newline")))
      {
        jw_lexeme l =
        {
          .kind = jw_csv("newline"),
          .value = {
            .data = "\n",
            .length = 1
          },
          .location = {
            .file = path,
            .row = row,
            .col = col
          }
        };

        jw_array_append(result, l);
      }

      pos++;
      row++;
      col = 1;

      continue;
    }

    if (content[pos] == ' ')
    {
      if ((options & JW_AST_INDENT) && col == 1)
      {
        jw_lexeme l =
        {
          .kind = jw_csv("indent"),
          .value = {
            .data = &content[pos],
            .length = 0
          },
          .location = {
            .file = path,
            .row = row,
            .col = col
          }
        };

        while (content[pos] == ' ')
        {
          pos++;
          col++;
          l.value.length++;
        }

        if (content[pos] != '\n')
        {
          jw_array_append(result, l);
        }
      }
      else
      {
        pos++;
        col++;
      }

      continue;
    }

    if (content[pos] == '\"')
    {
      jw_lexeme l =
      {
        .kind = jw_csv("string"),
        .value = {
          .data = &content[pos],
          .length = 1
        },
        .location = {
          .file = path,
          .row = row,
          .col = col
        }
      };
      pos++;
      col++;

      bool escaping = false;
      while ((escaping || content[pos] != '\"') && content[pos] != '\n')
      {
        if (content[pos] == '\\' && !escaping)
        {
          escaping = true;
        }
        else
        {
          escaping = false;
        }

        pos++;
        col++;
        l.value.length++;
      }

      if (content[pos] != '\"')
      {
        jw_lexer_error(l, "failed to find closing \" for string literal");
      }

      pos++;
      col++;
      l.value.length++;
      jw_array_append(result, l);
      continue;
    }

    bool found = false;
    for (size_t i = 0; i < lexer->definitions.length; i++)
    {
      jw_lexeme_definition def = lexer->definitions.data[i];
      bool flag = false;

      for (size_t j = 0; j < def.rules.length; j++)
      {
        jw_lexeme_rule rule = def.rules.data[j];
        size_t used = jw_lexeme_rule_try(rule, &content[pos], eof - pos);

        if (used != 0)
        {
          jw_lexeme l =
          {
            .kind = def.name,
            .value = {
              .data = &content[pos],
              .length = used
            },
            .location = {
              .file = path,
              .row = row,
              .col = col
            }
          };
          jw_array_append(result, l);

          pos += used;
          col += used;

          flag = true;
          break;
        }
      }

      if (flag)
      {
        found = true;
        break;
      }
    }

    if (found)
    {
      continue;
    }
    else
    {
      fprintf(stderr, "%s:%zu:%zu: substring starting with character '%c' does not match any defined rule\n", path, row, col, content[pos]);
      exit(1);
    }
  }

  return result;
}

static void jw_lexeme_rule_free(jw_lexeme_rule rule)
{
  for (size_t i = 0; i < rule.subrules.length; i++)
  {
    jw_lexeme_rule_free(rule.subrules.data[i]);
  }

  jw_array_free(rule.subrules);
  jw_array_free(rule.regex);
}

static void jw_lexeme_definition_free(jw_lexeme_definition def)
{
  for (size_t i = 0; i < def.rules.length; i++)
  {
    jw_lexeme_rule_free(def.rules.data[i]);
  }

  jw_array_free(def.rules);
}

static void jw_lexer_free(jw_lexer lexer)
{
  for (size_t i = 0; i < lexer->definitions.length; i++)
  {
    jw_lexeme_definition_free(lexer->definitions.data[i]);
  }

  jw_array_free(lexer->definitions);
  free(lexer);
}

static jw_lexemes jw_tokenize_grammar(const char* path, const char* data)
{
  jw_lexemes result = {0};

  size_t pos = 0, eof = strlen(data), row = 1, col = 1;

  while (pos != eof)
  {
    if (data[pos] == '\n')
    {
      if (!jw_sv_eq_sv(result.data[result.length - 1].kind, jw_csv("newline")))
      {
        jw_lexeme l =
        {
          .kind = jw_csv("newline"),
          .value = {
            .data = "\n",
            .length = 1
          },
          .location = {
            .file = path,
            .row = row,
            .col = col
          }
        };

        jw_array_append(result, l);
      }

      pos++;
      row++;
      col = 1;

      continue;
    }

    if (data[pos] == ' ')
    {
      if (col == 1)
      {
        jw_lexeme l =
        {
          .kind = jw_csv("indent"),
          .value = {
            .data = &data[pos],
            .length = 0
          },
          .location = {
            .file = path,
            .row = row,
            .col = col
          }
        };

        while (data[pos] == ' ')
        {
          pos++;
          col++;
          l.value.length++;
        }

        if (data[pos] != '\n')
        {
          jw_array_append(result, l);
        }
      }
      else
      {
        pos++;
        col++;
      }

      continue;
    }

    if (data[pos] == '<')
    {
      pos++;
      col++;

      jw_lexeme l =
      {
        .kind = jw_csv("rule"),
        .value = {
          .data = &data[pos],
          .length = 0
        },
        .location = {
          .file = path,
          .row = row,
          .col = col
        }
      };

      while (data[pos] != '>' && data[pos] != '\n')
      {
        l.value.length++;
        pos++;
        col++;
      }

      if (data[pos] != '>')
      {
        jw_lexer_error(l, "failed to find closing > for rule identifier");
      }

      pos++;
      col++;

      if (data[pos] == '+')
      {
        l.kind = jw_csv("rule-more");
        pos++;
        col++;
      }
      else if (data[pos] == '?')
      {
        l.kind = jw_csv("rule-maybe");
        pos++;
        col++;
      }
      else if (data[pos] == '*')
      {
        l.kind = jw_csv("rule-many");
        pos++;
        col++;
      }

      jw_array_append(result, l);

      continue;
    }

    if (data[pos] == '\"')
    {
      pos++;
      col++;

      jw_lexeme l =
      {
        .kind = jw_csv("lexeme-value"),
        .value = {
          .data = &data[pos],
          .length = 0
        },
        .location = {
          .file = path,
          .row = row,
          .col = col
        }
      };

      while (data[pos] != '\"' && data[pos] != '\n')
      {
        l.value.length++;
        pos++;
        col++;
      }

      if (data[pos] != '\"')
      {
        jw_lexer_error(l, "failed to find closing \" for string");
      }

      pos++;
      col++;
      jw_array_append(result, l);

      continue;
    }

    if (data[pos] == '[')
    {
      pos++;
      col++;

      jw_lexeme l =
      {
        .kind = jw_csv("lexeme-kind"),
        .value = {
          .data = &data[pos],
          .length = 0
        },
        .location = {
          .file = path,
          .row = row,
          .col = col
        }
      };

      while (data[pos] != ']' && data[pos] != '\n')
      {
        l.value.length++;
        pos++;
        col++;
      }

      if (data[pos] != ']')
      {
        jw_lexer_error(l, "failed to find closing ] for lexeme identifier");
      }

      pos++;
      col++;
      jw_array_append(result, l);

      continue;
    }

    if (data[pos] == 0)
    {
      break;
    }

    if (data[pos] == 13)
    {
      pos++;
      continue;
    }

    fprintf(stderr, "%s:%zu:%zu: error: out-of-context character '%c' (%s)\n", path, row, col, data[pos], &data[pos]);
    exit(1);
  }

  return result;
}


static jw_grammar jw_grammar_new(const char* path)
{
  jw_grammar result = calloc(1, sizeof(*result));
  result->file     = path;
  result->data     = jw_file_read(path);

  jw_lexemes tokens = jw_tokenize_grammar(path, result->data);
  jw_lexeme* file = tokens.data;
  size_t pos = 0, eof = tokens.length;

  while (pos != eof)
  {
    if (jw_is_lexeme_kind(file[pos], jw_csv("rule")))
    {
      jw_grammar_definition def =
      {
        .name = file[pos].value,
        .ruleset = {NULL, 0, 0}
      };
      pos++;

      for (size_t i = 0; i < result->definitions.length; i++)
      {
        if (jw_sv_eq_sv(result->definitions.data[i].name, def.name))
        {
          jw_lexer_error(file[pos - 1], "each rule must be uniquely identifiable, multiple definitions found for "JW_SV_FMT, JW_SV_ARG(def.name));
        }
      }

      jw_lexer_expect(file[pos], "\n");
      pos++;

      while (pos != eof && jw_is_lexeme_kind(file[pos], jw_csv("indent")))
      {
        pos++;

        jw_grammar_rules rules = {0};

        while (!jw_is_lexeme_kind(file[pos], jw_csv("newline")))
        {

          if (jw_is_lexeme_kind(file[pos], jw_csv("rule")))
          {
            jw_grammar_rule rule =
            {
              .kind = eGrammarRuleReference,
              .mod = eGrammarRuleUnmodified,
              .string = file[pos].value
            };
            pos++;

            jw_array_append(rules, rule);
            continue;
          }

          if (jw_is_lexeme_kind(file[pos], jw_csv("rule-many")))
          {
            jw_grammar_rule rule =
            {
              .kind = eGrammarRuleReference,
              .mod = eGrammarRuleModMany,
              .string = file[pos].value
            };
            pos++;

            jw_array_append(rules, rule);
            continue;
          }

          if (jw_is_lexeme_kind(file[pos], jw_csv("rule-more")))
          {
            jw_grammar_rule rule =
            {
              .kind = eGrammarRuleReference,
              .mod = eGrammarRuleModMore,
              .string = file[pos].value
            };
            pos++;

            jw_array_append(rules, rule);
            continue;
          }

          if (jw_is_lexeme_kind(file[pos], jw_csv("rule-maybe")))
          {
            jw_grammar_rule rule =
            {
              .kind = eGrammarRuleReference,
              .mod = eGrammarRuleModMaybe,
              .string = file[pos].value
            };
            pos++;

            jw_array_append(rules, rule);
            continue;
          }

          if (jw_is_lexeme_kind(file[pos], jw_csv("lexeme-value")))
          {
            jw_grammar_rule rule =
            {
              .kind = eGrammarRuleLexemeValue,
              .mod = eGrammarRuleUnmodified,
              .string = file[pos].value
            };
            pos++;

            jw_array_append(rules, rule);
            continue;
          }

          if (jw_is_lexeme_kind(file[pos], jw_csv("lexeme-kind")))
          {
            jw_grammar_rule rule =
            {
              .kind = eGrammarRuleLexemeKind,
              .mod = eGrammarRuleUnmodified,
              .string = file[pos].value
            };
            pos++;

            jw_array_append(rules, rule);
            continue;
          }

          jw_lexer_error(file[pos], "expected rule reference, lexeme reference, or lexeme literal but got "JW_SV_FMT" ("JW_SV_FMT")", JW_SV_ARG(file[pos].kind), JW_SV_ARG(file[pos].value));
        }

        jw_lexer_expect(file[pos], "\n");
        pos++;

        jw_array_append(def.ruleset, rules);
        continue;
      }

      if (def.ruleset.length == 0)
      {
        jw_lexer_warn(path, "definition for "JW_SV_FMT" contains no rules", JW_SV_ARG(def.name));
      }

      jw_array_append(result->definitions, def);
      continue;
    }

    jw_lexer_error(file[pos], "expected rule definition but got "JW_SV_FMT" ("JW_SV_FMT")", JW_SV_ARG(file[pos].kind), JW_SV_ARG(file[pos].value));
  }

#ifdef JW_PRINT_GRAMMAR
  for (size_t i = 0; i < result->definitions.length; i++)
  {
    jw_grammar_definition def = result->definitions.data[i];

    printf(JW_SV_FMT":\n", JW_SV_ARG(def.name));

    for (size_t j = 0; j < def.ruleset.length; j++)
    {
      jw_grammar_rules rules = def.ruleset.data[j];
      printf("  (");

      for (size_t k = 0; k < rules.length; k++)
      {
        jw_grammar_rule rule = rules.data[k];

        printf("%u.%u ("JW_SV_FMT")", rule.kind, rule.mod, JW_SV_ARG(rule.string));

        if (k != rules.length - 1)
        {
          printf(", ");
        }
      }

      printf(")\n");
    }
  }
#endif

  return result;
}

static void jw_grammar_definition_free(jw_grammar_definition def)
{
  for (size_t i = 0; i < def.ruleset.length; i++)
  {
    jw_array_free(def.ruleset.data[i]);
  }
  jw_array_free(def.ruleset);
}

static void jw_grammar_free(jw_grammar grammar)
{
  for (size_t i = 0; i < grammar->definitions.length; i++)
  {
    jw_grammar_definition_free(grammar->definitions.data[i]);
  }
  jw_array_free(grammar->definitions);
  free(grammar->data);
  free(grammar);
}

