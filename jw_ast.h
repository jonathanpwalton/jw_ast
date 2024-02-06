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

#ifndef JW_AST_H_
#define JW_AST_H_

#include <stdint.h>

#ifndef JW_DEFINE_SV_
#define JW_DEFINE_SV_

#include <string.h>

typedef struct
{
  const char* data;
  size_t      length;
} jw_sv;

#define jw_csv(cstr) ((jw_sv) {cstr, strlen(cstr)})
#define jw_sv_eq_sv(a, b) (strncmp(a.data, b.data, a.length > b.length ? a.length : b.length) == 0)
#define jw_sv_eq(sv, cstr) (jw_sv_eq_sv(sv, jw_csv(cstr)))

#define JW_SV_FMT "%.*s"
#define JW_SV_ARG(sv) (int) sv.length, sv.data

#endif //JW_DEFINE_SV_

#ifndef JW_DEFINE_LOC_
#define JW_DEFINE_LOC_

typedef struct
{
  const char* file;
  size_t      row;
  size_t      col;
} jw_loc;

#define JW_LOC_FMT "%s:%zu:%zu"
#define JW_LOC_ARG(loc) loc.file, loc.row, loc.col

#endif //JW_DEFINE_LOC_

typedef enum
{
  JW_AST_INDENT                 = 0x1,
  JW_AST_NEWLINE                = 0x2,
  JW_AST_CHILDLESS_COMPRESSION  = 0x4,
  JW_AST_TOTAL_COMPRESSION      = 0x8,
  JW_AST_DEBUG                  = 0x10,
} JW_AST_OPTIONS_ENUM;

typedef struct jw_parser* jw_parser;
typedef struct jw_asn jw_asn;

jw_parser jw_parser_new(const char* lexerPath, const char* grammarPath, size_t optionsBitfield);
void      jw_parser_free(jw_parser parser);

jw_asn* jw_ast_new(jw_parser parser, const char* inputPath);
void    jw_ast_free(jw_asn* ast);

size_t  jw_asn_children_count(jw_asn* asn);
jw_asn* jw_asn_child(jw_asn* asn, size_t index);
jw_sv   jw_asn_kind(jw_asn* asn);
jw_sv   jw_asn_value(jw_asn* asn);
jw_loc  jw_asn_location(jw_asn* asn);

#ifndef JW_DEFINE_MINMAX_
#define JW_DEFINE_MINMAX_

#define jw_min(a, b) (a < b ? a : b)
#define jw_max(a, b) (a > b ? a : b)

#endif //JW_DEFINE_MINMAX_

#ifndef JW_DEFINE_ARRAY_
#define JW_DEFINE_ARRAY_

#include <stdlib.h>

#define jw_array_typedef(name, type)\
  typedef struct name\
{\
  type* data;\
  size_t length;\
  size_t capacity;\
} name

#define jw_array_append(arr, val)\
  do\
{\
  if (arr.capacity <= arr.length)\
  {\
    arr.capacity += jw_max(1, arr.capacity / 2);\
    arr.data = realloc(arr.data, arr.capacity * sizeof(val));\
  }\
  arr.data[arr.length++] = val;\
} while(0)

#define jw_array_free(arr) free(arr.data)

#endif //JW_DEFINE_ARRAY_

#endif //JW_AST_H

