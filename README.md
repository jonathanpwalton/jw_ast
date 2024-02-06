# jw_ast
Lightweight C library for the creation of abstract syntax trees for arbitrary grammars loosely based on Backus-Naur form.

### Quick start
```c
jw_parser parser = jw_parser_new("path/to/lexer", "path/to/grammar", 0);
jw_asn* ast = jw_ast_new(parser, "path/to/file/to/parse");

jw_asn_print(ast, 0);

jw_ast_free(ast);
jw_parser_free(parser);
```

### Lexer creation

Lexers are defined in their own files for the most part, but a few options are available when creating a ```jw_parser``` object. By default, jw_ast does not tokenize newline characters or whitespace. In order to enable tokenization of a subset of these characters, the following options may be utilized upon the creation of a parser, as shown below. At the moment, tokenization of intra-line whitespace is not possible. Indent tokens are created when either ```\n``` or ```\t``` characters are found at the start of a line. Newline tokens are created at the end of each line as well as the end of the file, even if a newline is not present. Additionally, multiple newlines and lines with only whitespace are collapsed.

**Creating a parser that tokenizes both newlines and indents**

```c
jw_parser parser = jw_parser_new("path/to/lexer", "path/to/grammar", JW_AST_INDENT | JW_AST_NEWLINE);
```

**Lexer definitions**

Much like the creation of a grammar shown below, a lexer is composed of a series of lexeme definitions that can contain multiple rules. At the moment, string literals and naive regular expressions are available.

A lexeme's definition first begins by choosing a name and surrounding it in square brackets, as shown below. The name chosen here is important, because it can be used later in the creation of your grammar's rules.

```
[operator]
```

Rules for this lexeme are defined on subsequent lines. During tokenization, any substrings matching the rules defined here will be considered as a token of this type (provided proper delimitation is followed).

```
[operator]
  "+"
  "-"
  "*"
  "/"
```

Lexemes with a single rule are also possible, as shown here. This regular expression will match one or more alpha characters in a row.

```
[word]
  /a-zA-Z/+
```

A lexer defined with both of these rules:

```
[operator]
  "+"
  "-"
  "*"
  "/"

[word]
  /a-zA-Z/+
```

would be able to tokenize the following input:

```
the Big dog + cat * Horse - zebra / ELEPHANT tangerine
```

but would fail to tokenize this input due to the presence of uncategorized characters (```=```):

```
the Big dog + cat * Horse - zebra / ELEPHANT = tangerine
```

### Grammar creation

Like the creation of a lexer, a grammar is defined in its own file and all rules are defined therein. No subsequent generalizations are made by this implementation. Rules are defined like lexemes, but with triangular brackets instead. ***It is important that the first definition within your grammar encompasses the entirety of a file.***

This grammar will create a root node called ```book``` which is defined as one or more ```chapter``` nodes. Each chapter node is thus made of one or more ```paragraph``` nodes. These nodes are in turn defined as one or more ```sentence``` nodes followed by a ```newline``` **lexeme**. Each sentence is one or more ```word``` **lexemes** followed by a ```.``` **lexeme literal**.

```
<book>
  <chapter>+

<chapter>
  <paragraph>+

<paragraph>
  <sentence>+ [newline]

<sentence>
  [word]+ "."
```

**Creating purposeful grammars**

Grammar creation is a nuanced topic but this parser relies on a couple of assumptions:
1. No left-recursion. This means that a grammar definition must not include itself on the ***left*** side of its definition.
2. Meaningful delimiting of nodes. The above grammar is missing a key aspect which delimits each chapter from the next. Without something (a ```grammar rule reference``` or a ```lexeme value or literal```) that delimits between chapters, who's to say which paragraph belongs to which chapter?
   a. To fix this problem, a chapter should include something that delimits it. This can be done in several ways, but an easy way would be to look for a ***lexeme literal*** with the value of ```Chapter``` followed by a ***lexeme*** with kind ```number```, as shown here:

   ```
   <book>
     <chapter>+

   <chapter>
     "Chapter" [number] [newline] <paragraph>+

   <paragraph>
     <sentence>+ [newline]

   <sentence>
     [word]+ "."
   ```

   This of course would require the definition of a new lexeme whose name is ```number``` in our lexer, as shown here:

   ```
   [number]
     /0-9/+
   ```

### Limitations

Currently, the error-handling of failed rules is rather limited. If the current rule exists within a larger structure of rules and fails, the topmost rule will fail (unless that current rule is a ```maybe``` (e.g. ```<word>?```) or ```many``` (e.g. ```<word>*```) rule).
