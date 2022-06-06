open Types;

module type ReaderSig = {
  let read: rawProgram => concreteProgramPiece;
  let readAll: rawProgram => concreteProgram;
};

module Reader: ReaderSig = {
  /* I/P: a string, s
   * O/P: a list of characters corresponding to s */
  let charListOfString: string => list(char) =
    str => {
      let rec helper = (str, len, k) =>
        switch (k) {
        | 0 => []
        | i => [str.[len - i], ...helper(str, len, i - 1)]
        };
      helper(str, String.length(str), String.length(str));
    };

  /* I/P: a character, c
   * O/P: Some ((int) c), or None, if c isn't a digit */
  let getDigit = c => {
    let s = Char.code(c) - Char.code('0');
    if (0 <= s && s <= 9) {
      Some(s);
    } else {
      None;
    };
  };

  /* adds a digit, digit, to a number by making it the least sigficant digit */
  let addDigit: (int, int) => int = (digit, number) => number * 10 + digit;

  /* I/P: a char, c
   * O/P: true, if the character is allowable as a Racket identifier
   *      false, otherwise */
  let isLegalIdentifierCharacter: char => bool =
    ch =>
      switch (ch) {
      | ' '
      | '`'
      | '('
      | ')'
      | '['
      | ']'
      | '{'
      | '}'
      | '\\'
      | '|'
      | ';'
      | '\''
      | '"' => false
      | _ => true
      };

  /* I/P: a char list, chars, and current, a quoted_syntax option
   * O/P: the quoted syntax representation of a number or symbol */
  let rec readAtom:
    (list(char), option(concreteProgramPiece)) => concreteProgramPiece =
    (chars, current) =>
      switch (chars, current) {
      | ([], None) => failwith("vacuous expression")
      | ([], Some(cur)) => cur
      | (['-', '-', ...tl], None) => readAtom(tl, Some(SymbolC("--")))
      | (['-'], None) => SymbolC("-")
      | (['-'], Some(x)) =>
        switch (x) {
        | SymbolC(id) => SymbolC(id ++ "-")
        | NumberC(n) => SymbolC(string_of_int(n) ++ "-")
        | ListC(_) => failwith("EMF")
        }
      | (['-', ...tl], None) =>
        switch (readAtom(tl, None)) {
        | NumberC(n) => NumberC(- n)
        | SymbolC(s) => SymbolC("-" ++ s)
        | ListC(_) => failwith("EMF")
        }
      | ([hd, ...tl], None) =>
        switch (getDigit(hd)) {
        | Some(d) => readAtom(tl, Some(NumberC(d)))
        | None =>
          if (isLegalIdentifierCharacter(hd)) {
            readAtom(tl, Some(SymbolC(String.make(1, hd))));
          } else {
            failwith("invalid character identifier");
          }
        }
      | ([hd, ...tl], Some(NumberC(n))) =>
        switch (getDigit(hd)) {
        | None =>
          readAtom(
            tl,
            Some(SymbolC(string_of_int(n) ++ String.make(1, hd))),
          )
        | Some(d) => readAtom(tl, Some(NumberC(addDigit(d, n))))
        }
      | ([hd, ...tl], Some(SymbolC(s))) =>
        if (isLegalIdentifierCharacter(hd)) {
          readAtom(tl, Some(SymbolC(s ++ String.make(1, hd))));
        } else {
          failwith("invalid character identifier");
        }
      | (_, Some(ListC(_))) =>
        failwith("readAtom only handles atomic expressions")
      };

  /* a representation of S-expressions */
  type expressionTree =
    | Terminal(list(char))
    | Expression(list(expressionTree));

  /* trims whitespace from the beginning of a list of characters */
  let trimWhitespace: list(char) => list(char) =
    input => {
      let rec trimLeadingWhitespace: list(char) => list(char) = (
        fun
        | [] => []
        | [' ', ...tl]
        | ['\t', ...tl]
        | ['\n', ...tl] => trimLeadingWhitespace(tl)
        | [hd, ...tl] => [hd, ...tl]:
          list(char) => list(char)
      );
      List.rev(
        trimLeadingWhitespace(List.rev(trimLeadingWhitespace(input))),
      );
    };

  /* I/P: chars, a character list
   * O/P: a token and its remainder */
  let rec untilWhitespace: list(char) => (list(char), list(char)) =
    chars =>
      switch (chars) {
      | [] => ([], [])
      | [' ', ...tl]
      | ['\t', ...tl]
      | ['\n', ...tl] => ([], tl)
      | [')', ..._]
      | ['(', ..._] => ([], chars)
      | [hd, ...tl] =>
        let (chars, rest) = untilWhitespace(tl);
        ([hd, ...chars], rest);
      };

  /* I/P: a list of characters, chars
   * O/P: the expressionTree representation of chars,
   *      followed by the remaining characters */
  let rec treeOfExpression: list(char) => (expressionTree, list(char)) =
    chars => {
      let trimmedChars = trimWhitespace(chars);
      switch (trimmedChars) {
      | [] => (Terminal([]), [])
      | ['(', ...tl] =>
        let (tree_list, rest) = makeTreeList(tl);
        (Expression(tree_list), rest);
      | [_, ..._] =>
        let (symbol, rest) = untilWhitespace(trimmedChars);
        (Terminal(symbol), rest);
      };
    }
  and makeTreeList: list(char) => (list(expressionTree), list(char)) =
    chars => {
      let trimmedChars = trimWhitespace(chars);
      switch (trimmedChars) {
      | [] => failwith("wrong number of parentheses")
      | [')', ...tl] => ([], tl)
      | [_, ..._] =>
        let (tree, rest) = treeOfExpression(chars);
        let (rest_list, rest_rest) = makeTreeList(rest);
        ([tree, ...rest_list], rest_rest);
      };
    };

  /* I/P: expression, a list of characters
   * O/P: the quoted_syntax representation of expression,
   *      followed by the remaining characters */
  let readHelper: list(char) => (concreteProgramPiece, list(char)) =
    expression => {
      let rec readTree = (tree: expressionTree): concreteProgramPiece =>
        switch (tree) {
        | Terminal(chars) => readAtom(chars, None)
        | Expression(trees) => ListC(List.map(readTree, trees))
        };
      let (tree, rest) = treeOfExpression(expression);
      (readTree(tree), rest);
    };

  /* I/P: program, a string
   * O/P: a representation of program as a list of quoted_syntax,
   * 		corresponding to multiple calls to (read) in Racket */
  let read: rawProgram => concreteProgramPiece =
    input => {
      let (quoted, _) = readHelper(charListOfString(input));
      quoted;
    };

  /* I/P: program, a string
   * O/P: a representation of program as a list of quoted_syntax,
   * 		corresponding to multiple calls to (read) in Racket */
  let readAll: rawProgram => concreteProgram =
    input => {
      let rec readAllHelper: list(char) => concreteProgram =
        lst =>
          switch (lst) {
          | [] => []
          | [_, ..._] as chars =>
            let (quoted, rest) = readHelper(chars);
            [quoted, ...readAllHelper(rest)];
          };
      readAllHelper(charListOfString(input));
    };
};