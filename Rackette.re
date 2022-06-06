open CS17SetupRackette;
open Read.Reader;
open Types;

/* initialTLE produces the top-level environment with all builtin procedures
   as described in the Rackette grammar */
let initialTle: environment = {
  /* input: va, a list exactly two NumV values
     output: a NumV value whose argument is equal to the sum of the arguments
     of the input values */
  let plus: list(value) => value =
    va =>
      switch (va) {
      | [NumV(int1), NumV(int2)] => NumV(int1 + int2)
      | [_, _] => failwith("Wrong value type for +")
      | _ => failwith("+ expects 2 arguments")
      };
  /* input: va, a list of exactly two NumV values
     output: a NumV value whose argument is equal to the difference between the
     arguments of the input values */
  let minus: list(value) => value =
    va =>
      switch (va) {
      | [NumV(int1), NumV(int2)] => NumV(int1 - int2)
      | [_, _] => failwith("Wrong value type for -")
      | _ => failwith("- expects 2 arguments")
      };
  /* input: va, a list of exactly two NumV values
     output: a NumV value whose argument is equal to the product of the
     arguments of the input values */
  let multiply: list(value) => value =
    va =>
      switch (va) {
      | [NumV(int1), NumV(int2)] => NumV(int1 * int2)
      | [_, _] => failwith("Wrong value type for *")
      | _ => failwith("* expects 2 arguments")
      };
  /* input: va, a list of exactly two NumV values
     output: a NumV value whose argument is equal to the quotient of the
     arguments of the input values */
  let divide: list(value) => value =
    va =>
      switch (va) {
      | [NumV(int1), NumV(int2)] => NumV(int1 / int2)
      | [_, _] => failwith("Wrong value type for /")
      | _ => failwith("/ expects 2 arguments")
      };
  /* input: va, a list of exactly two NumV values
     output: a NumV value whose argument is equal to the remainder of the
     arguments of the input values */
  let rem: list(value) => value =
    va =>
      switch (va) {
      | [NumV(int1), NumV(int2)] => NumV(int1 mod int2)
      | [_, _] => failwith("Wrong value type for remainder")
      | _ => failwith("remainder expects 2 arguments")
      };
  /* input: va, a list of exactly two NumV values
     output: a BoolV value whose argument is true when the arguments of the
     input values are equal integers and false if they are not equal integers,
     error otherwise */
  let equalInt: list(value) => value =
    va =>
      switch (va) {
      | [NumV(int1), NumV(int2)] => BoolV(int1 == int2)
      | [_, _] => failwith("Wrong value type for =")
      | _ => failwith("= expects 2 arguments")
      };
  /* input: va, a list of exactly two NumV values
     output: a BoolV value whose argument is true if the argument of the first
     value, from the input list, is less than the argument of the second value
     and false if not, error otherwise  */
  let lssThn: list(value) => value =
    va =>
      switch (va) {
      | [NumV(int1), NumV(int2)] => BoolV(int1 < int2)
      | [_, _] => failwith("Wrong value type for <")
      | _ => failwith("< expects 2 arguments")
      };
  /* input: va, a list of exactly two NumV values
     output: a BoolV value whose argument is true if the argument of the first
     value, from the input list, is greater than the argument of the second
     value and false if not, error otherwise */
  let grtrThn: list(value) => value =
    va =>
      switch (va) {
      | [NumV(int1), NumV(int2)] => BoolV(int1 > int2)
      | [_, _] => failwith("Wrong value type for >")
      | _ => failwith("> expects 2 arguments")
      };
  /* input: va, a list of exactly two NumV values
     output: a BoolV value whose argument is true if the argument of the first
     value, from the input list, is less than or equal to the argument of the
     second value and false if not, error otherwise */
  let lessOrEqual: list(value) => value =
    va =>
      switch (va) {
      | [NumV(int1), NumV(int2)] => BoolV(int1 <= int2)
      | [_, _] => failwith("Wrong value type for <=")
      | _ => failwith("<= expects 2 arguments")
      };
  /* input: va, a list of exactly two NumV values
     output: a BoolV value whose argument is true if the argument of the first
     value, from the input list, is greater than the argument of the
     second value and false if not, error otherwise */
  let greaterOrEqual: list(value) => value =
    va =>
      switch (va) {
      | [NumV(int1), NumV(int2)] => BoolV(int1 >= int2)
      | [_, _] => failwith("Wrong value type for >=")
      | _ => failwith(">= expects 2 arguments")
      };
  /* input: va, a list of exactly two values
     output: a BoolV value whose argument is true if the argument of the first
     value, from the input list, is equal to the argument of the second value
     and false if not */
  let equalP: list(value) => value =
    va =>
      switch (va) {
      | [NumV(int1), NumV(int2)] => BoolV(int1 == int2)
      | [BoolV(bool1), BoolV(bool2)] => BoolV(bool1 == bool2)
      | [ListV(list1), ListV(list2)] => BoolV(list1 == list2)
      | [_, _] => BoolV(false)
      | _ => failwith("equal? expects 2 arguments")
      };
  /* input: va, a list of one value
     output: a BoolV value whose argument is true if the input is a NumV value
     and false otherwise */
  let nmbrP: list(value) => value =
    va =>
      switch (va) {
      | [NumV(_)] => BoolV(true)
      | [_] => BoolV(false)
      | _ => failwith("number? expects 1 argument")
      };
  /* input: va, a list of one NumV
     output: a BoolV value whose argument is true if the input is a NumV value
     and its argument is 0; otherwise the agrument is false */
  let zeroP: list(value) => value =
    va =>
      switch (va) {
      | [NumV(int)] => BoolV(int == 0)
      | [_] => failwith("zero? expects a number")
      | _ => failwith("zero? expects 1 argument")
      };
  /* input: va, a list of two values, one of which is a value and the other is
     a listV value
     output: a ListV value whose argument is a ListV containing the first
     argument in input added in front of the second list argument*/
  let cons: list(value) => value =
    va =>
      switch (va) {
      | [item, ListV(items)] => ListV([item, ...items])
      | [_, _] => failwith("Wrong value type for cons")
      | _ => failwith("cons expects 2 arguments")
      };
  /* input: va, a listV value with an agrument that is a list that contains at
     least one value
     output: a value that is the first item of the argument list */
  let first: list(value) => value =
    va =>
      switch (va) {
      | [ListV([hd, ..._tl])] => hd
      | [ListV([])] => failwith("first expects a non-empty list")
      | [_] => failwith("Wrong value type for first")
      | _ => failwith("first expects 1 argument")
      };
  /* input: va, a listV value with an argument that is a list that contains at
     least one value
     output: a ListV value whose argument is a list containing all the elements
     of the input argment list except for the first item */
  let rest: list(value) => value =
    va =>
      switch (va) {
      | [ListV([_hd, ...tl])] => ListV(tl)
      | [ListV([])] => failwith("rest expects a non-empty list")
      | [_] => failwith("Wrong value type for rest")
      | _ => failwith("rest expects 1 argument")
      };
  /* input: va, a listV value with an argument that is a list
     output: a BoolV value that is true if the argument list is empty and false
     otherwise */
  let emptyP: list(value) => value =
    va =>
      switch (va) {
      | [ListV([])] => BoolV(true)
      | [_] => BoolV(false)
      | _ => failwith("empty? expects 1 argument")
      };
  /* input: va, a listV value with an agrument that is a list
     output: a BoolV value that is true if the argument list contains at least
     one item and false if it is empty */
  let consP: list(value) => value =
    va =>
      switch (va) {
      | [ListV([_hd, ..._tl])] => BoolV(true)
      | [_] => BoolV(false)
      | _ => failwith("cons? expects 1 argument")
      };
  /* input: va, a list containg a BoolV value
     output: a BoolV value that is the opposite of the input BoolV value */
  let not: list(value) => value =
    va =>
      switch (va) {
      | [BoolV(bool)] => BoolV(!bool)
      | [_] => failwith("Wrong value type for not")
      | _ => failwith("not expects 1 argument")
      };
  /* input: va, a list of values
      output: all the values from the input list in a ListV value
     argument list*/
  let list: list(value) => value = va => ListV(va);
  [
    (Name("+"), BuiltinV({bName: "<builtin: + >", bProc: plus})),
    (Name("-"), BuiltinV({bName: "<builtin: - >", bProc: minus})),
    (Name("*"), BuiltinV({bName: "<builtin: * >", bProc: multiply})),
    (Name("/"), BuiltinV({bName: "<builtin: / >", bProc: divide})),
    (
      Name("remainder"),
      BuiltinV({bName: "<builtin: Remainder>", bProc: rem}),
    ),
    (Name("="), BuiltinV({bName: "<builtin: = >", bProc: equalInt})),
    (Name("<"), BuiltinV({bName: "<builtin: < >", bProc: lssThn})),
    (Name(">"), BuiltinV({bName: "<builtin: > >", bProc: grtrThn})),
    (Name("<="), BuiltinV({bName: "<builtin: <= >", bProc: lessOrEqual})),
    (
      Name(">="),
      BuiltinV({bName: "<builtin: >= >", bProc: greaterOrEqual}),
    ),
    (Name("equal?"), BuiltinV({bName: "<builtin: equal?>", bProc: equalP})),
    (
      Name("number?"),
      BuiltinV({bName: "<builtin: number?>", bProc: nmbrP}),
    ),
    (Name("zero?"), BuiltinV({bName: "<builtin: zero?>", bProc: zeroP})),
    (Name("cons"), BuiltinV({bName: "<builtin: cons>", bProc: cons})),
    (Name("first"), BuiltinV({bName: "<builtin: first>", bProc: first})),
    (Name("rest"), BuiltinV({bName: "<builtin: rest>", bProc: rest})),
    (Name("empty?"), BuiltinV({bName: "<builtin: empty?>", bProc: emptyP})),
    (Name("cons?"), BuiltinV({bName: "<builtin: cons?>", bProc: consP})),
    (Name("not"), BuiltinV({bName: "<builtin: not>", bProc: not})),
    (Name("list"), BuiltinV({bName: "<builtin: list>", bProc: list})),
  ];
};

/* parseExpression:
   input: input, a concreteProgramPiece that is not a definition
      output: an abstractProgramPiece that is the result of parsing input

   Recursion diagram 1:
   OI:ListC([NumberC(5), NumberC(23), NumberC(45)])
    RI: [NumberC(23), NumberC(45)]
    RO: [NumE(23), NumC(45)]
   Ideas: map parseExpression onto the RI and then
    slap ApplicationE([NumE(5)]), onto the front of that list
   OO: ApplicationE([NumE(5), NumE(23), NumC(45)])

   Recursion diagram 2:
   OI: ListC([SymbolC("and"), SymbolC("true"), SymbolC("false")])
    RI: [SymbolC("true"), SymbolC("false")])
    RO: (BoolE(true), BoolE(false))
   Ideas: Put the recursive output inside AndE()
   OO: AndE(BoolE(true), BoolE(false))
   */

let rec parseExpression: concreteProgramPiece => expression =
  input => {
    /* input: input, a list of concreteProgramPieces consisting of two item
       lists of concreteProgramPieces

       output: a list of condData records where the first item of each two item
       list in the input is now an expression in the conditionExpr field and
       item2 of each two item list is an expression in resultExpr field. Each
       is comprised of one two item list  */
    let rec condHelper: list(concreteProgramPiece) => list(condData) =
      input =>
        switch (input) {
        | [] => []
        | [ListC([item1, item2]), ...tl] => [
            {
              conditionExpr: parseExpression(item1),
              resultExpr: parseExpression(item2),
            },
            ...condHelper(tl),
          ]
        | _ => failwith("invalid condData")
        };
    /* input: input, a concreteProgramPiece whose argument can not be a keyword
       output: a name whose argument is a the same argument as the input */
    let nameHelper: concreteProgramPiece => name =
      input =>
        switch (input) {
        | SymbolC("true")
        | SymbolC("false")
        | SymbolC("empty")
        | SymbolC("and")
        | SymbolC("or")
        | SymbolC("if")
        | SymbolC("cond")
        | SymbolC("lambda")
        | SymbolC("let")
        | SymbolC("define") => failwith("invalid name")
        | SymbolC(n) => Name(n)
        | _ => failwith("invalid name")
        };
    /* input: input, a list of concreteProgramPieces where each item in the
       list is a two item lists of concreteProgramPieces

       output: a list of letPair records. In this new format each record is
       comprised of a two item list from the input. The first item is in the
       field pairName and its argument is in the form of a name while the
       second item in the list is now in the field pairExpr as an expression */
    let rec letPairsHelper: list(concreteProgramPiece) => list(letPair) =
      input =>
        switch (input) {
        | [] => []
        | [ListC([SymbolC(item1), cpp2]), ...tl] => [
            {pairName: Name(item1), pairExpr: parseExpression(cpp2)},
            ...letPairsHelper(tl),
          ]
        | _ => failwith("invalid let pair")
        };
    switch (input) {
    | NumberC(int) => NumE(int)
    | SymbolC("true") => BoolE(true)
    | SymbolC("false") => BoolE(false)
    | SymbolC("empty") => EmptyE
    | SymbolC(n) => NameE(nameHelper(SymbolC(n)))
    | ListC([]) => failwith("Cannot parse an empty list")
    | ListC([NumberC(int), ...tl]) =>
      ApplicationE([NumE(int), ...List.map(parseExpression, tl)])
    | ListC([SymbolC("and"), item1, item2]) =>
      AndE(parseExpression(item1), parseExpression(item2))
    | ListC([SymbolC("and"), ..._]) =>
      failwith("And needs to take in two expressions")
    | ListC([SymbolC("or"), item1, item2]) =>
      OrE(parseExpression(item1), parseExpression(item2))
    | ListC([SymbolC("or"), ..._]) =>
      failwith("Or needs to take in two expressions")
    | ListC([SymbolC("if"), item1, item2, item3]) =>
      IfE({
        boolExpr: parseExpression(item1),
        trueExpr: parseExpression(item2),
        falseExpr: parseExpression(item3),
      })
    | ListC([SymbolC("if"), ..._]) =>
      failwith("If needs to take in three expressions")
    | ListC([SymbolC("cond"), ListC([item1, item2]), ...tl]) =>
      CondE([
        {
          conditionExpr: parseExpression(item1),
          resultExpr: parseExpression(item2),
        },
        ...condHelper(tl),
      ])
    | ListC([SymbolC("cond"), ..._]) =>
      failwith("Cond needs to take in at least a pair of valid condData")
    | ListC([SymbolC("lambda"), ListC([hd1, ...tl]), item2]) =>
      LambdaE({
        nameList: List.map(nameHelper, [hd1, ...tl]),
        lambdaBody: parseExpression(item2),
      })
    | ListC([SymbolC("lambda"), ListC([]), item2]) => LambdaE({
      nameList: [], lambdaBody: parseExpression(item2),
    })
    | ListC([SymbolC("lambda"), ..._]) =>
      failwith("Lambda needs to take in a list of names and an expression")
    | ListC([SymbolC("let"), ListC(item1), item2]) =>
      LetE({
        letPairs: letPairsHelper(item1),
        letBody: parseExpression(item2),
      })
    | ListC([SymbolC("let"), ..._]) =>
      failwith("Let needs to take in a list of pairs and an expression")
    | ListC([SymbolC(n), ...item]) =>
      ApplicationE([
        NameE(nameHelper(SymbolC(n))),
        ...List.map(parseExpression, item),
      ])
    | ListC([ListC(n), ...tl]) =>
      ApplicationE(List.map(parseExpression, [ListC(n), ...tl]))
    };
  };

/* Test cases for parseExpression */
checkExpectExpression(
  parseExpression(NumberC(3)),
  NumE(3),
  "parse expression with num",
);
checkExpectExpression(
  parseExpression(SymbolC("true")),
  BoolE(true),
  "parse expression with true",
);
checkExpectExpression(
  parseExpression(SymbolC("false")),
  BoolE(false),
  "parse expression with false",
);
checkExpectExpression(
  parseExpression(SymbolC("empty")),
  EmptyE,
  "parse expression with empty",
);
checkExpectExpression(
  parseExpression(SymbolC("football")),
  NameE(Name("football")),
  "parse expression with name",
);
checkExpectExpression(
  parseExpression(
    ListC([SymbolC("and"), SymbolC("false"), SymbolC("true")]),
  ),
  AndE(BoolE(false), BoolE(true)),
  "parse expression with and",
);
checkExpectExpression(
  parseExpression(
    ListC([SymbolC("or"), SymbolC("false"), SymbolC("true")]),
  ),
  OrE(BoolE(false), BoolE(true)),
  "parse expression with or",
);
checkExpectExpression(
  parseExpression(
    ListC([SymbolC("if"), SymbolC("true"), NumberC(3), NumberC(4)]),
  ),
  IfE({boolExpr: BoolE(true), trueExpr: NumE(3), falseExpr: NumE(4)}),
  "parse expression with if",
);
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("cond"),
      ListC([SymbolC("false"), NumberC(3)]),
      ListC([SymbolC("true"), NumberC(4)]),
      ListC([SymbolC("true"), NumberC(5)]),
    ]),
  ),
  CondE([
    {conditionExpr: BoolE(false), resultExpr: NumE(3)},
    {conditionExpr: BoolE(true), resultExpr: NumE(4)},
    {conditionExpr: BoolE(true), resultExpr: NumE(5)},
  ]),
  "parse expression with cond",
);
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("lambda"),
      ListC([SymbolC("x")]),
      ListC([SymbolC("+"), SymbolC("x"), NumberC(12)]),
    ]),
  ),
  LambdaE({
    nameList: [Name("x")],
    lambdaBody:
      ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(12)]),
  }),
  "parse expression with lambda",
);
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("lambda"),
      ListC([]),
      ListC([NumberC(12)]),
    ]),
  ),
  LambdaE({
    nameList: [],
    lambdaBody:
      ApplicationE([NumE(12)]),
  }),
  "parse expression with lambda with empty name list",
);
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("let"),
      ListC([
        ListC([SymbolC("x"), NumberC(12)]),
        ListC([SymbolC("y"), NumberC(13)]),
      ]),
      NumberC(100),
    ]),
  ),
  LetE({
    letPairs: [
      {pairName: Name("x"), pairExpr: NumE(12)},
      {pairName: Name("y"), pairExpr: NumE(13)},
    ],
    letBody: NumE(100),
  }),
  "parse expression with let",
);
checkExpectExpression(
  parseExpression(ListC([SymbolC("+"), NumberC(23), NumberC(45)])),
  ApplicationE([NameE(Name("+")), NumE(23), NumE(45)]),
  "parse expression with application",
);

/* parseDefinition:
   input: input, a concreteProgramPiece that is a definition
   output: an abstractProgramPiece that is the result of parsing input */

let parseDefinition: concreteProgramPiece => definition =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), SymbolC(a), item1]) => switch(a) {
      | "define"
      | "lambda"
      | "and"
      | "or"
      | "empty"
      | "let"
      | "true"
      | "false"
      | "if"
      | "cond" => failwith("cannot redefine a reserved keyword")
      | _ =>  (
        Name(a),
        parseExpression(item1),
      )
    }
    | _ => failwith("invalid definition")
    };

/* Test cases for parseDefinition */
checkError(
  () => parseDefinition(
    ListC([SymbolC("define"), SymbolC("define"), NumberC(6)]),
  ),
  "cannot redefine a reserved keyword",
);
checkExpectDefinition(
  parseDefinition(
    ListC([SymbolC("define"), SymbolC("singer"), NumberC(6)]),
  ),
  (Name("singer"), NumE(6)),
  "parse definition on singer",
);
checkExpectDefinition(
  parseDefinition(
    ListC([
      SymbolC("define"),
      SymbolC("adding"),
      ListC([SymbolC("+"), NumberC(6), NumberC(8)]),
    ]),
  ),
  (Name("adding"), ApplicationE([NameE(Name("+")), NumE(6), NumE(8)])),
  "parse definition on adding",
);

/* parsePiece:
   input: input, a concreteProgramPiece
   output: an abstractProgramPiece that is the result of parsing input */

let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) =>
      Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    };

/* Test cases for parsePiece */
checkExpectAbstractProgramPiece(
  parsePiece(NumberC(3)),
  Expression(NumE(3)),
  "parse piece with num",
);
checkExpectAbstractProgramPiece(
  parsePiece(SymbolC("true")),
  Expression(BoolE(true)),
  "parse piece with bool",
);
checkExpectAbstractProgramPiece(
  parsePiece(SymbolC("empty")),
  Expression(EmptyE),
  "parse piece with empty",
);
checkExpectAbstractProgramPiece(
  parsePiece(SymbolC("football")),
  Expression(NameE(Name("football"))),
  "parse piece with name",
);
checkExpectAbstractProgramPiece(
  parsePiece(ListC([SymbolC("and"), SymbolC("false"), SymbolC("true")])),
  Expression(AndE(BoolE(false), BoolE(true))),
  "parse piece with and",
);
checkExpectAbstractProgramPiece(
  parsePiece(ListC([SymbolC("or"), SymbolC("false"), SymbolC("true")])),
  Expression(OrE(BoolE(false), BoolE(true))),
  "parse piece with or",
);
checkExpectAbstractProgramPiece(
  parsePiece(
    ListC([SymbolC("if"), SymbolC("true"), NumberC(3), NumberC(4)]),
  ),
  Expression(
    IfE({boolExpr: BoolE(true), trueExpr: NumE(3), falseExpr: NumE(4)}),
  ),
  "parse piece with if",
);
checkExpectAbstractProgramPiece(
  parsePiece(
    ListC([
      SymbolC("cond"),
      ListC([SymbolC("false"), NumberC(3)]),
      ListC([SymbolC("true"), NumberC(4)]),
      ListC([SymbolC("true"), NumberC(5)]),
    ]),
  ),
  Expression(
    CondE([
      {conditionExpr: BoolE(false), resultExpr: NumE(3)},
      {conditionExpr: BoolE(true), resultExpr: NumE(4)},
      {conditionExpr: BoolE(true), resultExpr: NumE(5)},
    ]),
  ),
  "parse piece with cond",
);
checkExpectAbstractProgramPiece(
  parsePiece(
    ListC([
      SymbolC("lambda"),
      ListC([SymbolC("x")]),
      ListC([SymbolC("+"), SymbolC("x"), NumberC(12)]),
    ]),
  ),
  Expression(
    LambdaE({
      nameList: [Name("x")],
      lambdaBody:
        ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(12)]),
    }),
  ),
  "parse piece with lambda",
);
checkExpectAbstractProgramPiece(
  parsePiece(
    ListC([
      SymbolC("let"),
      ListC([
        ListC([SymbolC("x"), NumberC(12)]),
        ListC([SymbolC("y"), NumberC(13)]),
      ]),
      NumberC(100),
    ]),
  ),
  Expression(
    LetE({
      letPairs: [
        {pairName: Name("x"), pairExpr: NumE(12)},
        {pairName: Name("y"), pairExpr: NumE(13)},
      ],
      letBody: NumE(100),
    }),
  ),
  "parse piece with let",
);
checkExpectAbstractProgramPiece(
  parsePiece(ListC([SymbolC("+"), NumberC(23), NumberC(45)])),
  Expression(ApplicationE([NameE(Name("+")), NumE(23), NumE(45)])),
  "parse piece with application",
);
checkExpectAbstractProgramPiece(
  parsePiece(ListC([SymbolC("define"), SymbolC("x"), NumberC(3)])),
  Definition((Name("x"), NumE(3))),
  "parse piece with definition",
);

/* parse:
   input: input, a concreteProgram
   output: an abstractProgram that is the product of parsing input */

let parse: concreteProgram => abstractProgram =
  input => List.map(parsePiece, input);

/* Test cases for parse */
checkExpectAbstractProgram(
  parse([NumberC(3)]),
  [Expression(NumE(3))],
  "parse with num",
);
checkExpectAbstractProgram(
  parse([SymbolC("true")]),
  [Expression(BoolE(true))],
  "parse with bool",
);
checkExpectAbstractProgram(
  parse([SymbolC("empty")]),
  [Expression(EmptyE)],
  "parse with empty",
);
checkExpectAbstractProgram(
  parse([SymbolC("football")]),
  [Expression(NameE(Name("football")))],
  "parse with name",
);
checkExpectAbstractProgram(
  parse([ListC([SymbolC("and"), SymbolC("false"), SymbolC("true")])]),
  [Expression(AndE(BoolE(false), BoolE(true)))],
  "parse with and",
);
checkExpectAbstractProgram(
  parse([ListC([SymbolC("or"), SymbolC("false"), SymbolC("true")])]),
  [Expression(OrE(BoolE(false), BoolE(true)))],
  "parse with or",
);
checkExpectAbstractProgram(
  parse([
    ListC([SymbolC("if"), SymbolC("true"), NumberC(3), NumberC(4)]),
  ]),
  [
    Expression(
      IfE({boolExpr: BoolE(true), trueExpr: NumE(3), falseExpr: NumE(4)}),
    ),
  ],
  "parse with if",
);
checkExpectAbstractProgram(
  parse([
    ListC([
      SymbolC("cond"),
      ListC([SymbolC("false"), NumberC(3)]),
      ListC([SymbolC("true"), NumberC(4)]),
      ListC([SymbolC("true"), NumberC(5)]),
    ]),
  ]),
  [
    Expression(
      CondE([
        {conditionExpr: BoolE(false), resultExpr: NumE(3)},
        {conditionExpr: BoolE(true), resultExpr: NumE(4)},
        {conditionExpr: BoolE(true), resultExpr: NumE(5)},
      ]),
    ),
  ],
  "parse with cond",
);
checkExpectAbstractProgram(
  parse([
    ListC([
      SymbolC("lambda"),
      ListC([SymbolC("x")]),
      ListC([SymbolC("+"), SymbolC("x"), NumberC(12)]),
    ]),
  ]),
  [
    Expression(
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(12)]),
      }),
    ),
  ],
  "parse with lambda",
);
checkExpectAbstractProgram(
  parse([
    ListC([
      SymbolC("let"),
      ListC([
        ListC([SymbolC("x"), NumberC(12)]),
        ListC([SymbolC("y"), NumberC(13)]),
      ]),
      NumberC(100),
    ]),
  ]),
  [
    Expression(
      LetE({
        letPairs: [
          {pairName: Name("x"), pairExpr: NumE(12)},
          {pairName: Name("y"), pairExpr: NumE(13)},
        ],
        letBody: NumE(100),
      }),
    ),
  ],
  "parse with let",
);
checkExpectAbstractProgram(
  parse([ListC([SymbolC("+"), NumberC(23), NumberC(45)])]),
  [Expression(ApplicationE([NameE(Name("+")), NumE(23), NumE(45)]))],
  "parse with application",
);
checkExpectAbstractProgram(
  parse([ListC([SymbolC("define"), SymbolC("x"), NumberC(3)])]),
  [Definition((Name("x"), NumE(3)))],
  "parse with definition",
);

/* Test cases for parse expression and read */
checkExpectExpression(
  parseExpression(read("3")),
  NumE(3),
  "parse expression and read with num",
);
checkExpectExpression(
  parseExpression(read("true")),
  BoolE(true),
  "parse expression and read with true",
);
checkExpectExpression(
  parseExpression(read("false")),
  BoolE(false),
  "parse expression and read with false",
);
checkExpectExpression(
  parseExpression(read("empty")),
  EmptyE,
  "parse expression and read with empty",
);
checkExpectExpression(
  parseExpression(read("football")),
  NameE(Name("football")),
  "parse expression and read with name",
);
checkExpectExpression(
  parseExpression(read("(and false true)")),
  AndE(BoolE(false), BoolE(true)),
  "parse expression and read with and",
);
checkExpectExpression(
  parseExpression(read("(or false true)")),
  OrE(BoolE(false), BoolE(true)),
  "parse expression and read with or",
);
checkExpectExpression(
  parseExpression(read("(if true 3 4)")),
  IfE({boolExpr: BoolE(true), trueExpr: NumE(3), falseExpr: NumE(4)}),
  "parse expression and read with if",
);
checkExpectExpression(
  parseExpression(read("(cond (false 3) (true 4) (true 5))")),
  CondE([
    {conditionExpr: BoolE(false), resultExpr: NumE(3)},
    {conditionExpr: BoolE(true), resultExpr: NumE(4)},
    {conditionExpr: BoolE(true), resultExpr: NumE(5)},
  ]),
  "parse expression and read with cond",
);
checkExpectExpression(
  parseExpression(read("(lambda (x) (+ x 12))")),
  LambdaE({
    nameList: [Name("x")],
    lambdaBody:
      ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(12)]),
  }),
  "parse expression and read with lambda",
);
checkExpectExpression(
  parseExpression(read("(let ((x 12) (y 13)) 100)")),
  LetE({
    letPairs: [
      {pairName: Name("x"), pairExpr: NumE(12)},
      {pairName: Name("y"), pairExpr: NumE(13)},
    ],
    letBody: NumE(100),
  }),
  "parse expression and read with let",
);
checkExpectExpression(
  parseExpression(read("(+ 23 45)")),
  ApplicationE([NameE(Name("+")), NumE(23), NumE(45)]),
  "parse expression and read with application",
);

/* Test cases for parse definition and read */
checkExpectDefinition(
  parseDefinition(read("(define singer 6)")),
  (Name("singer"), NumE(6)),
  "parse definition and read on singer",
);
checkExpectDefinition(
  parseDefinition(read("(define adding (+ 6 8))")),
  (Name("adding"), ApplicationE([NameE(Name("+")), NumE(6), NumE(8)])),
  "parse definition and read on adding",
);

/* Test cases for parse piece and read */
checkExpectAbstractProgramPiece(
  parsePiece(read("3")),
  Expression(NumE(3)),
  "parse piece and read with num",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("true")),
  Expression(BoolE(true)),
  "parse piece and read with bool",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("empty")),
  Expression(EmptyE),
  "parse piece and read with empty",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("football")),
  Expression(NameE(Name("football"))),
  "parse piece and read with name",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("(and false true)")),
  Expression(AndE(BoolE(false), BoolE(true))),
  "parse piece and read with and",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("(or false true)")),
  Expression(OrE(BoolE(false), BoolE(true))),
  "parse piece and read with or",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("(if true 3 4)")),
  Expression(
    IfE({boolExpr: BoolE(true), trueExpr: NumE(3), falseExpr: NumE(4)}),
  ),
  "parse piece and read with if",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("(cond (false 3) (true 4) (true 5))")),
  Expression(
    CondE([
      {conditionExpr: BoolE(false), resultExpr: NumE(3)},
      {conditionExpr: BoolE(true), resultExpr: NumE(4)},
      {conditionExpr: BoolE(true), resultExpr: NumE(5)},
    ]),
  ),
  "parse piece and read with cond",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("(lambda (x) (+ x 12))")),
  Expression(
    LambdaE({
      nameList: [Name("x")],
      lambdaBody:
        ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(12)]),
    }),
  ),
  "parse piece and read with lambda",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("(let ((x 12) (y 13)) 100)")),
  Expression(
    LetE({
      letPairs: [
        {pairName: Name("x"), pairExpr: NumE(12)},
        {pairName: Name("y"), pairExpr: NumE(13)},
      ],
      letBody: NumE(100),
    }),
  ),
  "parse piece and read with let",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("(+ 23 45)")),
  Expression(ApplicationE([NameE(Name("+")), NumE(23), NumE(45)])),
  "parse piece and read with application",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("(define x 3)")),
  Definition((Name("x"), NumE(3))),
  "parse piece and read with definition",
);

/* Test cases for parse and read */
checkExpectAbstractProgram(
  parse(readAll("3")),
  [Expression(NumE(3))],
  "parse with num",
);
checkExpectAbstractProgram(
  parse(readAll("true")),
  [Expression(BoolE(true))],
  "parse with bool",
);
checkExpectAbstractProgram(
  parse(readAll("empty")),
  [Expression(EmptyE)],
  "parse with empty",
);
checkExpectAbstractProgram(
  parse(readAll("football")),
  [Expression(NameE(Name("football")))],
  "parse with name",
);
checkExpectAbstractProgram(
  parse(readAll("(and false true)")),
  [Expression(AndE(BoolE(false), BoolE(true)))],
  "parse with and",
);
checkExpectAbstractProgram(
  parse(readAll("(or false true)")),
  [Expression(OrE(BoolE(false), BoolE(true)))],
  "parse with or",
);
checkExpectAbstractProgram(
  parse(readAll("(if true 3 4)")),
  [
    Expression(
      IfE({boolExpr: BoolE(true), trueExpr: NumE(3), falseExpr: NumE(4)}),
    ),
  ],
  "parse with if",
);
checkExpectAbstractProgram(
  parse(readAll("(cond (false 3) (true 4) (true 5))")),
  [
    Expression(
      CondE([
        {conditionExpr: BoolE(false), resultExpr: NumE(3)},
        {conditionExpr: BoolE(true), resultExpr: NumE(4)},
        {conditionExpr: BoolE(true), resultExpr: NumE(5)},
      ]),
    ),
  ],
  "parse with cond",
);
checkExpectAbstractProgram(
  parse(readAll("(lambda (x) (+ x 12))")),
  [
    Expression(
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(12)]),
      }),
    ),
  ],
  "parse with lambda",
);
checkExpectAbstractProgram(
  parse(readAll("(let ((x 12) (y 13)) 100)")),
  [
    Expression(
      LetE({
        letPairs: [
          {pairName: Name("x"), pairExpr: NumE(12)},
          {pairName: Name("y"), pairExpr: NumE(13)},
        ],
        letBody: NumE(100),
      }),
    ),
  ],
  "parse with let",
);
checkExpectAbstractProgram(
  parse(readAll("(+ 23 45)")),
  [Expression(ApplicationE([NameE(Name("+")), NumE(23), NumE(45)]))],
  "parse with application",
);
checkExpectAbstractProgram(
  parse(readAll("(define x 3)")),
  [Definition((Name("x"), NumE(3)))],
  "parse and read with definition",
);

checkExpectAbstractProgram(
  parse(readAll("((lambda (x y) ((lambda (y) (+ x y)) x )) 17 18)")),
  [
    Expression(
      ApplicationE([
        LambdaE({
          nameList: [Name("x"), Name("y")],
          lambdaBody:
            ApplicationE([
              LambdaE({
                nameList: [Name("y")],
                lambdaBody:
                  ApplicationE([
                    NameE(Name("+")),
                    NameE(Name("x")),
                    NameE(Name("y")),
                  ]),
              }),
              NameE(Name("x")),
            ]),
        }),
        NumE(17),
        NumE(18),
      ]),
    ),
  ],
  "parse and read with a nested lambda",
);
checkExpectAbstractProgram(
  parse(
    readAll(
      "(define lengthen2 (lambda (alon) (cons 9 alon))) 
      (lengthen2 (list 2 3))",
    ),
  ),
  [
    Definition((
      Name("lengthen2"),
      LambdaE({
        nameList: [Name("alon")],
        lambdaBody:
          ApplicationE([
            NameE(Name("cons")),
            NumE(9),
            NameE(Name("alon")),
          ]),
      }),
    )),
    Expression(
      ApplicationE([
        NameE(Name("lengthen2")),
        ApplicationE([NameE(Name("list")), NumE(2), NumE(3)]),
      ]),
    ),
  ],
  "parse and read with a definition and application",
);
checkExpectAbstractProgram(
  parse(
    readAll(
      "(define flip (lambda (alops) (cond ((empty? alops) empty) ((cons? alops)
      (cons (list (second (first alops)) (first (first alops)))
      (flip (rest alops)))))))",
    ),
  ),
  [
    Definition((
      Name("flip"),
      LambdaE({
        nameList: [Name("alops")],
        lambdaBody:
          CondE([
            {
              conditionExpr:
                ApplicationE([
                  NameE(Name("empty?")),
                  NameE(Name("alops")),
                ]),
              resultExpr: EmptyE,
            },
            {
              conditionExpr:
                ApplicationE([
                  NameE(Name("cons?")),
                  NameE(Name("alops")),
                ]),
              resultExpr:
                ApplicationE([
                  NameE(Name("cons")),
                  ApplicationE([
                    NameE(Name("list")),
                    ApplicationE([
                      NameE(Name("second")),
                      ApplicationE([
                        NameE(Name("first")),
                        NameE(Name("alops")),
                      ]),
                    ]),
                    ApplicationE([
                      NameE(Name("first")),
                      ApplicationE([
                        NameE(Name("first")),
                        NameE(Name("alops")),
                      ]),
                    ]),
                  ]),
                  ApplicationE([
                    NameE(Name("flip")),
                    ApplicationE([
                      NameE(Name("rest")),
                      NameE(Name("alops")),
                    ]),
                  ]),
                ]),
            },
          ]),
      }),
    )),
  ],
  "parse and read with a complex program",
);

/*
 eval:
 input: the top-level environment called tle; local environment called env; and
   an expr which is an expression
 output: a value that is the result of evaluating the expression in the
   top-level environment extended by the local environment

 Recursion diagram 1:
 OI: (initialTle, [], AndE(BoolE(true), BoolE(true)))
  RI: (initialTle, [], AndE(BoolE(true))) | (initialTle, [], AndE(BoolE(true)))
  RO: BoolV(true) | BoolV(true)
 Ideas: Compare the results in the recursive output and return BoolV(true) if
  they both BoolV(true), otherwise return BoolV(false)
 OO: BoolV(true)

 Recursion diagram 2:
 OI: (initialTle, [], NumE(6))
  RI: N/A
  RO: N/A
 Ideas: This is a base and so are BoolE expressions.
 OO: NumV(6)
 */

let rec eval: (environment, environment, expression) => value =
  (tle, env, expr) => {
    let rec duplicateChecker: list(name) => bool =
      ln =>
        switch (ln) {
        | [] => false
        | [hd, ...tl] => List.mem(hd, tl) || duplicateChecker(tl)
        };

    /* input: tle, the top level environment; env, the local environment; expr,
       an expression
       output: a list of values that are the result of evaluating the expr in
       the top level environment and the local environment */
    let rec evalMap:
      ((environment, environment, expression) => value, list(expression)) =>
      list(value) =
      (eval, expr) =>
        switch (eval, expr) {
        | (_, []) => []
        | (eval, [hd, ...tl]) => [
            eval(tle, env, hd),
            ...evalMap(eval, tl),
          ]
        };

    /* input: namelist, a list of names that make up the formal arugments of
       the lambda expression, and vallist, a list of values which are the
       actual arguments
       output: an environment where the formal arguments are bound the acutal
       arguments in the form of tuples */
    let rec lambdaHelper: (list(name), list(value)) => environment =
      (namelist, vallist) =>
        switch (namelist, vallist) {
        | ([name1, ...nametl], [itemhd, ...itemtl]) =>
          if (duplicateChecker([name1, ...nametl])) {
            failwith("lambda: cannot bound a variable more than once");
          } else {
            [(name1, itemhd), ...lambdaHelper(nametl, itemtl)];
          }
        | _ => []
        };

    /* input: lp, a list of letPairs
       output: a list comprised of the expressions associated with pairName for
       each letPair in the input list */
    let rec letPairHelper: list(letPair) => list(name) =
      lp =>
        switch (lp) {
        | [] => []
        | [{pairName: item1, pairExpr: _item2}, ...tl] => [
            item1,
            ...letPairHelper(tl),
          ]
        };

    /* input: envl, an environment, lp, a list of letPairs
       output: a new environment consisting of pairNames bound to evaluated
       pairExprs in the form of tuples and the input environment */
    let rec letHelper: (environment, list(letPair)) => environment =
      (envl, lp) =>
        switch (lp) {
        | [] => []
        | [{pairName: item1, pairExpr: item2}, ...tl] =>
          if (duplicateChecker(letPairHelper(lp))) {
            failwith("let: cannot bound a variable more than once");
          } else {
            List.append(
              [(item1, eval(tle, env, item2)), ...letHelper(envl, tl)],
              envl,
            );
          }
        };
    /* input: env, an enviroment, and nam, a name
       output: the value bound to name in the environment */
    let rec lookup: (environment, name) => value =
      (env, nam) =>
        switch (env, nam) {
        | ([(item1, item2), ...tl], nam) =>
          if (item1 == nam) {
            item2;
          } else {
            lookup(tl, nam);
          }
        | _ => failwith("lookup error")
        };

    switch (tle, env, expr) {
    | (_tle, _env, NumE(int)) => NumV(int)
    | (_tle, _env, BoolE(tf)) => BoolV(tf)
    | (_tle, _env, EmptyE) => ListV([])
    | (tle, env, NameE(item)) => lookup(List.append(env, tle), item)
    | (tle, env, AndE(item1, item2)) =>
      switch (eval(tle, env, item1)) {
      | BoolV(true) =>
        switch (eval(tle, env, item2)) {
        | BoolV(true) => BoolV(true)
        | BoolV(false) => BoolV(false)
        | _ => failwith("and expects a boolean as second argument")
        }
      | BoolV(false) => BoolV(false)
      | _ => failwith("and expects a boolean as first argument")
      }
    | (tle, env, OrE(item1, item2)) =>
      switch (eval(tle, env, item1)) {
      | BoolV(false) =>
        switch (eval(tle, env, item2)) {
        | BoolV(true) => BoolV(true)
        | BoolV(false) => BoolV(false)
        | _ => failwith("or expects a boolean as second argument")
        }
      | BoolV(true) => BoolV(true)
      | _ => failwith("or expects a boolean as first argument")
      }
    | (tle, env, IfE({boolExpr: bool, trueExpr: item1, falseExpr: item2})) =>
      switch (eval(tle, env, bool)) {
      | BoolV(true) => eval(tle, env, item1)
      | BoolV(false) => eval(tle, env, item2)
      | _ => failwith("if: first argument must evaluate to a boolean")
      }
    | (_tle, _env, CondE([])) => failwith("all cases were false")
    | (tle, env, CondE([{conditionExpr: cexpr, resultExpr: rexpr}, ...tl])) =>
      switch (eval(tle, env, cexpr)) {
      | BoolV(true) => eval(tle, env, rexpr)
      | BoolV(false) => eval(tle, env, CondE(tl))
      | _ => failwith("not a valid cond case")
      }
    | (_tle, env, LambdaE({nameList: namelist, lambdaBody: expr})) =>
      if (duplicateChecker(namelist)) {
        failwith("lambda: cannot bound a variable more than once");
      } else {
        ClosureV({cNameList: namelist, cExpr: expr, cEnv: env});
      }
    | (tle, env, LetE({letPairs: lp, letBody: expr})) =>
      eval(tle, letHelper(env, lp), expr)
    | (tle, _env, ApplicationE(exprList)) =>
      switch (evalMap(eval, exprList)) {
      | [BuiltinV({bName: _name, bProc: proc}), ...tl] => proc(tl)
      | [
          ClosureV({cNameList: namelist, cExpr: expr, cEnv: env2}),
          ...vallist,
        ] =>
        if (List.length(namelist) == List.length(vallist)) {
          eval(
            tle,
            List.append(lambdaHelper(namelist, vallist), env2),
            expr,
          );
        } else {
          failwith("formals and actuals must have the same number");
        }
      | _ => failwith("expected a function after open parenthesis")
      }
    };
  };

/*
 addDefinition:
 input: an environment, called env, and a definition, denoted as (id, expr)
 output: a new environment with the definition added to it */

let addDefinition: (environment, (name, expression)) => environment =
  (env, (id, expr)) => {
    let rec addDefHelper: (environment, name) => bool =
      (env, id) =>
        switch (env, id) {
        | ([], _) => true
        | ([(item1, _item2), ...tl], id) =>
          !(id == item1) && addDefHelper(tl, id)
        };
    if (addDefHelper(env, id)) {
      [(id, eval(env, [], expr)), ...env];
    } else {
      failwith("cannot redefine");
    };
  };

/* Test cases for addDefinition */
checkExpect(
  addDefinition([], (Name("x"), NumE(3))),
  [(Name("x"), NumV(3))],
  "add definition to an empty environment",
);
checkExpect(
  addDefinition([(Name("y"), NumV(4))], (Name("x"), NumE(3))),
  [(Name("x"), NumV(3)), (Name("y"), NumV(4))],
  "add definition to a non-empty environment",
);

/* stringofValue:
   input: aValue, a value
   output: a string representation of the argument of aValue

   Recursion diagram 1:
   Original input: ListV([NumV(3), NumV(2)])
   Recursive input: ListV([NumV(2)])
   Recursive output: "(cons 2 empty)"
   string.append (cons 3 in front of RO and ) in the end
   Original output: "(cons 3 (cons 2 empty))"

   Recursion diagram 2:
   OI: ListV([BoolV(true)])
   Recursive input: BoolV(true)
   Recursive output: "true"
   string.append (cons 3 in front of RO and ) in the end
   Original output: "(cons true empty)"

   Recursion diagram 3:
   OI: ListV([])
   RI: N/A
   RO: N/A
   return "empty" for ListV([])
   OO: "empty"
   */

let rec stringOfValue: value => string =
  fun
  | NumV(int) => string_of_int(int)
  | BoolV(bool) => string_of_bool(bool)
  | ListV([]) => "'()"
  | ListV([hd, ...tl]) =>
    "(cons " ++ stringOfValue(hd) ++ " " ++ stringOfValue(ListV(tl)) ++ ")"
  | BuiltinV(builtinData) => builtinData.bName
  | ClosureV(_) => "User-defined procedure";

/* Test cases for stringOfValue */
checkExpect(stringOfValue(NumV(3)), "3", "print a num");
checkExpect(stringOfValue(BoolV(true)), "true", "print a bool");
checkExpect(stringOfValue(ListV([])), "'()", "print an empty list");
checkExpect(
  stringOfValue(ListV([NumV(3), NumV(4)])),
  "(cons 3 (cons 4 '()))",
  "print a non-empty list",
);
checkExpect(
  stringOfValue(
    ClosureV({
      cNameList: [Name("x")],
      cExpr: ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(2)]),
      cEnv: [],
    }),
  ),
  "User-defined procedure",
  "print a closure",
);

/* Test cases for eval and stringOfValue */
checkExpect(
  stringOfValue(eval(initialTle, [], NumE(3))),
  "3",
  "eval and stringOfValue with num",
);
checkExpect(
  stringOfValue(eval(initialTle, [], BoolE(false))),
  "false",
  "eval and stringOfValue with bool",
);
checkExpect(
  stringOfValue(eval(initialTle, [], EmptyE)),
  "'()",
  "eval and stringOfValue with empty",
);
checkExpect(
  stringOfValue(eval(initialTle, [], NameE(Name("+")))),
  "<builtin: + >",
  "eval and stringOfValue with builtin name",
);
checkExpect(
  stringOfValue(eval(initialTle, [], AndE(BoolE(false), BoolE(false)))),
  "false",
  "eval and stringOfValue with and false false",
);
checkExpect(
  stringOfValue(eval(initialTle, [], AndE(BoolE(true), BoolE(false)))),
  "false",
  "eval and stringOfValue with and true false",
);
checkExpect(
  stringOfValue(eval(initialTle, [], AndE(BoolE(false), BoolE(true)))),
  "false",
  "eval and stringOfValue with and false true",
);
checkExpect(
  stringOfValue(eval(initialTle, [], AndE(BoolE(true), BoolE(true)))),
  "true",
  "eval and stringOfValue with and true true",
);
checkExpect(
  stringOfValue(eval(initialTle, [], OrE(BoolE(false), BoolE(false)))),
  "false",
  "eval and stringOfValue with or false false",
);
checkExpect(
  stringOfValue(eval(initialTle, [], OrE(BoolE(true), BoolE(false)))),
  "true",
  "eval and stringOfValue with or true false",
);
checkExpect(
  stringOfValue(eval(initialTle, [], OrE(BoolE(false), BoolE(true)))),
  "true",
  "eval and stringOfValue with or false true",
);
checkExpect(
  stringOfValue(eval(initialTle, [], OrE(BoolE(true), BoolE(true)))),
  "true",
  "eval and stringOfValue with or true true",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      IfE({boolExpr: BoolE(false), trueExpr: NumE(3), falseExpr: NumE(4)}),
    ),
  ),
  "4",
  "eval and stringOfValue with if",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      CondE([
        {conditionExpr: BoolE(false), resultExpr: NumE(3)},
        {conditionExpr: BoolE(true), resultExpr: NumE(4)},
        {conditionExpr: BoolE(true), resultExpr: NumE(5)},
      ]),
    ),
  ),
  "4",
  "eval and stringOfValue with cond",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(2)]),
      }),
    ),
  ),
  "User-defined procedure",
  "eval and stringOfValue with lambda",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      LetE({
        letPairs: [
          {pairName: Name("x"), pairExpr: NumE(0)},
          {pairName: Name("y"), pairExpr: NumE(1)},
        ],
        letBody:
          ApplicationE([
            NameE(Name("+")),
            NameE(Name("x")),
            NameE(Name("y")),
          ]),
      }),
    ),
  ),
  "1",
  "eval and stringOfValue with let",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      ApplicationE([NameE(Name("+")), NumE(1), NumE(2)]),
    ),
  ),
  "3",
  "eval and stringOfValue with builtin +",
);
checkExpect(
  stringOfValue(
    eval(initialTle, [], ApplicationE([NameE(Name("zero?")), NumE(9)])),
  ),
  "false",
  "eval and stringOfValue with builtin zero?",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      ApplicationE([
        LambdaE({
          nameList: [Name("x")],
          lambdaBody:
            ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(2)]),
        }),
        NumE(3),
      ]),
    ),
  ),
  "5",
  "eval and stringOfValue with lambda and actual",
);

/* Test cases for read, parseExpression, eval, and stringOfValue */
checkExpect(
  stringOfValue(eval(initialTle, [], parseExpression(read("3")))),
  "3",
  "read and parseExpression and eval and stringOfValue with num",
);
checkExpect(
  stringOfValue(eval(initialTle, [], parseExpression(read("true")))),
  "true",
  "read and parseExpression and eval and stringOfValue with bool",
);
checkExpect(
  stringOfValue(eval(initialTle, [], parseExpression(read("empty")))),
  "'()",
  "read and parseExpression and eval and stringOfValue with empty",
);
checkExpect(
  stringOfValue(eval(initialTle, [], parseExpression(read("+")))),
  "<builtin: + >",
  "read and parseExpression and eval and stringOfValue with builtin name",
);
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(and true false)"))),
  ),
  "false",
  "read and parseExpression and eval and stringOfValue with and true false",
);
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(and true true)"))),
  ),
  "true",
  "read and parseExpression and eval and stringOfValue with and true true",
);
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(and false false)"))),
  ),
  "false",
  "read and parseExpression and eval and stringOfValue with and false false",
);
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(and false true)"))),
  ),
  "false",
  "read and parseExpression and eval and stringOfValue with and false true",
);
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(or true false)"))),
  ),
  "true",
  "read and parseExpression and eval and stringOfValue with or true false",
);
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(or true true)"))),
  ),
  "true",
  "read and parseExpression and eval and stringOfValue with or true true",
);
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(or false false)"))),
  ),
  "false",
  "read and parseExpression and eval and stringOfValue with or false false",
);
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(or false true)"))),
  ),
  "true",
  "read and parseExpression and eval and stringOfValue with or false true",
);
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(if false 3 5)"))),
  ),
  "5",
  "read and parseExpression and eval and stringOfValue with if",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      parseExpression(read("(cond (false 3) (true 4) (true 5))")),
    ),
  ),
  "4",
  "read and parseExpression and eval and stringOfValue with cond",
);
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(lambda (x) (+ x 12))"))),
  ),
  "User-defined procedure",
  "read and parseExpression and eval and stringOfValue with lambda",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      parseExpression(read("(let ((x 0) (y 1)) (+ x y))")),
    ),
  ),
  "1",
  "read and parseExpression and eval and stringOfValue with let",
);
checkExpect(
  stringOfValue(eval(initialTle, [], parseExpression(read("(+ 1 2)")))),
  "3",
  "read and parseExpression and eval and stringOfValue with builtin +",
);
checkExpect(
  stringOfValue(eval(initialTle, [], parseExpression(read("(zero? 9)")))),
  "false",
  "read and parseExpression and eval and stringOfValue with builtin zero?",
);
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("((lambda (x) (+ x 2)) 3)"))),
  ),
  "5",
  "read and parseExpression and eval and stringOfValue with lambda and actual",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      parseExpression(
        read("((lambda (x y) ((lambda (y) (+ x y)) x)) 17 18)"),
      ),
    ),
  ),
  "34",
  "read and parseExpression and eval and stringOfValue with nested lambdas",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      parseExpression(
        read(
          "(let ((x 0) (y 18)) (let ((f (lambda (a b) (+ x b )))"
          ++ "(x 17)) (f y x)))",
        ),
      ),
    ),
  ),
  "17",
  "read and parseExpression and eval and stringOfValue with"
  ++ "let and nested lambda within let",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      parseExpression(
        read(
          "(let ((x false) (y 1))
               (cond
                 ((zero? y) (not x))
                 ((not (zero? y)) x)))",
        ),
      ),
    ),
  ),
  "false",
  "read and parseExpression and eval and stringOfValue with let and cond",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      parseExpression(
        read(
          "((lambda (x y z) (+ z
                 (if (> x y)
                    (+ x y)
                    (- x y)))) 1 2 3)",
        ),
      ),
    ),
  ),
  "2",
  "read and parseExpression and eval and stringOfValue with lambda and if",
);

/*process:
  input: an abstractProgram called pieces
  output: a list of values that results from processing pieces */
let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition(d), ...tl] => processHelper(addDefinition(tle, d), tl)
        | [Expression(e), ...tl] => [
            eval(tle, [], e),
            ...processHelper(tle, tl),
          ]
        };
    processHelper(initialTle, pieces);
  };

/* rackette:
   input: a rawProgram called program
   output: a list of strings resulted from reading, parsing, processing, and
   printing program under the rules of Rackette */
let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));

/* Test cases for rackette */
checkExpect(rackette("3"), ["3"], "rackette with num");
checkExpect(rackette("true"), ["true"], "rackette with bool");
checkExpect(rackette("empty"), ["'()"], "rackette with empty");
checkExpect(rackette("+"), ["<builtin: + >"], "rackette with builtin name");
checkExpect(rackette("(and true false)"), ["false"], "rackette with and");
checkExpect(rackette("(or true false)"), ["true"], "rackette with or");
checkExpect(rackette("(if false 2 3)"), ["3"], "rackette with if");
checkExpect(
  rackette("(cond (false 3) (true 54))"),
  ["54"],
  "rackette with cond",
);
checkExpect(
  rackette("((lambda (x) (+ x 1)) 9)"),
  ["10"],
  "rackette with lambda and actual",
);
checkExpect(rackette("(let ((x 0)) (* x 1))"), ["0"], "rackette with let");
checkExpect(
  rackette("(cons 2 empty)"),
  ["(cons 2 '())"],
  "rackette with builtin",
);
checkExpect(
  rackette("(define f 3) f"),
  ["3"],
  "rackette with define with num",
);
checkExpect(
  rackette("(define f false) f"),
  ["false"],
  "rackette with define with bool",
);
checkExpect(
  rackette("(define f (lambda (x) (+ x 1))) (f 3)"),
  ["4"],
  "rackette with define with lambda",
);
checkExpect(
  rackette("(define my-positive? (lambda (n) (> n 0))) (my-positive? 2)"),
  ["true"],
  "rackette with define with lambda2",
);
checkExpect(
  rackette("(define sum-angles (lambda (n) (* 180 (- n 2)))) (sum-angles 3)"),
  ["180"],
  "rackette with define with lambda3",
);
checkExpect(
  rackette("(define sum (lambda (n) (/ (* n (+ n 1)) 2))) (sum 3)"),
  ["6"],
  "rackette with define with lambda4",
);
checkExpect(
  rackette(
    "(define my-append (lambda (alon1 alon2)
  (cond
    ((empty? alon1) alon2)
    ((cons? alon1) (cons (first alon1) (my-append (rest alon1) alon2))))))
    (my-append (list 1 2 3) (list 2 3 4))",
  ),
  ["(cons 1 (cons 2 (cons 3 (cons 2 (cons 3 (cons 4 '()))))))"],
  "rackette with define with lambda5",
);
checkExpect(
  rackette(
    "(define remove-until-zero (lambda (alon)
  (cond
    ((empty? alon) empty)
    ((cons? alon)
     (if (= (first alon) 0)
        alon
        (remove-until-zero (rest alon)))))))
        (remove-until-zero (list 1 0 2))",
  ),
  ["(cons 0 (cons 2 '()))"],
  "rackette with define with lambda6",
);
checkExpect(
  rackette(
    "(define sum-list (lambda (aloi)
  (cond
    ((empty? aloi) 0)
    ((cons? aloi) (+ (first aloi) (sum-list (rest aloi)))))))
    (sum-list (list 1 2 3 1 23))
",
  ),
  ["30"],
  "rackette with define with lambda7",
);
checkExpect(
  rackette(
    "(define count-up (lambda (n k)
  (if (= k 0)
     empty
     (cons n (count-up (+ n 1) (- k 1))))))
     (count-up 34 8)",
  ),
  [
    "(cons 34 (cons 35 (cons 36 (cons 37 "
    ++ "(cons 38 (cons 39 (cons 40 (cons 41 '()))))))))",
  ],
  "rackette with define with lambda8",
);
checkExpect(
  rackette(
    "(define fact (lambda (x) 
    (if (zero? x) 1 (* x (fact (- x 1)))))) (fact 3)",
  ),
  ["6"],
  "rackette with define with lambda9",
);

checkError(() => rackette("j"), "lookup error");
checkError(() => rackette("|"), "invalid character identifier");
checkError(() => rackette("("), "wrong number of parentheses");
checkError(() => rackette("(+ 1 false)"), "Wrong value type for +");
checkError(() => rackette("(* 1 2 3 3 3 3 4)"), "* expects 2 arguments");
checkError(
  () => rackette("(equal? false false false)"),
  "equal? expects 2 arguments",
);
checkError(
  () => rackette("(first empty)"),
  "first expects a non-empty list",
);
checkError(() => rackette("and"), "invalid name");
checkError(
  () => rackette("(and false false false)"),
  "And needs to take in two expressions",
);
checkError(
  () => rackette("(or false false false)"),
  "Or needs to take in two expressions",
);
checkError(
  () => rackette("(if false false false false)"),
  "If needs to take in three expressions",
);
checkError(
  () => rackette("(cond (false 2) (true 2 3))"),
  "invalid condData",
);
checkError(
  () => rackette("(cond)"),
  "Cond needs to take in at least a pair of valid condData",
);
checkError(
  () => rackette("(lambda (x) )"),
  "Lambda needs to take in a list of names and an expression",
);
checkError(() => rackette("(let ((x 2 3)) 4)"), "invalid let pair");
checkError(
  () => rackette("(let ((x 2 3)))"),
  "Let needs to take in a list of pairs and an expression",
);
checkError(() => rackette("(define 3)"), "invalid definition");
checkError(
  () => rackette("(and true 1)"),
  "and expects a boolean as second argument",
);
checkError(
  () => rackette("(and 1 1)"),
  "and expects a boolean as first argument",
);
checkError(
  () => rackette("(or false 1)"),
  "or expects a boolean as second argument",
);
checkError(
  () => rackette("(or 1 1)"),
  "or expects a boolean as first argument",
);
checkError(
  () => rackette("(if 1 2 3)"),
  "if: first argument must evaluate to a boolean",
);
checkError(() => rackette("(cond (false 3))"), "all cases were false");
checkError(
  () => rackette("((lambda (x) (+ x 1)) 1 2)"),
  "formals and actuals must have the same number",
);
checkError(
  () => rackette("(lambda (x x) (+ 1 2))"),
  "lambda: cannot bound a variable more than once",
);
checkError(
  () => rackette("(let ((x 4) (x 32)) (+ 1 x))"),
  "let: cannot bound a variable more than once",
);
checkError(() => rackette("f"), "lookup error");
checkError(
  () => rackette("(2)"),
  "expected a function after open parenthesis",
);
checkError(() => rackette("(define x 1) (define x 2)"), "cannot redefine");

