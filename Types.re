/* a rawProgram is a string that represents an input program in Rackette*/
type rawProgram = string;
/* Examples of rawProgram:
"(+ 3 5)"
"(zero? 5)"
"this is a program"
"(lambda (x) (+ x 5))" */



/* a concreteProgramPiece the result of reading a piece in rawProgram
It is either 
a NumberC(x) where x is an int
a SymbolC(x) where x is a string
or ListC(x) where x is a list of concreteProgramPiece
nothing else is a concreteProgramPiece */
type concreteProgramPiece =
  | NumberC(int)
  | SymbolC(string)
  | ListC(list(concreteProgramPiece));

/* Examples of concreteProgramPiece:
NumberC(3), NumberC(5)
SymbolC("+"), SymbolC("zero?")
ListC([SymbolC("+"), NumberC(3), NumberC(5)])
ListC([SymbolC("zero?"), NumberC(5)]) 
ListC([SymbolC("lambda"), ListC([SymbolC("x")]), 
  ListC([SymbolC("+"), SymbolC("x"), NumberC(5)])])*/


/* a concreteProgram is the result of reading the rawProgram
It is a list of concreteProgramPiece*/
type concreteProgram = list(concreteProgramPiece);

/* Examples of concreteProgram:
[NumberC(3)]
[SymbolC("zero?")]
[ListC([SymbolC("+"), NumberC(3), NumberC(5)])]
[ListC([SymbolC("-"), NumberC(3), 
  ListC([SymbolC("+"), NumberC(3), NumberC(5)])])]
[ListC([SymbolC("lambda"), ListC([SymbolC("x")]), 
  ListC([SymbolC("+"), SymbolC("x"), NumberC(5)])])] */



/* a Rackette name is a builtin, keyword or user-defined name
It is Name(x), where x is a string*/
type name =
  | Name(string);

/* Examples of name:
Name("+"), Name("define"), Name("x"), Name("lambda")*/



/* a Rackette expression is the result of parsing a non-definition
concreteProgramPiece
It is  either
NumE(x), where x is an int
BoolE(x), where x is a bool
EmptyE
NameE(x), where x is a Rackette name
AndE(x, y), where x and y are expressions
OrE(x, y), where x and y are expressions
IfE(x), where x is ifData
CondE(x), where x is a list of condData
LambdaE(x), where x is lambdaData
LetE(x), where x is letData
ApplicationE(x), where x is a list of expressions
nothing else is an expression

a ifData is a record of the arguments of a if-expression, which is
{boolExpr: x, trueExpr: y, falseExpr: z,}, where x, y, and z are expressions

a condData is a record of the arguments of a condition-expression, which is
{conditionExpr: x, resultExpr: y,}, where x and y are expressions

a lambdaData is a record of the arguments of a the lambda-expression, which is
{nameList: x, lambdaBody: y, }, 
where x is a list of names and y is an expression

a letPair is a record of the temporary bindings in a let-expression, which is
{pairName: x, pairExpr: y,}
where x is a name and y is an expression

a letData is a record of the arguments of a let-expression, which is
{letPairs: x, letBody: y,}
where x is a list of letPairs and y is an expression
*/
type expression =
  | NumE(int)
  | BoolE(bool)
  | EmptyE
  | NameE(name)
  | AndE(expression, expression)
  | OrE(expression, expression)
  | IfE(ifData)
  | CondE(list(condData)) 
  | LambdaE(lambdaData)
  | LetE(letData)
  | ApplicationE(list(expression))
  and ifData = {
    boolExpr: expression,
    trueExpr: expression,
    falseExpr: expression,
  }
  and condData = { 
    conditionExpr: expression, 
    resultExpr: expression,
  }
  and lambdaData = {
    nameList: list(name),
    lambdaBody: expression,
  } 
  and letPair = {
    pairName: name, 
    pairExpr: expression,  
  }
  and letData = {
    letPairs: list(letPair),
    letBody: expression,
  }

/* Examples of expression:
NumE(17), NumE(5), NumE(12), NumE(-14)
BoolE(true), BoolE(false)
EmptyE
NameE(Name("Tiny")), NameE(Name("Dancer")), NameE(Name("hello"))
AndE(BoolE(true), BoolE(false)), AndE(BoolE(true), BoolE(true))
OrE(BoolE(false), BoolE(true)), OrE(BoolE(true), BoolE(false))
IfE({boolExpr: BoolE(true), trueExpr: NumE(5), falseExpr: BoolE(true),})
IfE({boolExpr: BoolE(true), trueExpr: NumE(12), falseExpr: NumE(-14),})
CondE([{conditionExpr: ApplicationE([NameE(Name("empty?")), EmptyE]), 
  resultExpr: BoolE(true),}])
CondE([{conditionExpr: ApplicationE([NameE(Name("zero?")), NumE(5)]), 
  resultExpr: BoolE(false),}])
CondE([{conditionExpr: BoolE(true), resultExpr: AndE(true, false)}
  {conditionExpr: BoolE(false), resultExpr: NumE(-14),}])
LambdaE({nameList: [Name("x"), Name("y")], lambdaBody: NumE(5),})
LambdaE({nameList: [Name("lambda")], lambdaBody: NumE(-20),})
LambdaE({nameList: [Name("m")], lambdaBody: EmptyE,})
LambdaE({nameList: [Name("x")], 
  lambdaBody: ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(5)]),})
LetE({letPairs: [{pairName: Name("n"), pairExpr: Name("tiny"),}], 
     letBody: AndE(BoolE(true), BoolE(true)),})
LetE({letPairs: [{pairName: Name("x"), pairExpr: NumE(12),}, 
                 {pairName: Name("y"), pairExpr: NumE(14),}],
      letBody: NumE(12),})
ApplicationE([NameE(Name("+")), NumE(3), NumE(5)])
ApplicationE([NameE(Name("zero?")), NumE(5)])
ApplicationE([NameE(Name("empty?")), EmptyE]) */

/* Examples of ifData:
{ifConditionExpr: BoolE(true), trueExpr: NumE(12), falseExpr: NumE(-14),}
{ifConditionExpr: BoolE(false), trueExpr: NumE(12), falseExpr: NumE(-14),}

Examples of condData:
{conditionExpr: BoolE(true), resultExpr: AndE(true, false),}
{conditionExpr: BoolE(false), resultExpr: NumE(-14),}

Examples of lambdaData:
{nameList: [Name("x")], lambdaBody: BoolE(false),}

Examples of letPair:
{pairName: Name("x"), pairExpr: NumE(12),}
{pairName: Name("y"), pairExpr: NumE(14),}

Examples of letData:
{letPairs: [{pairName: Name("x"), pairExpr: NumE(12)}, 
            {pairName: Name("y"), pairExpr: NumE(14)}],
 letBody: NumE(12),} */
  


/* a Rackette definition is the result of parsing a concreteProgramPiece
that starts with the keyword define
It is a tuple (x, y), where x is a name and y is an expression*/
type definition = (name, expression);

/* Examples of definition:
(Name("x"), NumE(17))
(Name("dancer"), ApplicationE([NameE(Name("zero?")), NumE(5)])) */



/* an abstractProgramPiece is the result of parsing a concreteProgramPiece
It is either
Definition(x), where x is a definition
or Expression(x), where x is an expression
nothing else is an abstractProgramPiece
It is a piece of Rackette that can be processed
*/
type abstractProgramPiece =
  | Definition(definition)
  | Expression(expression);

/* Examples of abstractProgramPiece: 
Definition(Name("x"), NumE(17))
Definition((Name("dancer"), ApplicationE([NameE(Name("zero?")), NumE(5)]))) 
Expression(BoolE(true))
Expression(NumE(17)) */



/* an abstractProgram is the result of parsing a concreteProgram
It is a list of abstractProgramPieces 
It is a representation of a Rackette program with any number of pieces */
type abstractProgram = list(abstractProgramPiece);

/* Examples of abstractProgram:
[Definition(Name("x"), NumE(17)), Expression(NumE(17))]
[Definition(Name("cheers")), Expression(NameE(Name("mate")))]
[Expression(BoolE(true)), Expression(BoolE(true)), 
  Expression(LetE({letPairs: [{pairName: Name("x"), pairExpr: NumE(12),}, 
                  {pairName: Name("y"), pairExpr: NumE(14),}],
      letBody: NumE(12),}))] */



/* a Rackette value is the result of evaluating an abstractProgram 
It is either
NumV(x), where x is an int
BoolV(x), where x is a bool
ListV(x), where x is a list of values
BuiltinV(x), where x is a builtinData
ClosureV(x), where x is a closureData
Value is the result of evaluating a Rackette expression

a builtinData is a record representing a pre-defined procedure
{bName: x, bProc: y => z, }
where x is a string, y is a list of values, and z is a value

a closureData is a record representing a user-defined procedure
{cNameList: x, cExpr: y, cEnv: z, }
where x is a list of names, y is an expression, and z is an environment

an environment is a list of bindings

a binding is a relationship between formal and actual arguments, it is a
tuple (x, y), where x is a name (representing formal argument) and y is a value
(representing actual argument)
*/
type value =
  | NumV(int)
  | BoolV(bool)
  | ListV(list(value))
  | BuiltinV(builtinData)
  | ClosureV(closureData)
  and builtinData = { 
    bName: string,
    bProc: list(value) => value,
  }
  and closureData = {
    cNameList: list(name),
    cExpr: expression, 
    cEnv: environment,
  }
  /* Environments and bindings aren't values
     But we use "and" here so bindings have access to values
     and closures have access to environments */
  and environment = (list(binding))
  and binding = (name, value);

/* Examples of values:
NumV(7), NumV(99)
BoolV(true), BoolV(false)
ListV([BoolV(true), BoolV(false)]), ListV([NumV(7), NumV(99)])
BuiltinV({bName: "<builtin: + >", bProc: plus,})
BuiltinV({bName: "<builtin: = >", bProc: equal,})
ClosureV({
  cNameList: [Name("x"), Name("y")],
  cExpr: NameE(Name("*")), 
  cEnv: [(Name("x"), NumV(2)), (Name("y"), NumV(7))],})
ClosureV({
  cNameList: [Name("f")],
  cExpr: NumE(-4), 
  cEnv: [(Name("f"), NumV(-4))],}) */

/* Examples of builtinData:
{bName: "<builtin: + >", bProc: plus,}
{bName: "<builtin: = >", bProc: equal,}

Examples of closureData:
{cNameList: [Name("f")], 
 cExpr: NumE(-4), 
 cEnv: [(Name("f"), NumV(-4))],}
{cNameList: [Name("x"), Name("y")],
 cExpr: NameE(Name("*")), 
 cEnv: [(Name("x"), NumV(2)), (Name("y"), NumV(7))],}

Examples of environment:
[(Name("f"), NumV(-4))]
[(Name("x"), NumV(2)), (Name("y"), NumV(7))]

Examples of binding:
(Name("f"), NumV(-4))
(Name("x"), NumV(2))
(Name("y"), NumV(7))
*/