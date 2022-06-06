type rawProgram = string;

type concreteProgramPiece =
  | NumberC(int)
  | SymbolC(string)
  | ListC(list(concreteProgramPiece));

type concreteProgram = list(concreteProgramPiece);

/* a Rackette name */
type name =
  | Name(string);

/* a Rackette expression */
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
  
/* a Rackette definition */
type definition = (name, expression);

/* a piece of Rackette that can be processed:
 * either a definition or an expression */
type abstractProgramPiece =
  | Definition(definition)
  | Expression(expression);

/* a representation of a Rackette program -
 * any number of pieces */
type abstractProgram = list(abstractProgramPiece);

/* a Rackette value: the result of evaluating a Rackette expression */
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
