open Types;

/* ======================== Printing helper procedures ====================== */

/* ------------------------------- printGreen ---------------------------------
 * Input: s, a string to print
 * Output: nothing, but s will be printed with a green color
 */
let printGreen: string => unit =
  s => print_endline("\027[32m" ++ s ++ "\027[0m");

/* -------------------------------- printRed ----------------------------------
 * Input: s, a string to print
 * Output: nothing, but s will be printed with a red color
 */
let printRed: string => unit =
  s => print_endline("\027[31m" ++ s ++ "\027[0m");

/* ------------------------------ stringOfAList --------------------------------
 * Input: lst, a list of type 'a
 *        strOf, a procedure that turns type 'a to a string
 * Output: the string representation of lst
 */
let stringOfAList: (list('a), 'a => string) => string =
  (lst, strOf) =>
  "[" ++
    List.fold_right(
      (a, b) => strOf(a) ++ (if (b == "]") { "" } else { ", " }) ++ b,
      lst,
      "]"
    );

/* ---------------------- stringOfConcreteProgramPiece -------------------------
 * Input: concrpiece, an concreteProgramPiece
 * Output: the string representation of concrpiece
 */
let rec stringOfConcreteProgramPiece: concreteProgramPiece => string =
  concrpiece =>
    switch (concrpiece) {
    | NumberC(intx) => "NumberC(" ++ string_of_int(intx) ++ ")"
    | SymbolC(stringx) => "SymbolC(" ++ stringx ++ ")"
    | ListC(concreteProgramPieceList) =>
      "ListC("
      ++ stringOfConcreteProgram(concreteProgramPieceList: concreteProgram)
      ++ ")"
    }
/* ------------------------ stringOfConcreteProgram ----------------------------
 * Input: concrprog, an concreteProgram
 * Output: the string representation of concrprog
 */
and stringOfConcreteProgram: concreteProgram => string =
  concrprog => stringOfAList(concrprog, stringOfConcreteProgramPiece);

/* ------------------------------ stringOfName ---------------------------------
 * Input: namex, a name
 * Output: the string representation of namex
 */
let stringOfName: name => string =
  namex =>
    switch (namex) {
    | Name(stringx) => "Name(" ++ stringx ++ ")"
    };

/* ---------------------------- stringOfNameList -------------------------------
 * Input: nameList, a list of names
 * Output: the string representation of nameList
 */
let stringOfNameList: list(name) => string =
  nameList => stringOfAList(nameList, stringOfName);

/* --------------------------- stringOfExpression ------------------------------
 * Input: expr, an expression
 * Output: the string representation of expr
 */
let rec stringOfExpression: expression => string =
  expr =>
    switch (expr) {
    | NumE(intx) => "NumE(" ++ string_of_int(intx) ++ ")"
    | BoolE(boolx) => "BoolE(" ++ string_of_bool(boolx) ++ ")"
    | EmptyE => "EmptyE"
    | NameE(namex) => "NameE(" ++ stringOfName(namex) ++ ")"
    | AndE(expr1, expr2) =>
      "AndE("
      ++ stringOfExpression(expr1)
      ++ ", "
      ++ stringOfExpression(expr2)
      ++ ")"
    | OrE(expr1, expr2) =>
      "OrE("
      ++ stringOfExpression(expr1)
      ++ ", "
      ++ stringOfExpression(expr2)
      ++ ")"
    | IfE(ifRec) =>
      "IfE({"
      ++ "boolExpr: "
      ++ stringOfExpression(ifRec.boolExpr)
      ++ ", "
      ++ "trueExpr: "
      ++ stringOfExpression(ifRec.trueExpr)
      ++ ", "
      ++ "falseExpr: "
      ++ stringOfExpression(ifRec.falseExpr)
      ++ ","
      ++ "})"
    | CondE((exprpairlist: list(condData))) =>
      let stringOfCondRecord: condData => string = (
        condD =>
          "{"
          ++ "conditionExpr: "
          ++ stringOfExpression(condD.conditionExpr)
          ++ ", "
          ++ "resultExpr: "
          ++ stringOfExpression(condD.resultExpr)
          ++ ","
          ++ "}"
      );
      "CondE(" ++ stringOfAList(exprpairlist, stringOfCondRecord) ++ ")";
    | LambdaE(lambdaD) =>
      "LambdaE({"
      ++ "nameList: "
      ++ stringOfNameList(lambdaD.nameList)
      ++ ", "
      ++ "lambdaBody: "
      ++ stringOfExpression(lambdaD.lambdaBody)
      ++ ","
      ++ "})"
    | LetE(letD) =>
      let stringOfLetExpressionPair: letPair => string = (
        pair =>
          "{"
          ++ "pairName: "
          ++ stringOfName(pair.pairName)
          ++ ", "
          ++ "pairExpr: "
          ++ stringOfExpression(pair.pairExpr)
          ++ "}"
      );
      "LetE({"
      ++ stringOfAList(letD.letPairs, stringOfLetExpressionPair)
      ++ ", "
      ++ "letBody: "
      ++ stringOfExpression(letD.letBody)
      ++ ","
      ++ "})";
    | ApplicationE(expressionlist) =>
      "ApplicationE({"
      ++ stringOfAList(expressionlist, stringOfExpression)
      ++ "})"
    };

/* --------------------------- stringOfDefinition ------------------------------
 * Input: def, a definition
 * Output: the string representation of def
 */
let stringOfDefinition: definition => string =
  def => {
    let (name1, expr1) = def;
    "(" ++ stringOfName(name1) ++ ", " ++ stringOfExpression(expr1) ++ ")";
  };

/* ---------------------- stringOfAbstractProgramPiece -------------------------
 * Input: piece, an abstractProgramPiece
 * Output: the string representation of piece
 */
let stringOfAbstractProgramPiece: abstractProgramPiece => string =
  piece =>
    switch (piece) {
    | Expression(expr) => "Expression(" ++ stringOfExpression(expr) ++ ")"
    | Definition(def) => "Definition(" ++ stringOfDefinition(def) ++ ")"
    };

/* ------------------------ stringOfAbstractProgram ----------------------------
 * Input: abstr, an abstractProgram
 * Output: the string representation of abstr
 */
let stringOfAbstractProgram: abstractProgram => string =
  abstr => stringOfAList(abstr, stringOfAbstractProgramPiece);


/* =========================== Testing procedures ========================== */

/* ------------------------------- checkExpect --------------------------------
 * Inputs: actual, given value to be checked against expected
 *         expected, the expected value of actual
 *         message, a string describing the test
 * NOTE: You can use this procedure on any type, but it will only print nice on
 *       failure if it's an atomic data type like string, int, float, bool.
 * Output: nothing (procedure prints information), but succeeds if actual and
 *         expected are the same and fails otherwise
 */
let checkExpect: ('a, 'a, string) => unit =
  (actual, expected, message) =>
    if (actual == expected) {
      printGreen("ce_Success: " ++ message);
    } else {
      printRed("ce_Fail: " ++ message);
      printRed("expected output: ");
      Js.log(expected);
      printRed("actual output: ");
      Js.log(actual);
    };

/* --------------------- checkExpectConcreteProgramPiece -----------------------
 * Inputs: actual, given concreteProgramPiece to be checked against expected
 *         expected, the expected value of actual
 *         message, a string describing the test
 * Output: nothing (procedure prints information), but succeeds if actual and
 *         expected are the same and fails otherwise
 */
let checkExpectConcreteProgramPiece:
  (concreteProgramPiece, concreteProgramPiece, string) => unit =
  (actual, expected, message) =>
    if (actual == expected) {
      printGreen("ceSuccess: " ++ message);
    } else {
      printRed("ceFail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfConcreteProgramPiece(expected));
      printRed("actual output: ");
      printRed(stringOfConcreteProgramPiece(actual));
    };

/* ----------------------- checkExpectConcreteProgram --------------------------
 * Inputs: actual, given concreteProgram to be checked against expected
 *         expected, the expected value of actual
 *         message, a string describing the test
 * Output: nothing (procedure prints information), but succeeds if actual and
 *         expected are the same and fails otherwise
 */
let checkExpectConcreteProgram:
  (concreteProgram, concreteProgram, string) => unit =
  (actual, expected, message) =>
    if (actual == expected) {
      printGreen("ceSuccess: " ++ message);
    } else {
      printRed("ceFail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfConcreteProgram(expected));
      printRed("actual output: ");
      printRed(stringOfConcreteProgram(actual));
    };

/* ----------------------------- checkExpectName -------------------------------
 * Inputs: actual, given name to be checked against expected
 *         expected, the expected value of actual
 *         message, a string describing the test
 * Output: nothing (procedure prints information), but succeeds if actual and
 *         expected are the same and fails otherwise
 */
let checkExpectName: (name, name, string) => unit =
  (actual, expected, message) =>
    if (actual == expected) {
      printGreen("ceSuccess: " ++ message);
    } else {
      printRed("ceFail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfName(expected));
      printRed("actual output: ");
      printRed(stringOfName(actual));
    };

/* ----------------------- checkExpectExpression --------------------------
 * Inputs: actual, given expression to be checked against expected
 *         expected, the expected value of actual
 *         message, a string describing the test
 * Output: nothing (procedure prints information), but succeeds if actual and
 *         expected are the same and fails otherwise
 */
let checkExpectExpression: (expression, expression, string) => unit =
  (actual, expected, message) =>
    if (actual == expected) {
      printGreen("ceSuccess: " ++ message);
    } else {
      printRed("ceFail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfExpression(expected));
      printRed("actual output: ");
      printRed(stringOfExpression(actual));
    };

/* -------------------------- checkExpectDefinition ----------------------------
 * Inputs: actual, given definition to be checked against expected
 *         expected, the expected value of actual
 *         message, a string describing the test
 * Output: nothing (procedure prints information), but succeeds if actual and
 *         expected are the same and fails otherwise
 */
let checkExpectDefinition: (definition, definition, string) => unit =
  (actual, expected, message) =>
    if (actual == expected) {
      printGreen("ceSuccess: " ++ message);
    } else {
      printRed("ceFail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfDefinition(expected));
      printRed("actual output: ");
      printRed(stringOfDefinition(actual));
    };

/* --------------------- checkExpectAbstractProgramPiece -----------------------
 * Inputs: actual, given abstractProgramPiece to be checked against expected
 *         expected, the expected value of actual
 *         message, a string describing the test
 * Output: nothing (procedure prints information), but succeeds if actual and
 *         expected are the same and fails otherwise
 */
let checkExpectAbstractProgramPiece:
  (abstractProgramPiece, abstractProgramPiece, string) => unit =
  (actual, expected, message) =>
    if (actual == expected) {
      printGreen("ceSuccess: " ++ message);
    } else {
      printRed("ceFail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfAbstractProgramPiece(expected));
      printRed("actual output: ");
      printRed(stringOfAbstractProgramPiece(actual));
    };

/* ----------------------- checkExpectAbstractProgram --------------------------
 * Inputs: actual, given abstractProgram to be checked against expected
 *         expected, the expected value of actual
 *         message, a string describing the test
 * Output: nothing (procedure prints information), but succeeds if actual and
 *         expected are the same and fails otherwise
 */
let checkExpectAbstractProgram:
  (abstractProgram, abstractProgram, string) => unit =
  (actual, expected, message) =>
    if (actual == expected) {
      printGreen("ceSuccess: " ++ message);
    } else {
      printRed("ceFail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfAbstractProgram(expected));
      printRed("actual output: ");
      printRed(stringOfAbstractProgram(actual));
    };

/* ------------------------------- checkWithin --------------------------------
 * Input: actual, given float value to be checked against expected
 *        expected, the expected value of actual
 *        within, the margin of error
 * Output: nothing, but succeeds if input and expected are in within of each
 *                  other and fails otherwise
 */
let checkWithin: (float, float, float) => unit =
  (actual, expected, within) =>
    if (abs_float(actual -. expected) <= abs_float(within)) {
      printGreen("cwSuccess ");
    } else {
      printRed("cwFail ");
      printRed("expected output: ");
      Js.log(expected);
      printRed("actual output: ");
      Js.log(actual);
    };

/* -------------------------------- checkError --------------------------------
 * Inputs: thunk, a zero-argument procedure that outputs the thing that
 *                will throw an error
 *         expect, a string of the expected error message (whatever text was
 *                 put into the failwith)
 * Output: nothing (procedure prints information), but succeeds if calling
 *         thunk throws an error with the message expect and fails otherwise
 */
let checkError: (unit => 'a, string) => unit =
  (thunk, expect) =>
    try (
      {
        ignore(thunk());
        failwith("Error did not occur");
      }
    ) {
    | Failure(err) when err == expect => printGreen("checkErrorSuccess ")
    | Failure(err) when err == "Error did not occur" =>
      printRed("Error did not occur")
    | Failure(err) =>
      printRed(
        "checkErrorFail. Expected error: "
        ++ expect
        ++ "; Actual error: "
        ++ err,
      )
    };