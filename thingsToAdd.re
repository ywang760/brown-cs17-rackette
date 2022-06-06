// // checkExpect(stringOfValue(ClosureV({cNameList: [Name("x")],
// //            cExpr:
// //             ApplicationE([NameE((Name("+"))), NameE((Name("x"))), NumE(2)]),
// //            cEnv: []})), "User-defined procedure", "print a closure")

//   //  let plus: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(int1), NumV(int2)] => NumV(int1 + int2)
//   //   | [_, _] => failwith("Wrong value type for +")
//   //   | _ => failwith("+ expects 2 arguments")};
//   // let minus: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(int1), NumV(int2)] => NumV(int1 - int2)
//   //   | [_, _] => failwith("Wrong value type for -")
//   //   | _ => failwith("- expects 2 arguments")};
//   // let multiply: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(int1), NumV(int2)] => NumV(int1 * int2)
//   //   | [_, _] => failwith("Wrong value type for *")
//   //   | _ => failwith("* expects 2 arguments")};
//   // let divide: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(int1), NumV(int2)] => NumV(int1 / int2)
//   //   | [_, _] => failwith("Wrong value type for /")
//   //   | _ => failwith("/ expects 2 arguments")};
//   // let rem: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(int1), NumV(int2)] => NumV(int1 mod int2)
//   //   | [_, _] => failwith("Wrong value type for remainder")
//   //   | _ => failwith("remainder expects 2 arguments")};
//   // let equalInt: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(int1), NumV(int2)] => BoolV(int1 == int2)
//   //   | [_, _] => failwith("Wrong value type for =")
//   //   | _ => failwith("= expects 2 arguments")};
//   // let lssThn: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(int1), NumV(int2)] => BoolV(int1 < int2)
//   //   | [_, _] => failwith("Wrong value type for <")
//   //   | _ => failwith("< expects 2 arguments")};
//   // let grtrThn: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(int1), NumV(int2)] => BoolV(int1 > int2)
//   //   | [_, _] => failwith("Wrong value type for >")
//   //   | _ => failwith("> expects 2 arguments")};
//   // let lessOrEqual: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(int1), NumV(int2)] => BoolV(int1 <= int2)
//   //   | [_, _] => failwith("Wrong value type for <=")
//   //   | _ => failwith("<= expects 2 arguments")};
//   // let greaterOrEqual: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(int1), NumV(int2)] => BoolV(int1 >= int2)
//   //   | [_, _] => failwith("Wrong value type for >=")
//   //   | _ => failwith(">= expects 2 arguments")};
//   // let equalP: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(int1), NumV(int2)] => BoolV(int1 == int2)
//   //   | [BoolV(bool1), BoolV(bool2)] => BoolV(bool1 == bool2)
//   //   | [ListV(list1), ListV(list2)] => BoolV(list1 == list2)
//   //   | [_, _] => BoolV(false)
//   //   | _ => failwith("equal? expects 2 arguments")};
//   // let nmbrP: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(_)] => BoolV(true)
//   //   | [_] => BoolV(false)
//   //   | _ => failwith("number? expects 1 argument")};
//   // let zeroP: list(value) => value = va =>
//   //   switch(va) {
//   //   | [NumV(int)] => BoolV(int == 0)
//   //   | [_] => BoolV(false)
//   //   | _ => failwith("zero? expects 1 argument")};
//   // let cons: list(value) => value = va =>
//   //   switch(va) {
//   //   | [item, ListV(items)] => ListV([item, ...items])
//   //   | [_, _] => failwith("Wrong value type for cons")
//   //   | _ => failwith("cons expects 2 arguments")};
//   // let first: list(value) => value = va =>
//   //   switch(va) {
//   //   | [ListV([hd, ..._tl])] => hd
//   //   | [ListV([])] => failwith("first expects a non-empty list")
//   //   | [_] => failwith("Wrong value type for first")
//   //   | _ => failwith("first expects 1 argument")};
//   // let rest: list(value) => value = va =>
//   //   switch(va) {
//   //   | [ListV([_hd, ...tl])] => ListV(tl)
//   //   | [ListV([])] => failwith("rest expects a non-empty list")
//   //   | [_] => failwith("Wrong value type for rest")
//   //   | _ => failwith("rest expects 1 argument")};
//   // let emptyP: list(value) => value = va =>
//   //   switch(va) {
//   //   | [ListV([])] => BoolV(true)
//   //   | [_] => BoolV(false)
//   //   | _ => failwith("empty? expects 1 argument")};
//   // let consP: list(value) => value = va =>
//   //   switch(va) {
//   //   | [ListV([])] => BoolV(false)
//   //   | [ListV([_])] => BoolV(true)
//   //   | [_] => BoolV(false)
//   //   | _ => failwith("cons? expects 1 argument")};
//   // let not: list(value) => value = va =>
//   //   switch(va) {
//   //   | [BoolV(bool)] => BoolV(!bool)
//   //   | [_] => failwith("Wrong value type for not")
//   //   | _ => failwith("not expects 1 argument")};
//   // let list: list(value) => value = va => ListV(va);         


// // checkExpect(stringOfValue(eval(initialTle, [],
// // LambdaE({nameList: [Name("x")],
// //          lambdaBody:
// //           ApplicationE([NameE((Name("+"))), NameE((Name("x"))), NumE(2)])})
// // )), "User-defined procedure", "eval and stringOfValue with lambda")
// // checkExpect(stringOfValue(eval(initialTle, [],
// // LetE({letPairs:
// //        [{pairName: Name("x"), pairExpr: NumE(0)},
// //         {pairName: Name("y"), pairExpr: NumE(1)}],
// //       letBody:
// //        ApplicationE([NameE((Name("+"))), NameE((Name("x"))),
// //                      NameE((Name("y")))])})
// // )), "1", "eval and stringOfValue with let")
// // checkExpect(stringOfValue(eval(initialTle, [], 
// // ApplicationE([NameE((Name("+"))), NumE(1), NumE(2)]))), 
// // "3", "eval and stringOfValue with builtin +")
// // checkExpect(stringOfValue(eval(initialTle, [],
// // ApplicationE([NameE((Name("zero?"))), NumE(9)]))), 
// // "false", "eval and stringOfValue with builtin zero?")
// // checkExpect(stringOfValue(eval(initialTle, [],
// // ApplicationE([LambdaE({nameList: [Name("x")],
// //                        lambdaBody:
// //                         ApplicationE([NameE((Name("+"))), NameE((Name("x"))),
// //                                       NumE(2)])}),
// //               NumE(3)])
// // )), 
// // "5", "eval and stringOfValue with lambda and actual")




// // checkExpect(stringOfValue(eval(initialTle, [], 
// // parseExpression(read("(lambda (x) (+ x 12))"))
// // )), "User-defined procedure", "read and parseExpression and eval and stringOfValue with lambda")
// // checkExpect(stringOfValue(eval(initialTle, [], 
// // parseExpression(read("(let ((x 0) (y 1)) (+ x y))"))
// // )), "1", "read and parseExpression and eval and stringOfValue with let")
// // checkExpect(stringOfValue(eval(initialTle, [], 
// // parseExpression(read("(+ 1 2)"))
// // )), "3", "read and parseExpression and eval and stringOfValue with builtin +")
// // checkExpect(stringOfValue(eval(initialTle, [], 
// // parseExpression(read("(zero? 9)"))
// // )), "false", "read and parseExpression and eval and stringOfValue with builtin zero?")
// // checkExpect(stringOfValue(eval(initialTle, [], 
// // parseExpression(read("((lambda (x) (+ x 2)) 3)"))
// // )), "5", "read and parseExpression and eval and stringOfValue with lambda and actual")
// // checkExpect(stringOfValue(eval(initialTle, [], 
// // parseExpression(read("((lambda (x y) ((lambda (y) (+ x y)) x)) 17 18)"))
// // )), "34", "read and parseExpression and eval and stringOfValue with nested lambdas")
// // checkExpect(stringOfValue(eval(initialTle, [], 
// // parseExpression(read("(let ((x 0) (y 18)) (let ((f (lambda (a b) (+ x b ))) (x 17)) (f y x)))"))
// // )), "17", "read and parseExpression and eval and stringOfValue with let and nested lambda within let")
// // checkExpect(stringOfValue(eval(initialTle, [], 
// // parseExpression(read("(let ((x false) (y 1))
// //                (cond
// //                  ((zero? y) (not x))
// //                  ((not (zero? y)) x)))"))
// // )), "false", "read and parseExpression and eval and stringOfValue with let and cond")
// // checkExpect(stringOfValue(eval(initialTle, [], 
// // parseExpression(read("((lambda (x y z) (+ z
// //                  (if (> x y)
// //                     (+ x y)
// //                     (- x y)))) 1 2 3)"))
// // )), "2", "read and parseExpression and eval and stringOfValue with lambda and if")


// // checkExpect(rackette("(and true false)"), ["false"], "rackette with and")
// // checkExpect(rackette("(or true false)"), ["true"], "rackette with or")
// // checkExpect(rackette("(if false 2 3)"), ["3"], "rackette with if")
// // checkExpect(rackette("(cond (false 3) (true 54))"), ["54"], "rackette with cond")
// // checkExpect(rackette("((lambda (x) (+ x 1)) 9)"), ["10"], "rackette with lambda and actual")
// // checkExpect(rackette("(let ((x 0)) (* x 1))"), ["0"], "rackette with let")
// // checkExpect(rackette("(cons 2 empty)"), ["(cons 2 '())"], "rackette with builtin")
// // checkExpect(rackette("(define f 3) f"), ["3"], "rackette with define with num")
// // checkExpect(rackette("(define f false) f"), ["false"], "rackette with define with bool")
// // checkExpect(rackette("(define f (lambda (x) (+ x 1))) (f 3)"), ["4"], "rackette with define with lambda")
// // checkExpect(rackette("(define my-positive? (lambda (n) (> n 0))) (my-positive? 2)"),
// // ["true"], "rackette with define with lambda2")
// // checkExpect(rackette("(define sum-angles (lambda (n) (* 180 (- n 2)))) (sum-angles 3)"),
// // ["180"], "rackette with define with lambda3")
// // checkExpect(rackette("(define sum (lambda (n) (/ (* n (+ n 1)) 2))) (sum 3)"),
// // ["6"], "rackette with define with lambda4")



// // change error message for 
// // if eval
// // or expects a boolean as first/second argument
// // let/lambda: cannot bound a variable more than ounce



// // checkError(() => rackette("j"), "lookup error")
// // checkError(() => rackette("|"), "invalid character identifier")
// // checkError(() => rackette("("), "wrong number of parentheses")
// // checkError(() => rackette("(+ 1 false)"), "Wrong value type for +")
// // checkError(() => rackette("(* 1 2 3 3 3 3 4)"), "* expects 2 arguments")
// // checkError(() => rackette("(equal? false false false)"), "equal? expects 2 arguments")
// // checkError(() => rackette("(first empty)"), "first expects a non-empty list")
// // checkError(() => rackette("and"), "invalid name")
// // checkError(() => rackette("(and false false false)"), "And needs to take in two expressions")
// // checkError(() => rackette("(or false false false)"), "Or needs to take in two expressions")
// // checkError(() => rackette("(if false false false false)"), "If needs to take in three expressions")
// // checkError(() => rackette("(cond (false 2) (true 2 3))"), "invalid condData")
// // checkError(() => rackette("(cond)"), "Cond needs to take in at least a pair of valid condData")
// // checkError(() => rackette("(lambda (x) )"), "Lambda needs to take in a list of names and an expression")
// // checkError(() => rackette("(let ((x 2 3)) 4)"), "invalid let pair")
// // checkError(() => rackette("(let ((x 2 3)))"), "Let needs to take in a list of pairs and an expression")
// // checkError(() => rackette("(define 3)"), "invalid definition")


// // checkError(() => rackette("(and true 1)"), "and expects a boolean as second argument")
// // checkError(() => rackette("(and 1 1)"), "and expects a boolean as first argument")
// // checkError(() => rackette("(or false 1)"), "or expects a boolean as second argument")
// // checkError(() => rackette("(or 1 1)"), "or expects a boolean as first argument")
// // checkError(() => rackette("(if 1 2 3)"), "if: first argument must evaluate to a boolean")
// // checkError(() => rackette("(cond (false 3))"), "all cases were false")
// // checkError(() => rackette("((lambda (x) (+ x 1)) 1 2)"), "formals and actuals must have the same number")
// // checkError(() => rackette("(lambda (x x) (+ 1 2))"), "lambda: cannot bound a variable more than once")
// // checkError(() => rackette("(let ((x 4) (x 32)) (+ 1 x))"), "let: cannot bound a variable more than once")
// // checkError(() => rackette("f"), "lookup error")
// // checkError(() => rackette("(2)"), "expected a function after open parenthesis")


// // checkError(() => rackette("(define x 1) (define x 2)"), "cannot redefine")

// things to work on:
// (define my-append (lambda (alon1 alon2)
//   (cond
//     ((empty? alon1) alon2)
//     ((cons? alon1) (cons (first alon1) (my-append (rest alon1) alon2))))))
//     (my-append (list 1 2 3) (list 2 3 4))
// (define remove-until-zero (lambda (alon)
//   (cond
//     ((empty? alon) empty)
//     ((cons? alon)
//      (if (= (first alon) 0)
//         alon
//         (remove-until-zero (rest alon)))))))
//         (remove-until-zero (list 1 0 2))
// (define sum-list (lambda (aloi)
//   (cond
//     ((empty? aloi) 0)
//     ((cons? aloi) (+ (first aloi) (sum-list (rest aloi)))))))
//     (sum-list (list 1 2 3 1 23))
// (define count-up (lambda (n k)
//   (if (= k 0)
//      empty
//      (cons n (count-up (+ n 1) (- k 1))))))
//      (count-up 34 8)

//   // let consP: list(value) => value = va =>
//   //   switch(va) {
//   //   | [ListV([hd, ...tl])] => BoolV(true)
//   //   | [_] => BoolV(false)
//   //   | _ => failwith("cons? expects 1 argument")};

    