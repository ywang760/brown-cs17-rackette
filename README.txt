The user should type in rackette and then their procedure as a string. For example, the procedure (+ 3 5) would be written as rackette(“(+ 3 5)”). This procedure application expression will be read, parsed, evaluated and a string value will be returned to the user. In this case “8” would be returned. In some cases, the user might type in a rackette program that might run into error, so the user should revise the syntax based on the error message. 

Upon taking a deeper dive into the workings of rackette, the users will find that their program will be taken in and read. This means the raw program will be turned into concrete pieces and those pieces will make up a concrete program. Then parse will take in the concrete program, turning concrete pieces into abstract pieces and turning concrete programs into abstract programs. This new abstract program will be composed of either definitions or expressions. Definitions are added to an environment by the procedure addDefinition and expressions are evaluated by the procedure eval.  Eval assesses the different expressions and after evaluations will return a value. Process will combine the definitions and expressions and output a list of values. These values will then be returned to the user by the stringOfValue procedure and the program ends.

There are no possible bugs that we can think of.

This project is the collaboration of Yutong Wang and Joshua Dover. We also received help from TA’s Isabel, Colby, and Harshini. 

Our rackette program also has a builtin "list" procedure that can take in a list of values and return a ListV value whose argument is a list containing the values from the input list. 
