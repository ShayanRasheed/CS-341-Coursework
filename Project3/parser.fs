//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// Shayan Rasheed
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  let private matchToken expected_token tokens =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

  // EMPTY - this portion of the code contains ;
  let private empty tokens =
    matchToken ";" tokens

  // EXPR-OP
  let private exprop (tokens:string list) =
    let next_token = List.head tokens

    // Check the next token in the list
    // if it is an operator, match it
    // Otherwise, throw an error
    if next_token.Equals("+") then
      matchToken "+" tokens
    elif next_token.Equals("-") then
      matchToken "-" tokens
    elif next_token.Equals("*") then
      matchToken "*" tokens
    elif next_token.Equals("/") then
      matchToken "/" tokens
    elif next_token.Equals("^") then
      matchToken "^" tokens
    elif next_token.Equals(">") then
      matchToken ">" tokens
    elif next_token.Equals(">=") then
      matchToken ">=" tokens
    elif next_token.Equals("<") then
      matchToken "<" tokens
    elif next_token.Equals("<=") then
      matchToken "<=" tokens
    elif next_token.Equals("==") then
      matchToken "==" tokens
    elif next_token.Equals("!=") then
      matchToken "!=" tokens
    else
      failwith ("expecting expression operator, but found " + next_token)

  // EXPR-VALUE
  let private exprvalue (tokens:string list) =
    let next_token = List.head tokens

    // Check if the next token is a valid value
    // Otherwise, throw an error
    if next_token.Equals("true") then
      matchToken "true" tokens
    elif next_token.Equals("false") then
      matchToken "false" tokens
    elif next_token.StartsWith("identifier") then
      matchToken next_token tokens
    elif next_token.StartsWith("str_literal") then
      matchToken next_token tokens
    elif next_token.StartsWith("int_literal") then
      matchToken next_token tokens
    else
      failwith ("expecting identifier or literal, but found " + next_token)

  // EXPRESSION
  let private expression (tokens:string list) =
    // First token in an expression should be a value
    let T2 = exprvalue tokens

    // The expression could be just one value
    let next_token = List.head T2
    if next_token.Equals(";") || next_token.Equals(")") || next_token.StartsWith("int_literal") then
      T2
    // Or it could have an operator and another value
    else
      let T3 = exprop T2
      let T4 = exprvalue T3
      T4

  // VAR DECLARE
  let private vardecl (tokens:string list) =
    // First token should be "int"
    let T2 = matchToken "int" tokens
    let next_token = List.head T2

    // Make sure that the next token is an identifier
    if next_token.StartsWith("identifier") then 
      let T3 = matchToken next_token T2
      // Next token is ;
      let T4 = matchToken ";" T3
      T4
    else
      let T3 = matchToken "identifier" T2
      T3

  // INPUT
  let private input (tokens:string list) =
    // Input starts with cin >>
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let next_token = List.head T3

    // Make sure next token is an identifier
    if next_token.StartsWith("identifier") then 
      let T4 = matchToken next_token T3
      let T5 = matchToken ";" T4
      T5
    else
      let T4 = matchToken "identifier" T3
      T4

  // OUTPUT VALUE
  let private outputValue (tokens:string list) =
    // Output value can be either endl or a value
    let next_token = List.head tokens

    if next_token.Equals("endl") then 
      matchToken "endl" tokens
    else
      exprvalue tokens

  // OUTPUT
  let private output (tokens:string list) =
    // Output starts with cout <<
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    // Then a valid value, followed by ;
    let T4 = outputValue T3
    let T5 = matchToken ";" T4
    T5

  // ASSIGNMENT
  let private assignment (tokens:string list) =
    // Assignments must start with an identifier
    let next_token = List.head tokens

    if next_token.StartsWith("identifier") then 
      let T2 = matchToken next_token tokens
      // Then =
      let T3 = matchToken "=" T2
      // Then a valid expression
      let T4 = expression T3
      // Then ;
      let T5 = matchToken ";" T4
      T5
    else
      let T2 = matchToken "identifier" tokens
      T2

  // STATEMENT
  let rec private stmt tokens =
    // This function determines the type of statement and calls the appropriate function
    let next_token = List.head tokens

    // If it is just ;, then call empty
    if next_token.Equals(";") then
      empty tokens
    // "if" means it is an if statement
    elif next_token.Equals("if") then
      ifstmt tokens
    // "cin" is input
    elif next_token.Equals("cin") then
      input tokens
    // "cout" is output
    elif next_token.Equals("cout") then
      output tokens
    // "int" is a variable declaration
    elif next_token.Equals("int") then
      vardecl tokens
    // "identifier" is an assignment
    elif next_token.StartsWith("identifier") then
      assignment tokens
    // If none of the above, error occurs
    else
      failwith("expecting statement, but found " + next_token)

  // THEN STMT
  and private then_part tokens =
    stmt tokens

  // ELSE STMT
  and private else_part (tokens:string list) =
    let next_token = List.head tokens
    // Can either be "else" or nothing
    if next_token.Equals("else") then
      let T2 = matchToken "else" tokens
      // Check for a statement after else
      let T3 = stmt T2
      T3
    // If there is no else statement, simply return tokens
    else
      tokens

  // IF STMT
  and private ifstmt tokens =
    // If statements start with "if ("
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    // Check for expression inside parenthesis
    let T4 = expression T3
    // Check for closing parenthesis
    let T5 = matchToken ")" T4
    // Now call functions for then and else stmts
    let T6 = then_part T5
    let T7 = else_part T6
    T7
      

  // MORE STATEMENTS FUNCTION
  let rec morstmts tokens =
    let next_token = List.head tokens

    // If next token is a closing bracket, there are no more stmts
    if next_token.Equals("}") then
      tokens
    else
      // Otherwise, continue calling stmt and morstmt
      let T2 = stmt tokens
      let T3 = morstmts T2
      T3

  // STATEMENTS FUNCTION
  let rec stmts tokens =
    let T2 = stmt tokens
    let T3 = morstmts T2
    T3

  //
  // simpleC
  //
  let private simpleC tokens = 
    // Check for void main() { } $
    // as well as statement(s) in between the brackets
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5

    let T7 = stmts T6

    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8
    T9


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message
