module AbstractSyntaxTree
open System

type Program = Declaration * Statement

and Statement =
| AssignmentL of L * A
| AssignmentR of String * A * A
| Statements of Statement * Statement
| IfStatement of B * Statement
| IfElseStatement of B * Statement * Statement
| WhileStatement of B * Statement
| Read of L
| Write of A

and Expression = // shouldn't these be used in statements?
| ExpL of L
| ExpA of A
| ExpB of B

and L =
| LabelX of String
| LabelA of String * A
| LabelFstR of String
| LabelSndR of String
and A =
| ArithmeticN of int
| ArithmeticX of string
| ArithmeticA of string * A
| ArithmeticFstR of string
| ArithmeticSndR of string
| ROp of A * ArithmeticOperator * A
and B =
| BoolValue of bool
| AOp of A * RelationalOperator * A
| BOp of B * BooleanOperator * B
| Not of B

and Operator =
| OpA of ArithmeticOperator
| OpR of RelationalOperator
| OpB of BooleanOperator

and ArithmeticOperator =
    | Plus
    | Minus
    | Multiply
    | Divide
and RelationalOperator =
    | LessThan
    | GreaterThan
    | LesserOrEqualTo
    | GreaterOrEqualTo
    | EqualTo
    | NotEqualTo
and BooleanOperator =
    | AndOp
    | OrOp


and Declaration =
    | DeclarationX of string                    // Declaration of variable 
    | DeclarationA of string * int              // declaration of array 
    | DeclarationR of string                    // Declaration of record
    | DeclarationD of Declaration * Declaration
