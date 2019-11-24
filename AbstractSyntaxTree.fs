module AbstractSyntaxTree

type Program = Declaration * Statement

and Statement =
| AssignmentL of L * A
| AssignmentR of string * A * A
| Statements of Statement * Statement
| IfStatement of B * Statement
| IfElseStatement of B * Statement * Statement
| WhileStatement of B * Statement
| Read of L
| Write of A

and L =
| LabelX of string
| LabelA of string * A
| LabelFstR of string
| LabelSndR of string

and A =
| ArithmeticN of int
| ArithmeticX of string
| ArithmeticA of string * A
| ArithmeticFstR of string
| ArithmeticSndR of string
| ROp of A * ArithmeticOperator * A
| ArithmeticNeg of A

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
    | DeclarationEmpty
    | DeclarationX of string                    // Declaration of variable 
    | DeclarationA of string * int              // declaration of array 
    | DeclarationR of string                    // Declaration of record
    | DeclarationD of Declaration * Declaration
