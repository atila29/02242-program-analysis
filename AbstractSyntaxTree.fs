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
with override this.ToString() = 
      match this with
      | AssignmentL(l, a) -> string l + " := " + string a + ";"
      | AssignmentR(x, a1, a2) -> x + " := (" + string a1 + ", " + string a2 + ");"
      | IfStatement(b, stm) -> "if (" + string b + ") {" + string stm + "}"
      | IfElseStatement(b, stm1, stm2) -> "if (" + string b + ") {" + string stm1 + "} else {" + string stm2+ "}"
      | WhileStatement(b, stm) -> "while (" + string b + ") {" + string stm + "}"
      | Read(l) -> "read " + string l + ";"
      | Write(a) -> "write " + string a + ";"
      | Statements(stm1, stm2) -> string stm1 + "\n" + string stm2

and L =
| LabelX of string
| LabelA of string * A
| LabelFstR of string
| LabelSndR of string
with override this.ToString() =
      match this with
      | LabelX(x) -> x
      | LabelA(x, a) -> x + "[" + string a + "]"
      | LabelFstR(x) -> x + ".fst"
      | LabelSndR(x) -> x + ".snd"

and A =
| ArithmeticN of int
| ArithmeticX of string
| ArithmeticA of string * A
| ArithmeticFstR of string
| ArithmeticSndR of string
| ROp of A * ArithmeticOperator * A
| ArithmeticNeg of A
with override this.ToString() =
      match this with
      | ArithmeticN(n) -> string n
      | ArithmeticX(x) -> x
      | ArithmeticA(x, a) -> x + "[" + string a + "]"
      | ArithmeticFstR(x) -> x + ".fst"
      | ArithmeticSndR(x) -> x + ".snd"
      | ROp(a1, op, a2) -> string a1 + " " + string op + " " + string a2
      | ArithmeticNeg(a) -> "-" + string a

and B =
| BoolValue of bool
| AOp of A * RelationalOperator * A
| BOp of B * BooleanOperator * B
| Not of B
with override this.ToString() =
      match this with
      | BoolValue(b) -> string b
      | AOp(a1, op, a2) -> string a1 + " " + string op + " " + string a2
      | BOp(b1, op, b2) -> string b1 + " " + string op + " " + string b2
      | Not(b) -> "!" + string b

and Operator =
| OpA of ArithmeticOperator
| OpR of RelationalOperator
| OpB of BooleanOperator
with override this.ToString() = 
      match this with
      | OpA(op) -> string op
      | OpR(op) -> string op
      | OpB(op) -> string op

and ArithmeticOperator =
    | Plus
    | Minus
    | Multiply
    | Divide
with override this.ToString() = 
      match this with
      | Plus -> "+"
      | Minus -> "-"
      | Multiply -> "*"
      | Divide -> "/"

and RelationalOperator =
    | LessThan
    | GreaterThan
    | LesserOrEqualTo
    | GreaterOrEqualTo
    | EqualTo
    | NotEqualTo
with override this.ToString() = 
      match this with
      | LessThan -> "<"
      | GreaterThan -> ">"
      | LesserOrEqualTo -> "<="
      | GreaterOrEqualTo -> ">="
      | EqualTo -> "=="
      | NotEqualTo -> "!="

and BooleanOperator =
    | AndOp
    | OrOp
with override this.ToString() = 
      match this with
      | AndOp -> "&"
      | OrOp -> "|"

and Declaration =
    | DeclarationEmpty
    | DeclarationX of string                    // Declaration of variable 
    | DeclarationA of string * int              // declaration of array 
    | DeclarationR of string                    // Declaration of record
    | DeclarationD of Declaration * Declaration
with override this.ToString() =
      match this with
      | DeclarationEmpty -> ""
      | DeclarationX(x) -> "int " + x
      | DeclarationA(x, n) -> "int[" + string n + "] " + x
      | DeclarationR(x) -> "{int fst; int snd} " + x
      | DeclarationD(d1, d2) -> string d1 + "\n" + string d2