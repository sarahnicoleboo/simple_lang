package simple_lang_typechecker.typechecker

//types
sealed trait Type
case object IntType extends Type
case object BoolType extends Type
case object StringType extends Type

//operators
sealed trait BinOp
case object PlusOp extends BinOp
case object MinusOp extends BinOp
case object LessThanOp extends BinOp
case object GreaterThanOp extends BinOp
case object ExactlyEqualsOp extends BinOp
case object NotEqualsOp extends BinOp

sealed trait UnaryOp
case object NotOp extends UnaryOp
case object NegateOp extends UnaryOp
//end of operators

case class Variable(name: String)

//expressions
sealed trait Exp
//primary exps
case class VariableExp(name: Variable) extends Exp
case class IntegerLiteralExp(value: Int) extends Exp
case class BooleanLiteralExp(value: Boolean) extends Exp
case class StringLiteralExp(value: String) extends Exp
//binary exps
case class BinopExp(left: Exp, op: BinOp, right: Exp) extends Exp
//unary exps
case class UnaryExp(op: UnaryOp, exp: Exp) extends Exp
//end of exps

//stmts
sealed trait Stmt
case class VariableDeclarationStmt(theType: Type, name: Variable, exp: Exp) extends Stmt
case class VariableAssignmentStmt(name: Variable, exp: Exp) extends Stmt
case class IfStmt(guard: Exp, ifTrue: Stmt, ifFalse: Stmt) extends Stmt
case class WhileStmt(guard: Exp, body: Stmt) extends Stmt
case class PrintStmt(exps: Seq[Exp]) extends Stmt
case class BlockStmt(smts: Seq[Stmt]) extends Stmt
case class ExpStmt(exp: Exp) extends Stmt

case class Program(stmt: Stmt)




