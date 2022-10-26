package simple_lang_typechecker.typechecker

//types
sealed trait Type
case object IntType extends Type
case object BoolType extends Type
case object StringType extends Type

//operators
sealed trait Op
sealed trait AdditiveOp extends Op
case object PlusOp extends AdditiveOp
case object MinusOp extends AdditiveOp

sealed trait ComparisonOp extends Op
case object LessThanOp extends ComparisonOp
case object GreaterThanOp extends ComparisonOp

sealed trait EqualsOp extends Op
case object ExactlyEqualsOp extends EqualsOp
case object NotEqualsOp extends EqualsOp
//end of operators

case class Variable(name: String)

//expressions
sealed trait Exp
//primary exps
sealed trait PrimaryExp extends Exp
case class VariableExp(name: Variable) extends PrimaryExp
case class IntegerLiteralExp(value: Int) extends PrimaryExp
case class BooleanLiteralExp(value: Boolean) extends PrimaryExp
case class StringLiteralExp(value: String) extends PrimaryExp

sealed trait BinopExp extends Exp
case class AdditiveExp(left: PrimaryExp, op: AdditiveOp, right: PrimaryExp) extends BinopExp
case class ComparisonExp(left: AdditiveExp, op: ComparisonOp, right: AdditiveExp) extends BinopExp
case class EqualsExp(left: ComparisonExp, op: EqualsOp, right: ComparisonExp) extends BinopExp
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




