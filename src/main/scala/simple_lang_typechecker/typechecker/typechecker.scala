package simple_lang_typechecker.typechecker

class IllTypedException(message: String) extends Exception(message)

object Typechecker {
	def typeOfExp(exp: Exp, env: Map[Variable, Type]): Type = {
		exp match {
			case p : PrimaryExp => p match {	//matched a primary expression
				case IntegerLiteralExp(_) => IntType
				case BooleanLiteralExp(_) => BoolType
				case StringLiteralExp(_) => StringType
				case VariableExp(name) => {
					env.getOrElse(name, throw new IllTypedException("variable" + name + "not in scope"))
				}
			}
			case b : BinopExp => b match {	//matched a binop expression
				case AdditiveExp(left, op, right) => (op, typeOfExp(left, env), typeOfExp(right, env)) match {
					case (PlusOp, IntType, IntType) => IntType
					case (PlusOp, StringType, StringType) => StringType
					case (MinusOp, IntType, IntType) => IntType
					case _ => throw new IllTypedException("incompatible operands for an additive exp")
				}
				case ComparisonExp(left, op, right) => (op, typeOfExp(left, env), typeOfExp(right, env)) match {
					case (LessThanOp, IntType, IntType) => BoolType
					case (GreaterThanOp, IntType, IntType) => BoolType
					case _ => throw new IllTypedException("incompatible operands for a comparison exp")
				}
				case EqualsExp(left, op, right) => {
					val leftType = typeOfExp(left, env)
					val rightType = typeOfExp(right, env)
					(op, leftType, rightType) match {
						case (_, leftType, rightType) if leftType == rightType => BoolType
						case _ => throw new IllTypedException("operand mismatch for an equals exp")
					}
				}
			}
			case _ => throw new IllTypedException("reached end of typeOfExp without matching")
		}
	}
}