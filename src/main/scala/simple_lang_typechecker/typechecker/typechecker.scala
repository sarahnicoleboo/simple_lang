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
					env.getOrElse(name, throw new IllTypedException(s"variable $name not in scope"))
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
	
	def typecheckStmt(stmt: Stmt, env: Map[Variable, Type]): Map[Variable, Type] = {
		stmt match {
			case VariableDeclarationStmt(variableType, variable, exp) => {
				val expType = typeOfExp(exp, env)
				if (!env.contains(variable)) {
					if (variableType == expType) {
						env + (variable -> variableType)
					} else {
					throw new IllTypedException(s"expected $variableType; received $expType")
					}
				} else {
				throw new IllTypedException("Variable already declared in this scope")
				}
			}
			case VariableAssignmentStmt(variable, exp) => {
				if (!env.contains(variable)) {
					throw new IllTypedException("Variable not in scope")
				}
				env
			}
			case IfStmt(guard, ifTrue, ifFalse) => {
				if (typeOfExp(guard, env) == BoolType) {
					typecheckStmt(ifTrue, env)
					typecheckStmt(ifFalse, env)
					env
				} else {
					throw new IllTypedException("Guard should be of type boolean")
				}
			}
			case WhileStmt(guard, body) => {
				if (typeOfExp(guard, env) == BoolType) {
					typecheckStmt(body, env)
					env
				} else {
					throw new IllTypedException("Guard should be of type boolean")
				}
			}
			case PrintStmt(exps) => {
				for (exp <- exps) typeOfExp(exp, env)
				env
			}
			case BlockStmt(stmts) => {
				stmts.foldLeft(env)((currentEnv, currentStmt) => 
					typecheckStmt(currentStmt, currentEnv))
				env
			}
			case ExpStmt(exp) => {
				typeOfExp(exp, env)
				env
			}
		}
	}
}