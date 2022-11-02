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
					case (_, IntType, IntType) => BoolType
					case _ => throw new IllTypedException("incompatible operands for a comparison exp")
				}
				case EqualsExp(left, op, right) => {	//doesn't matter what types they are as long as they're the same on both sides
					val leftType = typeOfExp(left, env)
					val rightType = typeOfExp(right, env)
					(op, leftType, rightType) match {
						case (_, leftType, rightType) if leftType == rightType => BoolType
						case _ => throw new IllTypedException("operand mismatch for an equals exp")
					}
				}
			}
			case u : UnaryExp => u match {
				case NotExp(op, exp) => (op, typeOfExp(exp, env)) match {
					case (NotOp, BoolType) => BoolType
					case _ => throw new IllTypedException("operand n/a to logical not")
				}
				case NegateExp(op, exp) => (op, typeOfExp(exp, env)) match {
					case (NegateOp, IntType) => IntType
					case _ => throw new IllTypedException("operand n/a to integer negation")
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
	
	def typecheckProgram(stmts: Seq[Stmt], env: Map[Variable, Type]) = {
		for (stmt <- stmts) typecheckStmt(stmt, env)
	}
} // end of Typechecker

object Generator {
	import simple_lang_typechecker.unification._
	import simple_lang_typechecker.unification.UIterator._
	
	trait UnificationType {}
	val intUnificationType = VariantOf[UnificationType]("intType")
	val boolUnificationType = VariantOf[UnificationType]("boolType")
	val stringUnificationType = VariantOf[UnificationType]("stringType")
	
	//helpers for generating expressions with binary operators:
	//each tuple: Op, leftType, rightType, resultType
	//WHY NOT JUST MAKE THIS AN ITERATOR?
	val binops: Seq[(BinOp, Term[UnificationType], Term[UnificationType], Term[UnificationType])] =
		Seq (
			(PlusOp, intUnificationType, intUnificationType, intUnificationType),
			(PlusOp, stringUnificationType, stringUnificationType, stringUnificationType),
			(MinusOp, intUnificationType, intUnificationType, intUnificationType),
			(LessThanOp, intUnificationType, intUnificationType, boolUnificationType),
			(GreaterThanOp, intUnificationType, intUnificationType, intUnificationType),
			(ExactlyEqualsOp, intUnificationType, intUnificationType, boolUnificationType),
			(ExactlyEqualsOp, boolUnificationType, boolUnificationType, boolUnificationType),
			(ExactlyEqualsOp, stringUnificationType, stringUnificationType, boolUnificationType),
			(NotEqualsOp, intUnificationType, intUnificationType, boolUnificationType),
			(NotEqualsOp, boolUnificationType, boolUnificationType, boolUnificationType),
			(NotEqualsOp, stringUnificationType, stringUnificationType, boolUnificationType)
		)
		
		//change Unit here later
	def binopHelper(leftType: Term[UnificationType], rightType: Term[UnificationType], resultType: Term[UnificationType]): UIterator[Unit, BinOp] = {
		for {
			(op, expectedLeft, expectedRight, expectedResult) <- toUIterator(binops.iterator)
			_ <- unify(leftType, expectedLeft)
			_ <- unify(rightType, expectedRight)
			_ <- unify(resultType, expectedResult)
		} yield op
	}
	
	type GenTypeEnv = Map[Variable, Term[UnificationType]]
	
	//change Unit here later
	def genExp(env: GenTypeEnv, ofType: Term[UnificationType]): UIterator[Unit, Exp] = {
		disjuncts(
			for {	//integer literal
				_ <- unify(ofType, intUnificationType)
			} yield IntegerLiteralExp(0),	//how do i make this not just 0?
			for {	//boolean literal
				_ <- unify(ofType, boolUnificationType)
			} yield BooleanLiteralExp(true), 	//how do i make this not just true?
			for {	//string literal
				_ <- unify(ofType, stringUnificationType)
			} yield StringLiteralExp("hi"),		//how do i make this not just "hi"?
			for {	//variable
				(variable, variableType) <- toUIterator(env.iterator)
				_ <- unify(ofType, variableType)
			} yield VariableExp(variable),
			{	//BinopExps
				val leftType = NewVariable[UnificationType]
				val rightType = NewVariable[UnificationType]
				for {
					op <- binopHelper(leftType, rightType,  ofType)
					left <- genExp(env, leftType)
					right <- genExp(env, rightType)
				} yield {???}	//am confuse
			}
			//still need to do unary exps too
		)
	}
}