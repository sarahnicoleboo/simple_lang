package simple_lang_typechecker.typechecker

class IllTypedException(message: String) extends Exception(message)

object Typechecker {
	def typeOfExp(exp: Exp, env: Map[Variable, Type]): Type = {
		exp match {
			case IntegerLiteralExp(_) => IntType
			case BooleanLiteralExp(_) => BoolType
			case StringLiteralExp(_) => StringType
			case VariableExp(name) => {
				env.getOrElse(name, throw new IllTypedException(s"variable $name not in scope"))
			}
			case UnaryExp(op, theExp) => {
				(op, typeOfExp(theExp, env)) match {
					case (NotOp, BoolType) => BoolType
					case (NegateOp, IntType) => IntType
					case _ => throw new IllTypedException("Incompatible type with unary operator")
				}
			}
			 case BinopExp(left, op, right) => {
				val leftType = typeOfExp(left, env)
				val rightType = typeOfExp(right, env)
				(op, leftType, rightType) match {
					case (PlusOp, IntType, IntType) => IntType
					case (PlusOp, StringType, StringType) => StringType
					case (MinusOp, IntType, IntType) => IntType
					case (LessThanOp, IntType, IntType) => BoolType
					case (GreaterThanOp, IntType, IntType) => BoolType
					case (ExactlyEqualsOp, leftType, rightType) if leftType == rightType => BoolType
					case (NotEqualsOp, leftType, rightType) if leftType == rightType => BoolType
					case _ => throw new IllTypedException("Incompatible types with binary operator")
				}
			 }
			 case _ => throw new IllTypedException("Reached the end of typeOfExp without matching")
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
		}
	}
	
	def typecheckProgram(stmts: Seq[Stmt], env: Map[Variable, Type]) = {
		for (stmt <- stmts) typecheckStmt(stmt, env)
	}
} //end of Typechecker

object Generator {
	import simple_lang_typechecker.unification._
	import simple_lang_typechecker.unification.UIterator._
	
	//trait UnificationType {}	//moved to syntax.scala
	val intUnificationType = VariantOf[UnificationType]("intType")
	val boolUnificationType = VariantOf[UnificationType]("boolType")
	val stringUnificationType = VariantOf[UnificationType]("stringType")
	
	//helper for generating expressions with unary operators
	//each tuple: Op, expType, resultType
	val unops: Seq[(UnaryOp, Term[UnificationType], Term[UnificationType])] =
		Seq (
			(NotOp, boolUnificationType, boolUnificationType),
			(NegateOp, intUnificationType, intUnificationType)
		)
	 
	def unopHelper(expType: Term[UnificationType], resultType: Term[UnificationType]): UIterator[Int, UnaryOp] = {
		for {
			(op, expectedExpType, expectedResult) <- toUIterator(unops.iterator)
			_ <- unify(expType, expectedExpType)
			_ <- unify(resultType, expectedResult)
		} yield op
	}
	
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
		
	def binopHelper(leftType: Term[UnificationType], rightType: Term[UnificationType], resultType: Term[UnificationType]): UIterator[Int, BinOp] = {
		for {
			(op, expectedLeft, expectedRight, expectedResult) <- toUIterator(binops.iterator)
			_ <- unify(leftType, expectedLeft)
			_ <- unify(rightType, expectedRight)
			_ <- unify(resultType, expectedResult)
		} yield op
	}
	
	//pre-defined functions
	//each tuple is: (returnType, name, listOfParams)
	val functions: Seq[FunctionDef] =
		Seq(
			FunctionDef(intUnificationType, Variable("sum"), Seq((intUnificationType, Variable("x")), (intUnificationType, Variable("y")))),
			FunctionDef(boolUnificationType, Variable("isTrue"), Seq((boolUnificationType, Variable("a")))),
			FunctionDef(stringUnificationType, Variable("concat"), Seq((stringUnificationType, Variable("a")), (stringUnificationType, Variable("b"))))
		)
	
	//helper for generating function calls
	def functionParamHelper(params: Seq[(Term[UnificationType], Variable)], env: GenTypeEnv): UIterator[Int, Seq[Exp]] = {
		val paramTypes = params.map(_._1)
		val exps = List[Exp]()
		if (params.length == 0) {
			singleton(exps)
		} else {
			for (p <- paramTypes) {
				genExp(env, p) :: exps
			}
			singleton(exps.reverse)
		}
	}
	
	type GenTypeEnv = Map[Variable, Term[UnificationType]]	//alias so i don't have to keep typing this
	
	def genExp(env: GenTypeEnv, ofType: Term[UnificationType]): UIterator[Int, Exp] = {
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
			{	//UnaryExps
				val expType = NewVariable[UnificationType]
				for {
					op <- unopHelper(expType, ofType)
					theExp <- genExp(env, expType)
				} yield UnaryExp(op, theExp)
			},
			{	//BinopExps
				val leftType = NewVariable[UnificationType]
				val rightType = NewVariable[UnificationType]
				for {
					op <- binopHelper(leftType, rightType,  ofType)
					left <- genExp(env, leftType)
					right <- genExp(env, rightType)
				} yield BinopExp(left, op, right)
			},
			for {
				FunctionDef(returnType, name, params) <- toUIterator(functions.iterator)
				_ <- unify(returnType, ofType)	
				exps <- functionParamHelper(params, env)
			} yield FunctionCall(name, exps)
		)	//end of disjuncts
	} //end of genExp
	
	//helper for generating block statement
	def genNumStmtsInBlock(amount: Int, env: GenTypeEnv): UIterator[Int, (List[GenStmt], GenTypeEnv)] = {
		if (amount == 0) {
			singleton((List(), env))
		} else {
			for {
				(stmt, newEnv) <- genStmt(env)
				(stmts, finalEnv) <- genNumStmtsInBlock(amount - 1, newEnv)
			} yield (stmt :: stmts, finalEnv)
		}
	}
	
	//helper for generating print stmt
	def genNumExpsInPrint(amount: Int, env: GenTypeEnv): UIterator[Int, List[Exp]] = {
		if (amount == 0) {
			singleton(List())
		} else {
			val expType = NewVariable[UnificationType]
			for {
				exp <- genExp(env, expType)
				exps <- genNumExpsInPrint(amount - 1, env)
			} yield (exp :: exps)
		}
	}
	
	def genStmt(env: GenTypeEnv): UIterator[Int, (GenStmt, GenTypeEnv)] = {
		disjuncts(
			{	//Variable Declaration: type x = exp;
				val expType = NewVariable[UnificationType]
				for {
					exp <- genExp(env, expType)
					id <- getState
					_ <- putState(id + 1)
				} yield {
					val newVariable = Variable("x" + id)
					//??? below is because of the whole not a Type but a Term[UnificationType] thing
					//to be fixed later
					//(VariableDeclarationStmt(???, newVariable, exp), env + (newVariable -> expType))
					(VariableDeclarationGenStmt(expType, newVariable, exp), env + (newVariable -> expType))
				}
			},
			//Variable Assignment
			for {
				(variable, variableType) <- toUIterator(env.iterator)
				exp <- genExp(env, variableType)
			} yield (VariableAssignmentGenStmt(variable, exp), env),
			//If statement: if (exp) stmt else stmt
			for {
				guard <- genExp(env, boolUnificationType)
				(ifTrue, _) <- genStmt(env)
				(ifFalse, _) <- genStmt(env)
			} yield (IfGenStmt(guard, ifTrue, ifFalse), env),
			//While statement: while (exp) stmt
			for {
				guard <- genExp(env, boolUnificationType)
				(body, _) <- genStmt(env)
			} yield (WhileGenStmt(guard, body), env),
			for {	//print(exp*)
				numExps <- toUIterator(0.to(5).iterator)
				exps <- genNumExpsInPrint(numExps, env)
			} yield (PrintGenStmt(exps.toSeq), env),
			for {	//Block statement: { stmt* }
				numStmts <- toUIterator(0.to(5).iterator)
				(stmts, _) <- genNumStmtsInBlock(numStmts, env)
			} yield (BlockGenStmt(stmts.toSeq), env)
		) //end of disjuncts
	}
} //end of Generator