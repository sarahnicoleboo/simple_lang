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
	def functionType(param: Term[UnificationType], returnType: Term[UnificationType]) =
		VariantOf[UnificationType]("functionType", param, returnType)
		
	def makeString(size: Int): String = {
		size match {
			case 0 => ""
			case _ => util.Random.nextPrintableChar.toString ++ makeString(size-1).toString
		}
	}
	
	def makeUIString(size: Int) : UIterator[Int, String] = {
		singleton(makeString(size))
	}
	
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
 	def functionParamHelper(paramTypes: List[Term[UnificationType]], env: GenTypeEnv, bound: Int): UIterator[Int, List[Exp]] = {
		paramTypes match {
			case Nil => singleton(List())
			case head :: tail => {
				for {
					exp <- genExp(env, head, bound - 1)
					rest <- functionParamHelper(tail, env, bound)
				} yield exp :: rest
			}
		}
	}
	
	
	type GenTypeEnv = Map[Variable, Term[UnificationType]]	//alias so i don't have to keep typing this
	
	def genExp(env: GenTypeEnv, ofType: Term[UnificationType], bound: Int): UIterator[Int, Exp] = {
		if (bound == 0) {
			empty
		} else {
			disjuncts(
				for {	//integer literal
					_ <- unify(ofType, intUnificationType)
					num <- toUIterator(0.to(2).toIterator)
				} yield IntegerLiteralExp(num),
				for {	//boolean literal
					_ <- unify(ofType, boolUnificationType)
					values = Seq(true, false)
					value <- toUIterator(values.toIterator)
				} yield BooleanLiteralExp(value),
				for {	//string literal
					_ <- unify(ofType, stringUnificationType)
					size <- toUIterator(0.to(5).toIterator)
					resultString <- makeUIString(size)
				} yield StringLiteralExp(resultString),
				for {	//variable
					(variable, variableType) <- toUIterator(env.iterator)
					_ <- unify(ofType, variableType)
				} yield VariableExp(variable),
				{	//UnaryExps
					val expType = NewVariable[UnificationType]
					for {
						op <- unopHelper(expType, ofType)
						theExp <- genExp(env, expType, bound - 1)
					} yield UnaryExp(op, theExp)
				},
				{	//BinopExps
					val leftType = NewVariable[UnificationType]
					val rightType = NewVariable[UnificationType]
					for {
						op <- binopHelper(leftType, rightType,  ofType)
						left <- genExp(env, leftType, bound - 1)
						right <- genExp(env, rightType, bound - 1)
					} yield BinopExp(left, op, right)
				},
				for {	//NamedFunctionCalls
					FunctionDef(returnType, name, params) <- toUIterator(functions.iterator)
					_ <- unify(returnType, ofType)	
					exps <- functionParamHelper(params.map(_._1).toList, env, bound)
				} yield FunctionCall(name, exps),
				{	//HOF calls
					val paramType = NewVariable[UnificationType]
					for {
						outerExp <- genExp(env, functionType(paramType, ofType), bound - 1)
						innerExp <- genExp(env, paramType, bound - 1)
					} yield CallExp(outerExp, innerExp)
				},
				{	//HOF introduction
					val paramType = NewVariable[UnificationType]
					val returnType = NewVariable[UnificationType]
					for {
						_ <- unify(ofType, functionType(paramType, returnType))
						id <- getState
						_ <- putState(id + 1)
						variable = Variable("x" + id)
						exp <- genExp(env + (variable -> paramType), ofType, bound - 1)
					} yield FunctionExp(variable, exp)
				}
			)	//end of disjuncts
		}
	} //end of genExp
	
	//helper for generating block statement
	def genNumStmtsInBlock(amount: Int, env: GenTypeEnv, bound: Int): UIterator[Int, (List[GenStmt], GenTypeEnv)] = {
		if (amount == 0) {
			singleton((List(), env))
		} else {
			for {
				(stmt, newEnv) <- genStmt(env, bound - 1)
				(stmts, finalEnv) <- genNumStmtsInBlock(amount - 1, newEnv, bound)
			} yield (stmt :: stmts, finalEnv)
		}
	}
	
	//helper for generating print stmt
	def genNumExpsInPrint(amount: Int, env: GenTypeEnv, bound: Int): UIterator[Int, List[Exp]] = {
		if (amount == 0) {
			singleton(List())
		} else {
			val expType = NewVariable[UnificationType]
			for {
				exp <- genExp(env, expType, bound -  1)
				exps <- genNumExpsInPrint(amount - 1, env, bound)
			} yield (exp :: exps)
		}
	}
	
	def genStmt(env: GenTypeEnv, bound: Int): UIterator[Int, (GenStmt, GenTypeEnv)] = {
		if (bound == 0) {
			empty
		} else {
			disjuncts(
				{	//Variable Declaration: type x = exp;
					val expType = NewVariable[UnificationType]
					for {
						exp <- genExp(env, expType, bound - 1)
						id <- getState
						_ <- putState(id + 1)
					} yield {
						val newVariable = Variable("x" + id)
						(VariableDeclarationGenStmt(expType, newVariable, exp), env + (newVariable -> expType))
					}
				},
				//Variable Assignment
				for {
					(variable, variableType) <- toUIterator(env.iterator)
					exp <- genExp(env, variableType, bound - 1)
				} yield (VariableAssignmentGenStmt(variable, exp), env),
				//If statement: if (exp) stmt else stmt
				for {
					guard <- genExp(env, boolUnificationType, bound - 1)
					(ifTrue, _) <- genStmt(env, bound - 1)
					(ifFalse, _) <- genStmt(env, bound - 1)
				} yield (IfGenStmt(guard, ifTrue, ifFalse), env),
				//While statement: while (exp) stmt
				for {
					guard <- genExp(env, boolUnificationType, bound - 1)
					(body, _) <- genStmt(env, bound - 1)
				} yield (WhileGenStmt(guard, body), env),
				for {	//print(exp*)
					numExps <- toUIterator(0.to(2).iterator)
					exps <- genNumExpsInPrint(numExps, env, bound)
				} yield (PrintGenStmt(exps.toSeq), env),
				for {	//Block statement: { stmt* }
					numStmts <- toUIterator(0.to(2).iterator)
					(stmts, _) <- genNumStmtsInBlock(numStmts, env, bound)
				} yield (BlockGenStmt(stmts.toSeq), env)
			) //end of disjuncts
		}
	}
	
/* 	def cookType(rawType: Term[UnificationType]): UIterator[Int, Type] = {
		for {
			env <- get ??? no method
			lookedup <- env.fullLookup(rawType)
			cookedType <- lookedup match {
				case StructureTerm("intType", Seq()) => singleton(IntType)	//bcuz this needs to be Uiterator?
				case StructureTerm("boolType", Seq()) => singleton(BoolType)
				case StructureTerm("stringType", Seq()) => singleton(StringType)
				//case for FunctionType
			}
		} yield cookedType
	}
	
	def ensureStmtsInBlockCooked(stmts: List[GenStmt]): UIterator[Int, List[Stmt]] = {
		stmts match {
			case Nil => singleton(List())
			case head :: tail => {
				for {
					stmt <- cookStmt(head)
					rest <- ensureStmtsInBlockCooked(tail)
				} yield stmt :: rest
			}
		}
	}
	
	def cookStmt(stmt: GenStmt): UIterator[Int, Stmt] = {
		stmt match {
			case VariableDeclarationGenStmt(theType, name, exp) => {
				for {
					cookedType <- cookType(theType)
				} yield VariableAssignmentStmt(cookedType, name, exp)
			}
			case VariableAssignmentGenStmt(name, exp) => {
				singleton(VariableAssignmentStmt(name, exp)
			}
			case IfGenStmt(guard, ifTrue, ifFalse) => {
				for {
					cookedIfTrue <- cookStmt(ifTrue)
					cookedIfFalse <- cookStmt(ifFalse)
				} yield IfStmt(guard, cookedIfTrue, cookedIfFalse)
			}
			case WhileGenStmt(guard, body) => {
				for {
					cookedBody <- cookStmt(body)
				} yield WhileStmt(guard, cookedBody)
			}
			case PrintGenStmt(exps) => {
				singleton(PrintStmt(exps))
			}
			case BlockGenStmt(stmts) => {
				for {
					cookedStmts <- ensureStmtsInBlockCooked(stmts.toList)
				} yield BlockStmt(cookedStmts.toSeq)
			}
		}
	}
	
	def cookStmts(stmts: Seq[GenStmt]): UIterator[Int, Seq[Stmt]] = {
		UIterator.map(stmts)(cookStmt)
	} */
	
	def main(args: Array[String]) {
		genStmt(Map(), 2).reify(new UnificationEnvironment, 1).foreach(x => println(x)) //from KD 			//all three thing
		//genStmt(Map(), 2).reify(new UnificationEnvironment, 1).map(_._3).foreach(x => println(x))				// second and third
		//genStmt(Map(), 3).reify(new UnificationEnvironment, 1).map(_._3).map(_._1).foreach(x => println(x))	//just the AST
		//genExp(Map(), NewVariable[UnificationType], 3).reify(new UnificationEnvironment, 1).map(_._3).foreach(x=>println(x))
		
/* 		val result: Seq[GenStmt] = genStmt(Map(), 2).reify(new UnificationEnvironment, 1).map(_._3).map(_._1).toSeq
		cookStmts(result).foreach(x => println(x)) */
	}
} //end of Generator