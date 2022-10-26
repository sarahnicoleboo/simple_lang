import org.scalatest._
import simple_lang_typechecker.typechecker._

class TypecheckerTest extends FlatSpec {
	val emptyEnv = Map[Variable, Type]()
	
	"typeOfExp" should "handle an integer literal" in {
		assertResult(IntType) { Typechecker.typeOfExp(IntegerLiteralExp(10), emptyEnv) } 
	}
	
	"typeOfExp" should "handle a boolean literal" in {
		assertResult(BoolType) { Typechecker.typeOfExp(BooleanLiteralExp(true), emptyEnv) }
	}
}