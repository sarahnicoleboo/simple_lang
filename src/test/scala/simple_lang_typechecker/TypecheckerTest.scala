import org.scalatest._
import simple_lang_typechecker.typechecker._

class TypecheckerTest extends FlatSpec {
	val emptyEnv = Map[Variable, Type]()
	
	"typeOfExp" should "handle an integer literal" in {
		assertResult(IntType) { typeOfExp(IntegerLiteralExp(10), emptyEnv) } 
	}
}