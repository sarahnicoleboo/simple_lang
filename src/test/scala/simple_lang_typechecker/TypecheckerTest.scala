import org.scalatest._
import simple_lang_typechecker.typechecker._

class TypecheckerTest extends FlatSpec {

 	"testThis" should "1+1" in {
		testThis(1) should be (2)
	}
}