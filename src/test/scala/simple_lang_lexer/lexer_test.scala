package simple_lang_lexer

import org.scalatest.funsuite.AnyFunSuite

class lexer_test extends AnyFunSuite {
	import lexer._
	test("simple test") {
		val input = "hi there"
		val result = tokenize(input)
		assert(result.input == "hithere")
	}
}