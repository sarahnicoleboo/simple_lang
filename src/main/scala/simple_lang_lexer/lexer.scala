package simple_lang_lexer

object lexer {
	class tokenizer(val input: String)
	
	def tokenize(input: String) = {
		val newInput = input.filterNot(_.isWhitespace)
		tokenizer(newInput)
	}
	
	
}