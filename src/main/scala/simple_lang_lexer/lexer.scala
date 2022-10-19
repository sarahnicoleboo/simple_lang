package simple_lang_lexer

object lexer {
	class tokenizer(val input: String)
	
	def tokenize(input: String) = {
		tokenizer(input)
	}
	
}