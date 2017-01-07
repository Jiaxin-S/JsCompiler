package lexer

class token(val token_type: tokenType, val value: String, val row: Int, val col: Int) {
  
  override def toString: String = "token_type: " + token_type + "; value: " + value + "; in row " + row + " col " + col

}