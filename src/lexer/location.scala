package lexer

class location(var row: Int, var col: Int) {
  
 override def toString: String = "Location: row " + row + " col " + col
 
}