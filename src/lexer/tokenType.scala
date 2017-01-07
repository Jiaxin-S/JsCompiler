package lexer

abstract class tokenType
case class numbers() extends tokenType
case class variable_names() extends tokenType
case class operators() extends tokenType
case class string() extends tokenType
case class keywords() extends tokenType
case class unknown() extends tokenType
case class end_of_file() extends tokenType
   
