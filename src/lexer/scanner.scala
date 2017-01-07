package lexer
import scala.util.matching.Regex
import util.control.Breaks._

// feed the lexer with the program string, output will be the tokens

class scanner(var p_string: String) {
  
  var token_buffer = ""
  var curr_index = 0
  var curr_state = 0
  var code_location = new location(1, 1)
  var token_location = new location(1, 1)
  
  def get_next_char : Char = {
    token_buffer = token_buffer + p_string.charAt(curr_index)
    var new_val = p_string.charAt(curr_index)
    
    if ( new_val == '\n') {
      code_location.row = code_location.row + 1
      code_location.col = 1
    }
    else {
      code_location.col = code_location.col + 1
    }
    curr_index = curr_index + 1
    return new_val
  }
  
  def mark_start(curr_char: Char) : Unit = {
    token_buffer = "" + curr_char  
    token_location.row = code_location.row
    token_location.col = code_location.col
  }
  
  def error_msg(curr_state: Int) : Unit = {
    System.out.println("error occurs in state " + curr_state + "!");
    System.exit(1)
  }
  
  def emit(token_type: tokenType) : token = {
    var new_token = new token(token_type, token_buffer, token_location.row, token_location.col)
    return new_token
  }
  
  def check_state(curr_state: Int, curr_char: Option[Char]) : Option[token]  = {
    curr_state match {
      case 0 => state_zero(curr_char)
      case 1 => state_one(curr_char)
      case 2 => state_two(curr_char)
      case 3 => state_three(curr_char)
      case 4 => state_four(curr_char)
      case 5 => state_five(curr_char)
      case 6 => state_six(curr_char)
      case 7 => state_seven(curr_char)
      case 8 => state_eight(curr_char)
      case 9 => state_nine(curr_char)
      case 10 => state_ten(curr_char)
      case 11 => state_eleven(curr_char)
      case 12 => state_twelve(curr_char)
    }
  }
  
  abstract class Type;
  case class Zero() extends Type;
  case class NoneZeroNums(n1: Int) extends Type;
  case class WhiteSpace() extends Type;
  case class Return() extends Type;
  case class EndOfFile() extends Type;
  case class LeftBrace() extends Type;
  case class SingleQuote() extends Type;
  case class RightBrace() extends Type;
  case class SpecialChar(s1: Char) extends Type;
  case class Letters(l1: Char) extends Type;
  case class Slash() extends Type;
  case class NewLine() extends Type;
  case class NoneOperator(m1: Char) extends Type;
  case class Unknown(u1: Char) extends Type;
  
  def sort_type(temp: Option[Char]) : Type = {
    var curr_char : Char = '0'
    temp match {
        case Some(value) => curr_char = value
        case None => return EndOfFile()
    }
    
    if (curr_char == '0') {
      return Zero()
    }
    else if (curr_char.toString().matches("[1-9]")) {
       return NoneZeroNums(curr_char.toInt)
    }
    else if (curr_char == '\t' || curr_char == ' ') {
      return WhiteSpace()
    }
    else if (curr_char == '\n') {
      return NewLine()
    }
    else if (curr_char == '\r') {
      return WhiteSpace()
      // return Return()
    }
    else if (curr_char == '{') {
      return LeftBrace()
    }
    else if (curr_char == '\'') {
      return SingleQuote()
    }
    else if (curr_char == '}') {
      return RightBrace()
    }
    else if (curr_char.toString().matches("[\\~\\*\\_\\+\\-\\(\\)\\;\\,\\.\\&\\=\\<\\>]")) {
      return SpecialChar(curr_char)
    }
    else if (curr_char.toString().matches("[!@#$%^]")) {
      return NoneOperator(curr_char)
    }
    else if (curr_char.toString().matches("[a-zA-Z]")) {
      return Letters(curr_char)
    }
    else if (curr_char == '/') {
      return Slash()
    }
    else {
      return EndOfFile()
    }
    // TO-DO: Need to handle unknown type as well
  }
  
  def sort_token_type(token_buffer: String) : tokenType = {
    
    if (token_buffer.startsWith("\'") ){
      return string()
    }
    else if (token_buffer.matches("[0-9]+")) {
      return numbers()
    }
    else if (token_buffer.matches("program|function|begin|end|if|then|else|while|do|cout|cin|endl|or|and|div|mod|int|boolean|true|false")) {
      return keywords()
    }
    else if (token_buffer.matches("[a-zA-Z][a-zA-Z0-9]*")) {
      return variable_names()
    }
    else if (token_buffer.matches("\\+|-|\\*|\\(|\\)|;|,|\\.|&|~|==|=|<|<=|>|>=|<>|<<|>>")) {
      return operators()
    }
    else {
      return unknown()
    }
  }
  
  def state_zero(temp: Option[Char]) : Option[token] = {
    var curr_char : Char = '0'
    temp match {
        case Some(value) => curr_char = value
        case None => var new_token = emit(end_of_file()) 
                    curr_index = curr_index + 1
                    return Some(new_token)
    }
    
    var char_type = sort_type(temp)
    char_type match {
      case Zero() => mark_start(curr_char)
                     code_location.col = code_location.col + 1 //you duplicate these two lines an aweful lot...
                     curr_index = curr_index + 1
                     curr_state = 1
                     return None
      case NoneZeroNums(curr_char) => mark_start(curr_char.toChar)
                     code_location.col = code_location.col + 1
                     curr_index = curr_index + 1
                     curr_state = 2
                     return None
      case Letters(curr_char) => mark_start(curr_char)
                     code_location.col = code_location.col + 1
                     curr_index = curr_index + 1
                     curr_state = 3
                     return None
      case LeftBrace() => curr_index = curr_index + 1
                          code_location.col = code_location.col + 1
                          curr_state = 4
                          return None
      case Slash() => curr_index = curr_index + 1
                      code_location.col = code_location.col + 1
                      curr_state = 5
                      return None
      case Return() => curr_index = curr_index + 1
                       code_location.row = code_location.row + 1
                       code_location.col = 1
                       curr_state = 0
                       return None
      case NewLine() => curr_index = curr_index + 1
                       code_location.row = code_location.row + 1
                       code_location.col = 1
                       curr_state = 0
                       return None
      case WhiteSpace() => curr_index = curr_index + 1
                       code_location.col = code_location.col + 1
                       curr_state = 0
                       return None
      case SingleQuote() => mark_start(curr_char)
                            code_location.col = code_location.col + 1
                            curr_index = curr_index + 1
                            curr_state = 7
                            return None
      case SpecialChar('=') => mark_start(curr_char)
                            code_location.col = code_location.col + 1
                            curr_index = curr_index + 1
                            curr_state = 10
                            return None
      case SpecialChar('<') => mark_start(curr_char)
                            code_location.col = code_location.col + 1
                            curr_index = curr_index + 1
                            curr_state = 11
                            return None
      case SpecialChar('>') => mark_start(curr_char)
                            code_location.col = code_location.col + 1
                            curr_index = curr_index + 1
                            curr_state = 12
                            return None
      case SpecialChar(others) => mark_start(others) 
                            code_location.col = code_location.col + 1
                            curr_index = curr_index + 1
                            curr_state = 9
                            return None
    }
  }
  
  def state_one(curr_char: Option[Char]) : Option[token] = {
    var new_token = emit(sort_token_type(token_buffer)) 
    curr_state = 0
    return Some(new_token)
  }
  
  def state_two(curr_char: Option[Char]) : Option[token] = {
    var char_type = sort_type(curr_char)
    char_type match {
      case Zero() => get_next_char
                     curr_state = 2
                     return None
      case NoneZeroNums(curr_char) => get_next_char
                     curr_state = 2   
                     return None
      case default => var new_token = emit(sort_token_type(token_buffer))
                     curr_state = 0
                     return Some(new_token)
    }
  }
  
  def state_three(curr_char: Option[Char]) : Option[token] = {
    var char_type = sort_type(curr_char)
    char_type match {
      case Zero() => get_next_char
                     curr_state = 3
                     return None
      case NoneZeroNums(curr_char) => get_next_char
                     curr_state = 3
                     return None
      case Letters(curr_char) => get_next_char
                     curr_state = 3
                     return None
      case default => var new_token = emit(sort_token_type(token_buffer))
                     curr_state = 0
                     return Some(new_token)
    }
  }
  
  def state_four(curr_char: Option[Char]) : Option[token] = {
    var char_type = sort_type(curr_char)
    char_type match {
            
      case RightBrace() => curr_index = curr_index + 1
                     code_location.col = code_location.col + 1
                     curr_state = 0
                     return None
      case NewLine() => curr_index = curr_index + 1             
                     code_location.row = code_location.row + 1
                     code_location.col = 1
                     curr_state = 4 
                     return None
      case Return() => curr_index = curr_index + 1             
                     code_location.row = code_location.row + 1
                     code_location.col = 1
                     curr_state = 4 
                     return None               
      case EndOfFile() => error_msg(curr_state)
                     return None
      case default => curr_index = curr_index + 1
                     code_location.col = code_location.col + 1
                     curr_state = 4
                     return None
    }
  }
  
  def state_five(curr_char: Option[Char]) : Option[token] = {        
    var char_type = sort_type(curr_char)
    char_type match {
      case Slash() => curr_index = curr_index + 1
                     code_location.col = code_location.col + 1
                     curr_state = 6
                     return None
      case default => error_msg(curr_state)
                     return None
    }
  }
  
  def state_six(curr_char: Option[Char]) : Option[token] = {
    var char_type = sort_type(curr_char)
    char_type match {
      
      case Return() => curr_index = curr_index + 1
                     code_location.row = code_location.row + 1
                     code_location.col = 1
                     curr_state = 0
                     return None
      case NewLine() => curr_index = curr_index + 1
                     code_location.row = code_location.row + 1
                     code_location.col = 1
                     curr_state = 0
                     return None
      case EndOfFile() =>  var new_token = emit(end_of_file())
                     curr_index = curr_index + 1
                     curr_state = 0
                     return Some(new_token)
      case default => curr_index = curr_index + 1
                     code_location.col = code_location.col + 1
                     curr_state = 6
                     return None
    }
  }
  
  def state_seven(curr_char: Option[Char]) : Option[token] = {
    var char_type = sort_type(curr_char)
    char_type match {
      
      case SingleQuote() => get_next_char
                     curr_state = 8
                     return None
      case EndOfFile() => error_msg(curr_state)
                     return None
      case Return() => error_msg(curr_state)
                     return None
      case NewLine() => error_msg(curr_state)
                     return None
      case default => get_next_char
                     curr_state = 7
                     return None
    }
  }
  
  def state_eight(curr_char: Option[Char]) : Option[token] = {
    var new_token = emit(sort_token_type(token_buffer))
    curr_state = 0
    return Some(new_token)
  }
  
  def state_nine(curr_char: Option[Char]) : Option[token] = {
    var new_token = emit(sort_token_type(token_buffer)) 
    curr_state = 0
    return Some(new_token)
  }
  
  def state_ten(curr_char: Option[Char]) : Option[token] = {
    var char_type = sort_type(curr_char)
    char_type match {
      case SpecialChar('=') => get_next_char
                     curr_state = 9
                     return None
      case default => var new_token = emit(sort_token_type(token_buffer)) 
                     curr_state = 0
                     return Some(new_token)
    }
  }
  
  def state_eleven(curr_char: Option[Char]) : Option[token] = {
    var char_type = sort_type(curr_char)
    char_type match {
      case SpecialChar('=') => get_next_char
                     curr_state = 9
                     return None
      case SpecialChar('<') => get_next_char
                     curr_state = 9
                     return None
      case SpecialChar('>') => get_next_char
                     curr_state = 9
                     return None
      case default => var new_token = emit(sort_token_type(token_buffer)) 
                     curr_state = 0
                     return Some(new_token)
    }
  }
  
  def state_twelve(curr_char: Option[Char]) : Option[token] = {
    var char_type = sort_type(curr_char)
    char_type match {
      case SpecialChar('=') => get_next_char
                     curr_state = 9
                     return None
      case SpecialChar('>') => get_next_char
                     curr_state = 9
                     return None
      case default => var new_token = emit(sort_token_type(token_buffer)) 
                     curr_state = 0
                     return Some(new_token)
    }
  }
  
  var check_end = false;
  def loop_through() : Option[token] = {
    var result : Option[token] = None
    
    breakable {
      while (check_end != true) {
        if (curr_index == (p_string.length())) {
          check_end = true
          break
        }
        var curr_char = p_string.charAt(curr_index)
        result = check_state(curr_state, Some(curr_char))
        if (result != None) { 
          return result
        }
      }
    }
   
    if (curr_index == p_string.length()) {
      result = check_state(curr_state, None)
      System.out.println("after while")
      if (result != None) {
        return result
      }
    }
    
    return None
  }
}