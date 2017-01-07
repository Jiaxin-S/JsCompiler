package lexer

object main extends App{
  override def main(args:Array[String]) = {
    // get input from text file
    val source = scala.io.Source.fromFile("demo2.yasl") // need to update file name
    val lines = try source.mkString finally source.close()
    System.out.println(lines)
    
    var sc = new scanner(lines)
    System.out.println("line num: " + lines.length())
    var i = 0 
    while( i < lines.length()) {
      System.out.println("i value: " + i)
      var curr_token = sc.loop_through()
      
      if (curr_token == None) {
        System.exit(1)
      }
      
      System.out.println(curr_token.get) 
      System.out.println(curr_token.get.value.length())
      i = i + curr_token.get.value.length() + 1
      
      if ( i == lines.length() ) {
        i = i - 1
      }
    }
  
  }
}