package grammarClasses

class ExpressionParserEnhanced(input: String) {
  private var index:Int = 0
  def ParseS: S = {
    var SRetNull: S = null
    if (input(index) == '-' || input(index) == '(' || input(index) == 'S' || "[a-z]".r.matches(input(index).toString) || "[0-9]".r.matches(input(index).toString)) {
      //first set match passes, do not index past anything yet though
      val returnVal = ParseE
      if(index != input.length && input(index) != '\n'){
        println("ERROR expected end of input at "+index+ " instead got "+input(index))
        return null
      }
      return returnVal
    }
    //check fails, tell the user what the error was
    println("ERROR on line " + index + ": expected alphabetic letter, constant '-', '(', instead got "+input(index))
    SRetNull
  }

  def ParseE: E ={
    val ERetNull: E = null
    if (input(index) == '-' || input(index) == '(' || input(index) == 'S' || "[a-z]".r.matches(input(index).toString) || "[0-9]".r.matches(input(index).toString)) {
      //first set match passes, do not index past anything yet though
      return E(ParseT, ParseETail)
    }
    println("ERROR on line " + index + ": expected alphabetic letter, constant '-', '(', instead got "+input(index))
    ERetNull
  }

  def ParseETail:Option[Either[E2, E3]] ={
    if (index > input.length-1 || input(index) == '\n') {
      return None
    }
    if(input(index)=='+'){
      index+=1
      skipWhiteSpace()
      Some(Left(E2(ParseE)))
    }else if(input(index)=='-'){
      index+=1
      skipWhiteSpace()
      Some(Right(E3(ParseE)))
    }else if(input(index) == ')' || input(index) == ']'){
      None
    }else{
      //if we are not at the very end of the input or the current character isn't plus or minus, we have a syntax error
      println("ERROR: expected +, -, or end of input at index " + index + " instead found " + input(index))
      null
    }
  }

  def ParseT: T ={
    val TRetNull: T = null
    if (input(index) == '-' || input(index) == '(' || input(index) == 'S' || "[a-z]".r.matches(input(index).toString) || "[0-9]".r.matches(input(index).toString)) {
      return T(ParseF, ParseTTail)
    }
    //check fails, tell the user what the error was
    println("ERROR on line " + index + ": expected alphabetic letter, constant, '-', '(', instead got "+input(index))
    TRetNull
  }

  def ParseTTail: Option[TE] = {

    if(index>input.length-1 || input(index) == '\n'){
      return None
    }
    if (input(index) == '*') {
      index += 1
      skipWhiteSpace()
      Some(TE(ParseT, '*'))
    } else if (input(index) == '/') {
      index += 1
      skipWhiteSpace()
      Some(TE(ParseT, '/'))
    } else if(input(index) == '+' || input(index) == '-' || input(index) == ')'|| input(index) == ']'){
      None
    }else{
      println("ERROR: expected +, -, *, / or end of input at index " + index + " instead found " + input(index))
      null
    }
  }

  def ParseNonExponentF: F = {
    val FRetNull: F = null
    val ConstPattern = "^(\\-|(\\d(\\.))?)\\d+(\\.\\d+)?".r
    val VarPattern = "[abcdfghijklmnopqrstuvwxyz]".r
    val NumPattern = "[0-9]".r
    val SqrtPattern = "Sqrt\\[".r

    input(index).toString match{
      case "-" =>
        //we have a negative, then we know at this point that we are dealing with a negative number since we handled
        //every other negative case at the T node
        var currStr = input.substring(index)
        val constant = ConstPattern.findAllIn(currStr)
        if(constant.hasNext){
          currStr = constant.next()
          index += currStr.length
          skipWhiteSpace()
          Const(currStr.toDouble)
        }else if (input(index) == '-' && "[a-zS(]".r.matches(input(index+1).toString)){
          index += 1
          EP(T(Const(-1), Some(TE(T(ParseF, None), '*'))), None)
        }else{
          println("ERROR: Expected digit at index " + index + " instead found " + input(index))
          FRetNull
        }
      case VarPattern() =>
        var currStr = input.substring(index)
        val variable = VarPattern.findAllIn(currStr)
        if(variable.hasNext){
          currStr = variable.next()
          index+=currStr.length
          //check and see if the next two characters are "n["
          if(index+2<input.length && input.substring(index, index+2) == "n["){
            index += 2
            val returnVal = naturalLog(EP(ParseT, ParseETail))
            if (index == input.length || input(index) != ']') {
              println("ERROR: Expected ] at index " + index)
              return null
            }
            index += 1
            skipWhiteSpace()
            returnVal
          }else {
            skipWhiteSpace()
            Var(currStr)
          }
        }else{
          println("ERROR: Expected variable letter at index " + index + " instead found " + input(index))
          FRetNull
        }
      case "(" =>
        index += 1
        skipWhiteSpace()
        val returnedEP = EP(ParseT, ParseETail)
        if(index == input.length || input(index)!=')'){
          println("ERROR: Expected ) at index " + index)
          return null
        }
        index += 1
        returnedEP
      case NumPattern() =>
        var currStr = input.substring(index)
        val constant = ConstPattern.findAllIn(currStr)
        if (constant.hasNext) {
          currStr = constant.next()
          index += currStr.length
          skipWhiteSpace()
          Const(currStr.toDouble)
        } else {
          println("ERROR: Expected digit at index " + index + " instead found " + input(index))
          FRetNull
        }
      case "S" =>
        var currStr = input.substring(index)
        val Sqrt = SqrtPattern.findAllIn(currStr)
        if(Sqrt.hasNext){
          currStr = Sqrt.next()
          index += currStr.length
          skipWhiteSpace()
          val returnVal = FExp(EP(ParseT,ParseETail), EP(T(Const(1),Some(TE(T(Const(2), None),'/'))), None))
          if(index == input.length || input(index)!=']'){
            println("ERROR: Expected ] at index " + index)
            return null
          }
          index += 1
          skipWhiteSpace()
          returnVal
        }else{
          println("ERROR: Expected Sqrt at index " + index + " instead found " + input(index))
          FRetNull
        }

    }
  }

  def ParseF: F = {
    val leftSide = ParseNonExponentF
    if(index == input.length || input(index) != '^'){
      return leftSide
    }
    index+=1
    skipWhiteSpace()
    //if it's negative and NOT a number, then we multipliy the right value by -1
    //handle the negative case here
    var rightSide: F = null
    if (input(index) == '-' && "[a-zS(]".r.matches(input(index + 1).toString)) {
      index += 1
      rightSide = EP(T(Const(-1),Some(TE(T(ParseF,None), '*'))),None)
      FExp(leftSide, rightSide)
    } else {
      FExp(leftSide, ParseF)
    }
  }

  def skipWhiteSpace(): Unit = {
    if (index <= input.length - 1) {
      while (input(index) == ' ') {
        index +=1

      }
    }
  }

}
