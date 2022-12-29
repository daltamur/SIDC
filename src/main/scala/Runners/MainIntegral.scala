package Runners

import com.wolfram.jlink.{KernelLink, MathLinkException}
import grammarClasses.{E, ExpressionParserEnhanced, full_expression_parser}

import java.util.Scanner

object MainIntegral {
  //make our wolfram kernel link
  System.mapLibraryName("JLinkNativeLibrary")
  System.load("/usr/local/Wolfram/WolframEngine/13.1/SystemFiles/Links/JLink/SystemFiles/Libraries/Linux-x86-64/libJLinkNativeLibrary.so")
  val kernelLinkArgs: Array[String] = Array("-linkmode", "launch", "-linkname", "/usr/local/Wolfram/WolframEngine/13.1/Executables/math")
  var ml: KernelLink = null
  var subIsU: Boolean = true
  var curVar: String = ""
  try {
    ml = com.wolfram.jlink.MathLinkFactory.createKernelLink(kernelLinkArgs)
    ml.discardAnswer()
  } catch {
    case e: MathLinkException =>
      println("Fatal error opening link: " + e.getMessage)
  }

  val scanner: Scanner = new Scanner(System.in)
  scanner.useDelimiter("")

  println("Ignore the previous error, everything loaded successfully")

  def integrateExpandedFunction(exprVal: String): Unit = {
    try {

      var strResult = ""
      try {
        strResult = ml.evaluateToInputForm("Expand[" + exprVal + "]", 0) + "\n"
        if (strResult == "$Failed\n") {
          //just to get what the parse error is
          val expr = new ExpressionParserEnhanced(exprVal)
          expr.ParseS
        }

      } catch {
        case e: MathLinkException => println(e.getMessage)
      }

      val expr = new ExpressionParserEnhanced(strResult)
      val x = expr.ParseS
      curVar = expr.curVar
      println(x)
      println(x.getString)
      x.asInstanceOf[E].compute()
      try {
        strResult = ml.evaluateToOutputForm("Simplify[" + x.getIntegrationVal + "]", 0)
        System.out.println(strResult)
      } catch {
        case e: MathLinkException => println(e.getMessage)
      }

      println("Done computing")
    }
    catch {
      case e: Exception =>
        println("Something went wrong: " + e.getMessage + e.printStackTrace())
    }


  }

  def main(args: Array[String]): Unit = {
    //grammarClasses.E->grammarClasses.T [grammarClasses.E2]|grammarClasses.T [grammarClasses.E3]
    //grammarClasses.E2-> '+' grammarClasses.E
    //grammarClasses.E3-> '-' grammarClasses.E
    //grammarClasses.T->grammarClasses.F [grammarClasses.TE]
    //grammarClasses.TE-> '*' grammarClasses.T| '/' grammarClasses.T
    //grammarClasses.F->'('grammarClasses.E')'|var|const|grammarClasses.FExp|Sin(grammarClasses.E)
    //grammarClasses.FExp -> grammarClasses.F'^'grammarClasses.F
    //we're gonna use case classes just b/c they include the to-string method from the get-go
    //^\-?[0-9]+(\.[0-9]+)?|^\-?[0-9]+(\.[0-9]+)? (potential regex for negative numbers)
    //val expr = new grammarClasses.full_expression_parser("x+(92*x^(5.97264*5^(x*5^(x+9)))/2)/-54*(2*-x)/54+7")
    print("Expression? ")
    var exprVal = scanner.next()
    if (exprVal == "quit") {
      ml.close()
      System.exit(0)
    }
    var curCharacter = ""
    while (curCharacter != "\n") {
      curCharacter = scanner.next()
      exprVal = exprVal + curCharacter
    }


    try {

      var strResult = ""
      try {
        strResult = ml.evaluateToInputForm("Simplify[" + exprVal + "]", 0) + "\n"
        println(strResult)
        if (strResult == "$Failed\n") {
          //just to get what the parse error is
          val expr = new ExpressionParserEnhanced(exprVal)//full_expression_parser(exprVal)
          expr.ParseS
          if(!expr.error.isBlank){
            println(expr.error)
          }
          MainIntegral.main(args)
          return
        }



      } catch {
        case e: MathLinkException => println(e.getMessage)
      }

      //println(strResult)
      val expr = new ExpressionParserEnhanced(strResult)//full_expression_parser(strResult)
      val x = expr.ParseE
      if(!expr.error.isBlank){
        println(expr.error)
        MainIntegral.main(args)
        return
      }

      if(expr.curVar.equals("u") || expr.curVar.equals("v")){
        println("ERROR: Variable letter " + expr.curVar + " is protected. Use variable letters that are not u or v.")
        MainIntegral.main(args)
        return
      }
      println(x)
      x.compute()
      println(x.getIntegrationVal)
      try {
        strResult = ml.evaluateToOutputForm("Simplify[" + x.getIntegrationVal + "]", 0)
        if (strResult.contains("null")) {
          println("Couldn't Integrate Simplified Expression! Expanding...")
          integrateExpandedFunction(exprVal)
        } else {
          println(strResult)
        }
      } catch {
        case e: MathLinkException => println(e.getMessage)
      }

      println("Done computing")
      MainIntegral.main(args)
    }
    catch {
      case e: Exception =>
        println("Couldn't Integrate Simplified Expression! Expanding...")
        integrateExpandedFunction(exprVal)
        MainIntegral.main(args)
    }
  }
}
