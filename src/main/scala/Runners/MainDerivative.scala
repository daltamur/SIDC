package Runners

import com.wolfram.jlink.{KernelLink, MathLinkException}
import grammarClasses.full_expression_parser

import java.util.Scanner

object MainDerivative {

  //make our wolfram kernel link
  System.mapLibraryName("JLinkNativeLibrary")
  System.load("/usr/local/Wolfram/WolframEngine/13.1/SystemFiles/Links/JLink/SystemFiles/Libraries/Linux-x86-64/libJLinkNativeLibrary.so")
  val kernelLinkArgs: Array[String] = Array("-linkmode", "launch", "-linkname", "/usr/local/Wolfram/WolframEngine/13.1/Executables/math")
  var ml: KernelLink = null
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
        if (strResult == "$Failed\n") {
          //just to get what the parse error is
          val expr = new full_expression_parser(exprVal)
          expr.parseS()
          MainIntegral.main(args)
        }

      } catch {
        case e: MathLinkException => println(e.getMessage)
      }

      //println(strResult)
      val expr = new full_expression_parser(strResult)
      val x = expr.parseE()
      //println(x.getString)
      println(x)
      x.differentiate(ml)

      if (x.getDifferentiationVal != null) {
        println(x.getDifferentiationVal)
      } else {
        println("ERROR! Attempted to use a non-positive number in natural log")
      }
      try {
        strResult = ml.evaluateToOutputForm("Simplify[" + x.getDifferentiationVal + "]", 0)
        if (strResult.contains("null")) {
          println("Something went wrong")
        } else {
          println(strResult)
        }
      } catch {
        case e: MathLinkException => println(e.getMessage)
      }

      println("Done computing")
      MainDerivative.main(args)
    }
    catch {
      case e: Exception =>
        println(e)
        MainDerivative.main(args)
    }
  }
}
