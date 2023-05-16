import grammarClasses.{E, ExpressionParserEnhanced, full_expression_parser}

import java.util.Scanner
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    val scanner: Scanner = new Scanner(System.in)
    println("Type 'd' to solve derivatives or 'i' to solve integrals")
    var choice = scanner.nextLine()
    if(choice.equals("i")) {
      Runners.MainIntegral.main(args)
    }else{
      Runners.MainDerivative.main(args)
    }
  }
}
