package grammarClasses

import com.wolfram.jlink.KernelLink

case class naturalLog(innerFuntion: EP) extends F{
  override def getParamVal: Either[String, Double] = ???

  override def runCompute(): Unit = compute()

  override def getString(): String = {
    if(innerFuntion.getString.equals("(e)")){
      return "1"
    }

    if(innerFuntion.r.isDefined){
      if(innerFuntion.r.get.isLeft){
        return "ln["+innerFuntion.l.getString+"+"+innerFuntion.r.get.asInstanceOf[Left[E2, E3]].value.getString+"]"
      }else{
        return "ln["+innerFuntion.l.getString+"-"+innerFuntion.r.get.asInstanceOf[Right[E2, E3]].value.getString+"]"
      }
    }
    "ln["+innerFuntion.l.getString+"]"
  }

  //(2*x*ln[x^2+5])/(x^2+5)
  override var integrationVal: String = _
  override var differntiationVal: String = _

  override def eval(): Unit = ???

  override def getIntegrationVal: String = integrationVal

  override def differentiate(ml: KernelLink): Unit = {
    innerFuntion.differentiate(ml)
    differntiationVal = "(1/" + innerFuntion.getString + ")*" + innerFuntion.getDifferentiationVal
  }

  override def compute(): Unit = {
    //if we get here, it's going to be an IBP. We can just hardcode IBP into it so we can do stuff with ln
    var innerFunctionParsed = new ExpressionParserEnhanced(this.innerFuntion.getString)
    innerFunctionParsed.ParseE
    var varLetter = innerFunctionParsed.curVar
    //f is the natural log itself, g is the variable letter
    this.differentiate(Runners.MainIntegral.ml)
    var diffVal = Runners.MainIntegral.ml.evaluateToInputForm(this.getDifferentiationVal + "*" + varLetter, 0)
    var difValParsed = new ExpressionParserEnhanced(diffVal).ParseE
    if(difValParsed.checkIfAllConstants){
      val difVal = "("+difValParsed.getString + ")*"+varLetter
      integrationVal = "((" + this.getString() + "*" + varLetter + ")" + "-(" + difVal + "))"
    }else if(difValParsed.getString.equals("1/ln["+varLetter+"]") || (difValParsed.l.l.isInstanceOf[FExp] && difValParsed.l.l.asInstanceOf[FExp].l.isInstanceOf[naturalLog] && difValParsed.l.l.asInstanceOf[FExp].r.isInstanceOf[EP] && difValParsed.l.l.asInstanceOf[FExp].r.asInstanceOf[EP].getStringNoParen.equals("-1"))){
      integrationVal = "((" + this.getString() + "*" + varLetter + ")" + "-(LogIntegral[" + varLetter + "]))"
    } else{
      difValParsed.compute()
      integrationVal = "((" + this.getString() + "*" + varLetter + ")" + "-(" + difValParsed.getIntegrationVal + "))"
    }
  }
}
