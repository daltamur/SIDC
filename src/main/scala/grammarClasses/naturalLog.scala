package grammarClasses

import com.wolfram.jlink.KernelLink

case class naturalLog(innerFuntion: EP) extends F{
  override def getParamVal: Either[String, Double] = ???

  override def runCompute(): Unit = ???

  override def getString(): String = {
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

  override def getIntegrationVal: String = ???

  override def differentiate(ml: KernelLink): Unit = {
    if(!innerFuntion.l.checkIfNegativeConstant) {
      innerFuntion.differentiate(ml)
      differntiationVal = "(1/" + innerFuntion.getString + ")*" + innerFuntion.getDifferentiationVal
    }else{
      differntiationVal = null
    }
  }

  override def compute(): Unit = {



  }
}
