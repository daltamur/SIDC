package grammarClasses

import com.wolfram.jlink.KernelLink

case class naturalLog(innerFuntion: EP) extends F{
  override def getParamVal: Either[String, Double] = ???

  override def runCompute(): Unit = ???

  override def getString(): String = {
    if(innerFuntion.r.isDefined){
      return "ln["+innerFuntion.l.getString+innerFuntion.r.get.asInstanceOf[S].getString+"]"
    }
    "ln["+innerFuntion.l.getString+"]"
  }

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

  override def compute(): Unit = ???
}
