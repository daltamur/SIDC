package grammarClasses

import com.wolfram.jlink.KernelLink

case class Const(var v: Double, val eulersNum: Boolean) extends F {
  //def eval(env: Main.Environment): Int = v
  override var integrationVal: String = _
  override var differntiationVal: String = _

  override def eval(): Unit = {
    print(v)
  }

  override def compute(): Unit = {
    if(!eulersNum) {
      var vVal: String = ""
      if(v%1 == 0){
        vVal = v.toInt.toString
      }else{
        vVal = v.toString
      }
      integrationVal = vVal + Runners.MainIntegral.curVar
      if(Runners.MainIntegral.curVar.isBlank){
        integrationVal = vVal + "x"
      }
    } else{
      integrationVal = "e" + Runners.MainIntegral.curVar
      if (Runners.MainIntegral.curVar.isBlank) {
        integrationVal = "e" + "x"
      }
    }
  }

  override def differentiate(ml: KernelLink): Unit = {
    differntiationVal = "0"
  }

  def getConstVal: Double = {
    v
  }

  override def getIntegrationVal(): String = {
    integrationVal
  }

  override def getParamVal: Either[String, Double] = {
    Right(v)
  }

  override def runCompute(): Unit = {
    compute()
  }

  def convertToIntTruth(): Boolean = {
    if(v%1!=0){
      false
    }else{
      true
    }
  }
  override def getString(): String = {
    if(eulersNum){
      return "e"
    }
    if (convertToIntTruth()) {
      v.toInt.toString
    } else {
      v.toString
    }
  }
}
