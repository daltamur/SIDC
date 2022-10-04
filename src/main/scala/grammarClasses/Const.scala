package grammarClasses

import com.wolfram.jlink.KernelLink

case class Const(var v: Double) extends F {
  //def eval(env: Main.Environment): Int = v
  override var integrationVal: String = _
  override var differntiationVal: String = _

  override def eval(): Unit = {
    print(v)
  }

  override def compute(): Unit = {
    integrationVal = v + "x"
  }

  override def differentiate(ml: KernelLink): Unit = {
    differntiationVal = "0"
  }

  def getConstVal: Double = {
    return v
  }

  override def getIntegrationVal(): String = {
    return integrationVal
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
    if (convertToIntTruth()){
      return v.toInt.toString
    }else{
      return v.toString
    }
  }
}
