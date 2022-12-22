package grammarClasses

import com.wolfram.jlink.KernelLink

case class Var(var n: String) extends F {
  override var integrationVal: String = _
  override var differntiationVal: String = _

  override def eval(): Unit = {
    print(n)
  }

  def getVar: String = {
    n
  }

  override def compute(): Unit = {
    integrationVal = "(1/2)" + n + "^2"
  }

  override def differentiate(ml: KernelLink): Unit = {
    differntiationVal = "1"
  }

  override def getIntegrationVal(): String = {
    integrationVal
  }

  override def getDifferentiationVal: String = differntiationVal


  override def getParamVal: Either[String, Double] = {
    Left(n)
  }

  override def runCompute(): Unit = {
    compute()
  }

  override def getString(): String = n

  def checkAllOneVar(variable: String): Boolean = {
    n.equals(variable)
  }
}
