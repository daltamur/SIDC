package grammarClasses

import com.wolfram.jlink.KernelLink

case class TE(var l: T, operation: Char) extends S {
  override var integrationVal: String = _
  override var differntiationVal: String = _

  override def eval(): Unit = {
    print(operation)
    l.eval()
  }

  override def compute(): Unit = ???

  override def getIntegrationVal(): String = {
    integrationVal
  }

  override def getDifferentiationVal: String = differntiationVal

  override def differentiate(ml: KernelLink): Unit = {}

  override def getString: String = operation+l.getString

  def getLeft: T = {
    l
  }
}
