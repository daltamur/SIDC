package grammarClasses

import com.wolfram.jlink.KernelLink

case class E3(l: E) extends S {

  override var integrationVal: String = _
  override var differntiationVal: String = _

  override def eval(): Unit = {
    print('-')
    l.eval()
  }

  override def compute(): Unit = {
    l.compute()
    integrationVal = l.getIntegrationVal
  }

  override def getIntegrationVal: String = {
    integrationVal
  }

  override def getDifferentiationVal: String = differntiationVal

  override def differentiate(ml: KernelLink): Unit = {
    l.differentiate(ml)
    differntiationVal = l.getDifferentiationVal
  }

  override def getString: String = l.getString
}
