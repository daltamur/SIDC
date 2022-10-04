package grammarClasses

import com.wolfram.jlink.KernelLink

abstract class S extends java.util.concurrent.RecursiveAction{
  // some sort of abstract function would go here
  var integrationVal: String

  var differntiationVal: String
  def getDifferentiationVal: String
  def eval()
  def getIntegrationVal: String

  def differentiate(ml: KernelLink): Unit
  def getString: String
}
