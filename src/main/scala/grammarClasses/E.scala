package grammarClasses

import com.wolfram.jlink.KernelLink

case class E(l: T, r: Option[Either[E2, E3]]) extends S {

  override var integrationVal: String = _
  override var differntiationVal: String = _

  override def eval(): Unit = {
    /*
    l.eval()
    r match {
      case Some(Left(r)) =>
        r.eval()
      case Some(Right(r)) => r.eval()
      case None => print("")
    }
     */
  }

  override def compute(): Unit = {
    l.compute()
    r match {
      case Some(Left(r)) =>
        r.fork()
        r.join()
        integrationVal = l.getIntegrationVal + "+" + r.getIntegrationVal
      case Some(Right(r)) =>
        r.fork()
        r.join()
        integrationVal = l.getIntegrationVal + "-" + r.getIntegrationVal
      case None => integrationVal = l.getIntegrationVal
    }


  }

  def checkAllOneVar(variable: String): Boolean = {
    var returnedVal = l.checkAllOneVar(variable)
    if(!returnedVal){
      return returnedVal
    }
    if(r.isDefined && r.get.isLeft){
      returnedVal = r.get.asInstanceOf[Left[E2, E3]].value.l.checkAllOneVar(variable)
    }else if(r.isDefined && r.get.isRight){
      returnedVal = r.get.asInstanceOf[Right[E2, E3]].value.l.checkAllOneVar(variable)
    }
    returnedVal
  }

  override def getIntegrationVal: String = {
    integrationVal
  }

  override def getDifferentiationVal: String = differntiationVal

  override def differentiate(ml: KernelLink): Unit = {
    l.differentiate(ml)
    if(l.getDifferentiationVal != null) {
      r match {
        case Some(Left(r)) =>
          r.differentiate(ml)
          if(r.getDifferentiationVal != null) {
            differntiationVal = l.getDifferentiationVal + "+" + r.getDifferentiationVal
          }else{
            differntiationVal = null
          }
        case Some(Right(r)) =>
          r.differentiate(ml)
          if(r.getDifferentiationVal != null) {
            differntiationVal = l.getDifferentiationVal + "-" + r.getDifferentiationVal
          }else{
            differntiationVal = null
          }
        case None => differntiationVal = l.getDifferentiationVal
      }
    }else{
      differntiationVal = null
    }
  }

  override def getString: String = {
    r match {
      case None =>
        l.getString
      case _: Option[Either[E2, E3]]=>
        val rVal = r.get
        rVal match {
          case Left(rVal) =>
            l.getString + '+' + rVal.getString
          case Right(rVal) =>
            l.getString+ '-' + rVal.getString
        }
    }
  }
}
