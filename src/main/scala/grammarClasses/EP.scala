package grammarClasses

import com.wolfram.jlink.KernelLink

case class EP(var l: T, var r: Option[Either[E2, E3]]) extends F {

  override var integrationVal: String = _
  override var differntiationVal: String = _

  override def eval(): Unit = {
    //print("<start parenthesis>")
    print('(')
    l.eval()
    r match {
      case Some(Left(r)) => r.eval()
      case Some(Right(r)) => r.eval()
      case None => print("")
    }
    print(')')
    //print("<end parenthesis>")

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
    if (r.isDefined && r.get.isLeft) {
      returnedVal = r.get.asInstanceOf[Left[E2, E3]].value.l.checkAllOneVar(variable)
    } else if (r.isDefined && r.get.isRight) {
      returnedVal = r.get.asInstanceOf[Right[E2, E3]].value.l.checkAllOneVar(variable)
    }
    returnedVal
  }

  override def getIntegrationVal(): String = {
    integrationVal
  }

  override def differentiate(ml: KernelLink): Unit = {
    l.differentiate(ml)
    r match {
      case Some(Left(r)) =>
        r.differentiate(ml)
        differntiationVal = l.getDifferentiationVal + "+" + r.getDifferentiationVal
        differntiationVal = "("+differntiationVal+")"
      case Some(Right(r)) =>
        r.differentiate(ml)
        differntiationVal = "("+l.getDifferentiationVal + "-" + r.getDifferentiationVal+")"
        differntiationVal = "("+differntiationVal+")"
      case None => differntiationVal = "("+l.getDifferentiationVal+")"
    }
  }

  override def getDifferentiationVal: String = differntiationVal

  override def getParamVal: Either[String, Double] = ???

  override def runCompute(): Unit = {
    compute()
  }


   def checkIfSingleTerm(): Boolean = {
    r match {
      case Some(_) => false

      case None =>
        l.r match {
          case Some(_) => false
          case None =>
            l.l match {
              case _: Const => true
              case _: Var => true
              case _: FExp => true
              case _: EP => l.l.asInstanceOf[EP].checkIfSingleTerm()
            }
        }
    }
  }

  override def getString: String = {
    r match {
      case None =>
        '('+l.getString+')'
      case _: Option[Either[E2, E3]]=>
        val rVal = r.get
        rVal match {
          case Left(rVal) =>
            '('+l.getString + '+' + rVal.getString+')'
          case Right(rVal) =>
            '('+l.getString+ '-' + rVal.getString+')'
        }
    }
  }

  def checkIfJustConstant: Boolean = {
    r match {
      case Some(_) =>
        false
      case None =>
        //if there is none then there is a good chance that the T value in l is just a constant
        l.checkIfJustConstant
    }
  }
}
