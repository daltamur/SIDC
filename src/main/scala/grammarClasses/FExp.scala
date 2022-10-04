package grammarClasses

import com.wolfram.jlink.KernelLink

case class FExp(var l: F, var r: F) extends F {
  override var integrationVal: String = _
  override var differntiationVal: String = _

  override def eval(): Unit = {
    l.eval()
    print('^')
    r.eval()
  }

  def getBase: F = {
    l
  }

  def getExponent: F = {
    r
  }

  override def compute(): Unit = {
    l match {
      //we love pattern matching, folks
      case _: Var =>
        r match {
          //no known integrals for something like x^x, so we will only worry about if we have something like
          //1^x or x^1. Later on, we'll need to add some sort of exception catcher
          case _: Const =>
            val exp = r.getParamVal
            var newCoefficient = ""
            var newExp = 0.0
            r.getParamVal match {
              case Right(doubleVal) =>
                newExp = doubleVal + 1.0
                newCoefficient = "1/" + newExp.toInt
            }
            integrationVal = "((" + newCoefficient + ")" + "x^" + newExp.toInt+")"
        }


      case _: Const => r match {
        //it is gratuitous right now to do it with two constants, so we'll do it if we have x^5 or something like that
        case _: Var =>
          var variableLetter = ""
          var baseValue = 0.0
          r.getParamVal match {
            case Left(variable) => variableLetter = variable
          }
          l.getParamVal match {
            case Right(doubleVal) => baseValue = doubleVal
          }
          integrationVal = "((" + baseValue + "^" + variableLetter + ")/ln[" + baseValue + "])"

      }

      case _ =>
    }


  }

  override def differentiate(ml: KernelLink): Unit = {
    //for this, we can have the following forms:
    //Const^Var //Exponent rule
    //Const^EP //Generalized Exponent rule (just make sure that the EP is not just a negative constant)
    //Var^const //Power rule
    //Var^Var //Exponent Rule
    //Var^EP //Generalized Power Rule
    //EP^Const //Power Rule
    //EP^Var Generalized Power Rule
    //EP^EP Generalized Power Rule (Just check and make sure EP is not a negative constant)
    //note that some time later we will add logarithm and trig functionality ;)

    l match {
      case _: Const =>
        println("Const base val")
        r match {
          case _: Var =>
            //exponentRule rule
            val baseString = l.asInstanceOf[Const].getString()
            differntiationVal = "(ln[" + baseString + "]*(" + baseString + ")^" + r.asInstanceOf[Var].n+")"

          case _ =>
            //exponent is EP or FEXP
            //generalized Exponential Rule applies
            val baseString = l.asInstanceOf[Const].getString()
            r.differentiate(ml)
            differntiationVal = "(ln[" + baseString + "]*(" + getString() + ")*(" + r.getDifferentiationVal + "))"
          //done with constant as base
        }

      case _: Var =>
        println("Var Base Val")
        r match {
          case _: Var =>
            //Var^Var situation,
            differntiationVal = "(("+getString() + ")*(" + "ln[" + r.getString() + "]+1))"

          case _: Const =>
            //Var^Constant
            //Simple Power Rule
            powerRule(ml)

          case _: FExp =>
            //Var^FEXP
            //Generalized Power Rule
            generalizedPowerRule(ml)

          case _: EP =>
            //this also uses the general power rule, but let's make sure that it isn't just a parenthesized negative number (i.e x^(-2)) since Wolfram will do that
            if (!r.asInstanceOf[EP].checkIfSingleTerm()) {
              //Var^EP
              //Generalized power rule
              println("gen power rule")
              generalizedPowerRule(ml)
            }else{
              //var^const
              this.r = (r.asInstanceOf[EP].l.l)
              powerRule(ml)
            }
        }

      case _: EP =>
        println("EP Base Val")
        //we do generalized power rule for everything except when it is EP^(-Const), that will use generalized power rule
        //later on remember to do checks to make sure the parenthesized expressions are not just single values. For now we will just assume the are not
        r match {
          case _: Const =>
            //EP^Const Power rule
            powerRule(ml)

          case _: EP =>
            if(r.asInstanceOf[EP].checkIfSingleTerm()){
              this.r = (r.asInstanceOf[EP].l.l)
              powerRule(ml)
            }else{
              generalizedPowerRule(ml)
            }

          case _ =>
            //every other possibility uses the generalized power rule
            //Var^FEXP
            //Generalized Power Rule
            generalizedPowerRule(ml)
        }


    }
  }

  def generalizedPowerRule(ml: KernelLink): Unit = {
    //make the (ln[u(x)]*v(x)) term
    val rightSideTerm = T(naturalLog(EP(T(l, None), None)), Some(TE(T(this.r, None), '*')))
    rightSideTerm.differentiate(ml)
    println(rightSideTerm.getDifferentiationVal)
    if(rightSideTerm.getDifferentiationVal != null) {
      differntiationVal = "((" + getString() + ")*(" + rightSideTerm.getDifferentiationVal + "))"
    }else{
      differntiationVal = null
    }
  }
  def powerRule(ml: KernelLink): Unit = {
    val currentExponent = r.asInstanceOf[Const].v
    val newExponent = currentExponent - 1
    var newExponentString = ""
    if (newExponent % 1 == 0) {
      newExponentString = newExponent.toInt.toString
    } else {
      newExponentString = newExponent.toString
    }
    l.differentiate(ml)
    differntiationVal = "(("+r.getString() + ")*(" + l.getString() + "^" + newExponentString + ")*(" + l.getDifferentiationVal + "))"
  }

  override def getIntegrationVal: String = {
    integrationVal
  }
  override def getDifferentiationVal: String = differntiationVal

  //this is a placeholder, we'll never really use this function
  override def getParamVal: Either[String, Double] = {
    Left("NULL")
  }

  override def runCompute(): Unit = {
    compute()
  }

  override def getString(): String = l.getString+'^'+r.getString
}
