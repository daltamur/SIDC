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
      case value: Var =>
        r match {
          //no known integrals for something like x^x, so we will only worry about if we have something like
          //1^x or x^1. Later on, we'll need to add some sort of exception catcher
          case _: Const =>
            val exp = r.getParamVal
            var newCoefficient = ""
            var newExp = ""
            r.getParamVal match {
              case Right(doubleVal) =>
                newExp = "(" + r.getString() + "+ 1)"
                newCoefficient = "1/" + newExp
            }
            integrationVal = "((" + newCoefficient + ")" + value.n +"^" + newExp+")"

          case _: Var =>
            //nothing we can do with something like x^x

          case _: EP => {
            // just make sure everything inside there is a constant or euler's number
            if(r.asInstanceOf[EP].checkIfAllConstants){
              val exp = r.asInstanceOf[EP].getString
              if(exp.equals("(-1)")){
                integrationVal = "ln["+value.n+"]"
                return
              }
              var newCoefficient = ""
              var newExp = ""
              newExp = "(" + r.getString() + "+ 1)"
              newCoefficient = "1/" + newExp
              integrationVal = "((" + newCoefficient + ")" + value.n + "^" + newExp + ")"
            }else{
              println("ERROR! Cannont Integrate an expression of this form...")
            }
          }
        }


      case _: Const => r match {
        //it is gratuitous right now to do it with two constants, so we'll do it if we have x^5 or something like that
        case _: Var =>
          if(l.asInstanceOf[Const].eulersNum){
            integrationVal = "e^"+r.asInstanceOf[Var].n
            return
          }
          var variableLetter = ""
          var baseValue = ""
          r.getParamVal match {
            case Left(variable) => variableLetter = variable
          }

          baseValue = l.getString()
          if(Integer.parseInt(baseValue)>0) {
            integrationVal = "((" + baseValue + "^" + variableLetter + ")/ln[" + baseValue + "])"
          }else{
            var baseValueNum = Integer.parseInt(baseValue) * -1
            integrationVal = "(-1*((" + baseValueNum + "^" + variableLetter + ")/ln[" + baseValueNum + "]))"
          }

      }

      case _: EP =>
        r match {
          case _: EP => {
            //if we have two expression that are composed of parenthesized expressions, we have to manually check and see
            //if the parenthesized expression are of integratable forms

            //base is a variable, exponent is a constant
            if(!l.asInstanceOf[EP].checkIfAllConstants && r.asInstanceOf[EP].checkIfAllConstants){
              if(l.asInstanceOf[EP].l.l.isInstanceOf[Var]){
                val exp = r.asInstanceOf[EP].getString
                var newCoefficient = ""
                var newExp = ""
                newExp = "(" + r.getString() + "+ 1)"
                newCoefficient = "1/" + newExp
                integrationVal = "((" + newCoefficient + ")" + l.asInstanceOf[EP].l.l.asInstanceOf[Var].n + "^" + newExp + ")"
              }

            }

          }

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
            if(l.asInstanceOf[Const].eulersNum){
              differntiationVal = "e^"+r.asInstanceOf[Var].n
            }else {
              var baseString = l.asInstanceOf[Const].getString()
              if (l.asInstanceOf[Const].v > 0) {
                differntiationVal = "(ln[" + baseString + "]*(" + baseString + ")^" + r.asInstanceOf[Var].n + ")"
              } else {
                val baseVal = l.asInstanceOf[Const].v * -1
                differntiationVal = "(-1*(ln[" + baseVal.asInstanceOf[Int] + "]*(" + baseVal.asInstanceOf[Int] + ")^" + r.asInstanceOf[Var].n + "))"
              }
            }

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
              //this.r = (r.asInstanceOf[EP].l.l)
              powerRule(ml)
            }

          case _: naturalLog =>
            generalizedPowerRule(ml)
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


      case _: naturalLog =>
        println("Natural Log Base Val")
        r match {
          case _: Var =>
            //Var^Var situation,
            generalizedPowerRule(ml)

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
            } else {
              //var^const
              this.r = (r.asInstanceOf[EP].l.l)
              powerRule(ml)
            }

          case _: naturalLog =>
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

    if(r.isInstanceOf[EP]){
      val newExponent = "(" + r.asInstanceOf[EP].getString + "-1" + ")"
      l.differentiate(ml)
      differntiationVal = "((" + r.getString() + ")*(" + l.getString() + "^" + newExponent + ")*(" + l.getDifferentiationVal + "))"
      return
    }


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

  def checkAllOneVar(variable: String): Boolean = {
    var isOneVariable = true
    l match {
      case x:Var => isOneVariable = x.checkAllOneVar(variable)
      case x:FExp => isOneVariable = x.checkAllOneVar(variable)
      case x:EP => isOneVariable = x.checkAllOneVar(variable)
      case _ => //do nothing
    }
    if(isOneVariable) {
      r match {
        case x: Var => isOneVariable = x.checkAllOneVar(variable)
        case x: FExp => isOneVariable = x.checkAllOneVar(variable)
        case x: EP => isOneVariable = x.checkAllOneVar(variable)
        case _ => //do nothing
      }
    }
    isOneVariable
  }

  def checkIfAllConstants: Boolean ={
    var isAllConsts = true
    l match {
      case _: Const => isAllConsts = true
      case _: FExp => isAllConsts = l.asInstanceOf[FExp].checkIfAllConstants
      case _: naturalLog => isAllConsts = l.asInstanceOf[naturalLog].innerFuntion.checkIfAllConstants
      case _: EP => isAllConsts = l.asInstanceOf[EP].checkIfAllConstants
      case _ => return false
    }

    if(isAllConsts) {
      r match {
        case _: Const => isAllConsts = true
        case _: FExp => isAllConsts = r.asInstanceOf[FExp].checkIfAllConstants
        case _: naturalLog => isAllConsts = r.asInstanceOf[naturalLog].innerFuntion.checkIfAllConstants
        case _: EP => isAllConsts = r.asInstanceOf[EP].checkIfAllConstants
        case _ => isAllConsts = false
      }
    }

    isAllConsts
  }


  def negateExponent(): Unit = {
    val negatedExponent = EP(T(Const(-1, eulersNum = false) ,Some(TE(T(this.r, None), '*'))) ,None)
    val negateExponentSimplified = Runners.MainIntegral.ml.evaluateToInputForm(negatedExponent.getString, 0)
    val simplifiedNegatedExponentTree = new ExpressionParserEnhanced(negateExponentSimplified).ParseE
    val simplifiedNegatedExponentParenthesized = EP(simplifiedNegatedExponentTree.l, simplifiedNegatedExponentTree.r)
    this.r = simplifiedNegatedExponentParenthesized
  }
}
