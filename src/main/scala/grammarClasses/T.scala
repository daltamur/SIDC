package grammarClasses

import com.wolfram.jlink.KernelLink

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import Runners.MainIntegral
import Runners.MainIntegral.ml

case class T(var l: F, var r: Option[TE]) extends S {
  override var integrationVal: String = _
  override var differntiationVal: String = _
  private val substitutionMap = new mutable.HashMap[Int, ListBuffer[String]]()

  override def eval(): Unit = {
    //print("<Start T>")
    l.eval()
    r match {
      //there is some multiplication or division happening if we have a TE
      case Some(r) =>
        r.l.r match {
          //if the TE doesn't have a TE itself, then we know we likely have a simple exponent rule
          //for now, this is the one we are going to deal with.
          case None => r.operation match {
            //we are just going to deal with multiplication for now, a little later on we'll handle division
            case '*' =>
          }
        }

      //There is no TE
      case None => print("")
    }

    //print("<End T>")
  }

  override def getIntegrationVal: String = {
    integrationVal
  }

  override def getDifferentiationVal: String = differntiationVal

  def exponentRule(): Unit = {
    l match {
      case _: Const =>
        r match {
          //we already know there is a tail if we get this far, but we can only access the tail's variables if we do this pattern matching step
          case Some(r) =>
            r.l.l match {
              case _: Const =>
                //if we have two constants, just multiply them together and throw an x on the end
                l.getParamVal match {
                  case Right(leftValue) =>
                    r.l.l.getParamVal match {
                      case Right(rightValue) =>
                        val multiplicationVal = leftValue * rightValue
                        integrationVal = multiplicationVal + "*x"
                    }
                }

              case _: Var =>
                //so we have something like 5*x, so all we're going to do is divide the constant by 2 and slap an x on the end.
                l.getParamVal match {
                  case Right(leftValue) =>
                    var newCoefficient:String = ""
                    if(leftValue%1 == 0) {
                      newCoefficient = leftValue .asInstanceOf[Int]+"/"+ 2
                    }else{
                      newCoefficient = leftValue +"/"+ 2
                    }
                    r.l.l.getParamVal match {
                      case Left(variableLetter) =>
                        integrationVal = "("+newCoefficient + ")" + variableLetter + "^2"
                    }
                }

              case _: FExp =>
                //this is for if we have something like 5*x^2. We are going to make sure the exponent is a variable raised to a constant
                //(later on this will be more robust)
                val fexpVal = r.l.l.asInstanceOf[FExp]
                fexpVal.l match {
                  case _: Var =>
                    val base = fexpVal.l.asInstanceOf[Var]
                    fexpVal.r match {
                      case _: Const =>
                        val exponent = fexpVal.r.asInstanceOf[Const]
                        val multiplier = l.asInstanceOf[Const]
                        var newExponent = exponent.v + 1.0
                        var newMultiplier: String = ""
                        if(multiplier.v%1==0 && newExponent%1==0) {
                          newMultiplier = "(" + multiplier.v.asInstanceOf[Int] + "/" + newExponent.asInstanceOf[Int] + ")"
                        }else{
                          newMultiplier = "(" + multiplier.v + "/" + newExponent + ")"
                        }
                        if (newExponent % 1 == 0) {
                          integrationVal = newMultiplier + base.n + "^" + newExponent.asInstanceOf[Int]
                        }else{
                          integrationVal = newMultiplier + base.n + "^" + newExponent
                        }
                    }
                }
            }
        }
    }

  }

  def getNestedUVals(currentNode: F, currentImportance: Int): Unit = {
    currentNode match {
      case _: EP =>
        println("U value at: "+currentNode.getString())
        substitutionMap.put(currentImportance, substitutionMap.getOrElse(currentImportance,
          default = {
            new ListBuffer[String]
          }) += currentNode.asInstanceOf[EP].getString())
        getPossibleUValues(currentNode.asInstanceOf[EP].l, currentImportance+1)
        checkEExtension(currentNode.asInstanceOf[EP].r, currentImportance+1)

      case value: FExp =>
        value.l match {
          case ep: EP =>
            println("U value at: " + value.l.getString())
            substitutionMap.put(currentImportance, substitutionMap.getOrElse(currentImportance,
              default = {
                new ListBuffer[String]
              }) += value.l.getString())
            getPossibleUValues(ep.l, currentImportance + 1)
            checkEExtension(ep.r, currentImportance + 1)
          case _ =>
        }

        value.r match {
          case ep: EP =>
            println("U value at: " + value.r.getString())
            substitutionMap.put(currentImportance, substitutionMap.getOrElse(currentImportance,
              default = {
                new ListBuffer[String]
              }) += value.r.getString())
            getPossibleUValues(ep.l, currentImportance + 1)
            checkEExtension(ep.r, currentImportance + 1)

          case fexp: FExp =>
            println("U value at: " + fexp.getString())
            substitutionMap.put(currentImportance, substitutionMap.getOrElse(currentImportance,
              default = {
                new ListBuffer[String]
              }) += fexp.getString())
            getNestedUVals(fexp, currentImportance + 1)

          case _ =>

        }

      //do nothing for the rest of the possibilities for now
      case _ =>
    }
  }

   def getPossibleUValues(currentNode: T, currentImportance: Int): Unit = {
     //Since we know a U is almost certainly a nested expression, we will first look for the nested expressions
     //println("Current T Node: "+currentNode.getString)
     currentNode.l match {
       case _: EP =>
         getNestedUVals(currentNode.l, currentImportance)
         checkTExtension(currentNode.r, currentImportance)

       case _: FExp =>
         //add the exponent itself
         println("U value at: " + currentNode.l.asInstanceOf[FExp].getString())
         substitutionMap.put(currentImportance, substitutionMap.getOrElse(currentImportance,
           default = {
             new ListBuffer[String]
           }) += currentNode.l.asInstanceOf[FExp].getString())
         getNestedUVals(currentNode.l.asInstanceOf[FExp].l,currentImportance)
         if(currentNode.l.asInstanceOf[FExp].r.isInstanceOf[FExp]){
           println("U value at: "+currentNode.l.asInstanceOf[FExp].r.getString())
           substitutionMap.put(currentImportance, substitutionMap.getOrElse(currentImportance,
             default = {
               new ListBuffer[String]
           })+=currentNode.l.asInstanceOf[FExp].r.getString())
         }
         getNestedUVals(currentNode.l.asInstanceOf[FExp].r, currentImportance+1)
         checkTExtension(currentNode.r, currentImportance)

       case _ =>
         checkTExtension(currentNode.r, currentImportance)

     }

  }

  def checkEExtension(currentNode: Option[Either[E2, E3]], currentImportance: Int): Unit = {
    currentNode match {
      case Some(value) =>
        value match {
          case Left(value)  => {
            getPossibleUValues(value.l.l, currentImportance)
            checkEExtension(value.l.r, currentImportance)
          }
          case Right(value) => {
            getPossibleUValues(value.l.l, currentImportance)
            checkEExtension(value.l.r, currentImportance)
          }
        }

      case _ => //println("Nested expression finished")
    }
  }

  def checkTExtension(currentNode: Option[TE], currentImportance: Int): Unit = {
    currentNode match {
      case Some(value) => getPossibleUValues(value.l, currentImportance)

      case _ => //println("No further U's here")
    }
  }

   def checkIfPossibleSubstitutionRule(curTerm: T): Boolean = {
     curTerm.l match {
       //left value is a constant, skip over it and check all the other terms
       case Const(_, _) =>
         curTerm.r match {
           case Some(value) => checkIfPossibleSubstitutionRule(value.l)
           case _ => false
         }

       case FExp(l, r) =>
         if((l.isInstanceOf[Const] && r.isInstanceOf[EP]) || (l.isInstanceOf[EP] && r.isInstanceOf[Const]) || (l.isInstanceOf[Var] && r.isInstanceOf[Var])){
           true
         }else{
           false
         }

       //every other case, so long as there is multiplication or division happening, there is the possisbility of the substitution rule
       case _ =>
         curTerm.r match {
           case Some(_) => true
           case _ => false
         }

     }
  }

  def checkAllOneVar(variable: String): Boolean = {
    var isOneVariable = true
    l match {
      case x: Var => isOneVariable = x.checkAllOneVar(variable)
      case x: FExp => isOneVariable = x.checkAllOneVar(variable)
      case x: EP => isOneVariable = x.checkAllOneVar(variable)
      case _ => //do nothing
    }

    if(r.isDefined && isOneVariable){
      isOneVariable = r.get.l.checkAllOneVar(variable)
    }
    isOneVariable
  }


  def runIntegralSubRule(): Unit = {
    getPossibleUValues(this, 0)
    //iterate through substitution map keys
    for(subKey <- substitutionMap.keys.toList.sorted.reverse){
      //get the substitution vals at each key
      for(subVal <- substitutionMap(subKey).distinct.toList){
        var localSubValIsU = true
        println("Sub Val: " + subVal)
        var subExpression: String = null
        if(MainIntegral.subIsU) {
          println(this.getString.replace(subVal, "u"))
          subExpression = this.getString.replace(subVal, "u")
          MainIntegral.subIsU = false
        }else{
          println(this.getString.replace(subVal, "v"))
          subExpression = this.getString.replace(subVal, "v")
          MainIntegral.subIsU = true
          localSubValIsU = false
        }
        //take the string, make an expression, and get the derivative of it.
        var simplifiedVal = MainIntegral.ml.evaluateToInputForm("Simplify[" + subVal + "]", 0) + "\n"
        var expr = new ExpressionParserEnhanced(simplifiedVal)//full_expression_parser(simplifiedVal)
        var x = expr.ParseS
        x.asInstanceOf[E].differentiate(MainIntegral.ml)
        val subDeriv =  MainIntegral.ml.evaluateToInputForm("Simplify[" + x.getDifferentiationVal + "]", 0) + "\n"
        println("Sub-Value Derivative: " + subDeriv)
        val rewrittenExpression = "(" + subExpression + ") * (1/(" + subDeriv + "))"
        println("Substituted Val: " +  MainIntegral.ml.evaluateToInputForm("Simplify[" + rewrittenExpression + "]", 0) + "\n")
        simplifiedVal = MainIntegral.ml.evaluateToInputForm("Simplify[" + rewrittenExpression + "]", 0) + "\n"
        expr = new ExpressionParserEnhanced(simplifiedVal)
        x = expr.ParseE
        if(expr.error.isBlank) {
          var allOneVar: Boolean = true
          if (localSubValIsU) {
            allOneVar = x.asInstanceOf[E].checkAllOneVar("u")
          } else {
            allOneVar = x.asInstanceOf[E].checkAllOneVar("v")
          }
          if (allOneVar) {
            expr = new ExpressionParserEnhanced(simplifiedVal) //full_expression_parser(simplifiedVal)
            x = expr.ParseE
            x.asInstanceOf[E].compute()
            if (x.getIntegrationVal != null) {
              if (localSubValIsU) {
                integrationVal = x.getIntegrationVal.replace("u", subVal)
              } else {
                integrationVal = x.getIntegrationVal.replace("v", subVal)
              }
            }
          }
        }else{
          println("Won't work")
        }
      }
    }
  }

  def compute(): Unit = {
    //grammarClasses.F->'('grammarClasses.E')'|var|const|grammarClasses.FExp|Sin(grammarClasses.E)
    //When we are computing an grammarClasses.F, we either have an grammarClasses.EP (an grammarClasses.E expression nested in parentheses), a simple variable letter,
    //some constant value, or an exponent. For now, we are going to worry about the constant values
    if(checkIfPossibleSubstitutionRule(this)) {
      runIntegralSubRule()
    }else {
      r match {
        //there is some multiplication or division happening if we have a TE
        case Some(r) =>
          r.l.r match {
            //if the TE doesn't have a TE itself, then we know we likely have a simple exponent rule
            case None =>
              l match {
                case _: Const =>
                  r.l.l match {
                    case _: Var =>
                      r.operation match {
                        //we are just going to deal with multiplication for now, a little later on we'll handle division
                        case '*' => exponentRule()
                      }

                    case _: FExp =>
                      r.operation match {
                        //we are just going to deal with multiplication for now, a little later on we'll handle division
                        case '*' => exponentRule()
                      }
                  }
                case _: Var =>
                  r.l.l match {
                    case _: Const =>
                      r.operation match {
                        case '*' => exponentRule()
                      }
                  }
              }
            case Some(_) =>
            //getPossibleUValues(this)
          }

        //There is no TE, so just run the compute method on the F node
        case None =>
          l.runCompute()
          integrationVal = l.getIntegrationVal
      }
    }
  }

  def checkForMoreProducts(currentNumber: Int): Boolean = {
    if (currentNumber>2){
      true
    }else{
      this.r match {
        case Some(_) =>
          this.r.get.operation match {
            case '*' =>
              //println(currentNumber)
              this.r.get.l.checkForMoreProducts(currentNumber+1)

            case '/' =>
              //println(currentNumber)
              this.r.get.l.checkForMoreProducts(currentNumber + 1)

            case _ =>
              //println(currentNumber)
              this.r.get.l.checkForMoreProducts(currentNumber)
          }
        case None =>
          //println(currentNumber)
          false
      }
    }
  }

  def applyGeneralProductRule(ml: KernelLink): Unit = {
    //we will first check if the current l node has a constant, if it does, we know we are at the start of the expression
    //thanks to wolfram simplifying input and we can just do the const*rest of expression'
    println("Product rule!")
    l match {
      //our l node is a constant
      case _:Const =>
        r.get.l.differentiate(ml)
        differntiationVal = "(" + l.asInstanceOf[Const].getString() + ")*(" + r.get.l.differntiationVal + ")"

      //for all other things, we're going to be applying the product rule
      case _ =>
        r.get.operation match {
          case '*' =>
            l.differentiate(ml)
            r.get.l.differentiate(ml)
            //later on, we will check the type of the operator for if we have to apply the general product rule or general quotient rule
            differntiationVal = "((("+l.getString() + ")*(" + r.get.l.differntiationVal + "))+((" + l.getDifferentiationVal + ")*(" + r.get.l.getString+")))"

          case '/' =>
            //for every other type, we apply the general product rule
            //first, invert the divisor
            val invertedDivisor = FExp(EP(T(r.get.l.l, None), None), EP(T(Const(-1.0, false), None), None))
            val invertedDivisorString: String = ml.evaluateToInputForm("Simplify[" + invertedDivisor.getString() + "]", 0)
            val reParse = new full_expression_parser(invertedDivisorString)
            val finalInvertedExpression = reParse.parseT()
            this.l.differentiate(ml)
            finalInvertedExpression.r = Some(TE(T(this.r.get.l.r.get.l.l, None), this.r.get.l.r.get.operation))
            finalInvertedExpression.differentiate(ml)
            differntiationVal = "((" + this.l.getString() + ")*(" + finalInvertedExpression.getDifferentiationVal + ")     +     " + "(" + this.l.getDifferentiationVal + ")*(" + finalInvertedExpression.getString + "))"

        }
    }
  }


   def checkLeadingConstantNonProductRule(ml: KernelLink): Unit = {
    r match {
      //make sure it's not just a lone constant. And if it is, return 0
      case Some(value) =>
        value.operation match {
          //check if the operation is multiplication
          //thanks to wolfram, we will never see two constants multiplied together, so we don't have to worry about it
          //the constant term is ALWAYS going to be pushed to the front
          case '*' =>
            value.l.differentiate(ml)
            differntiationVal = this.l.asInstanceOf[Const].getString() + "*(" + value.l.differntiationVal+")"

          //if it is division, we will transform it so that it becomes const*derivativeOf(1/TE)
          //note that we will never have a case of constant/constant, so we do not need to check for that.
          case '/' =>
            value.l.l match {
              case _: Const =>
              //dividing by a constant, this will be exceedingly rare and only if the user gives some crazy derivative
              val constant_value = l.getString()+"/"+value.l.l.getString()
              //check if we're multipilying or dividing this fraction by something and act accordingly
              value.l.r match {
                case Some(value) =>
                  {
                    value.operation match {
                      case '*' =>
                        value.l.differentiate(ml)
                        differntiationVal = "("+constant_value+")*"+value.l.getDifferentiationVal
                      case '/' =>
                        value.l.differentiate(ml)
                        differntiationVal = "("+constant_value+")/"+value.l.getDifferentiationVal

                    }
                  }
                case None =>
                  differntiationVal = "0"
              }
              //dividing by an exponent
              case _: FExp =>
                value.l.l.asInstanceOf[FExp].r match {
                  case ep: EP =>
                    value.l.l.asInstanceOf[FExp].r = EP(T(Const(-1.0,false), Some(TE(T(ep, None), '*'))), None)
                    println(value.l.l.getString())
                  //exponent value is an exponent itself, do -1*(exponent value)
                  case exp: FExp =>
                    value.l.l.asInstanceOf[FExp].r = EP(T(Const(-1.0, false), Some(TE(T(EP(T(exp, None),None), None), '*'))), None)
                    println(value.l.l.getString())

                  //exponent is a constant
                  case constant: Const =>
                    value.l.l.asInstanceOf[FExp].r.asInstanceOf[Const].v = -constant.v
                    println(value.l.l.getString())
                  //exponent is a variable
                  case variable: Var =>
                    value.l.l.asInstanceOf[FExp].r.asInstanceOf[Var].n = "-"+variable.n
                    println(value.l.l.getString())

                }
              //dividing by a parenthesized expression with no exponent attached to it
           case expression: EP =>
             value.l.l =  FExp(expression,Const(-1.0, false))
             println(value.l.getString)
             value.l.l.asInstanceOf[FExp].differentiate(ml)
             differntiationVal = value.l.l.getDifferentiationVal
             println(differntiationVal)

           case variable: Var =>
             value.l.l =  FExp(variable,Const(-1.0, false))
             println(value.l.getString)
             value.l.l.asInstanceOf[FExp].differentiate(ml)
             differntiationVal = "("+l.getString() + ")*(" + value.l.l.getDifferentiationVal+")"
             println(differntiationVal)


            }



        }
      case None =>
        l.differentiate(ml)
        differntiationVal = l.getDifferentiationVal


    }
  }

  def nonProductRuleFExp(ml: KernelLink): Unit = {
    //thanks to wolfram, we will never see an exponent multiplied by another exponent,
    //an exponent multiplied by a constant (it will always be constant*FEXP),
    //or an exponent multiplied by a single variable.
    //so for multiplication, we only have to check for an exponent multilied by an EP
    r match {
      case Some(value) =>
        //if we have something, we apply the product rule since the only thing that could be multiplied against what we have
        //is an EP node
        value.operation match {
          case '*' =>
            this.l.differentiate(ml)
            value.l.l.differentiate(ml)
            differntiationVal = "(("+this.l.getDifferentiationVal + "*" + value.l.getString + ")+(" + this.l.getString() + "*" + value.l.l.getDifferentiationVal+"))"

          case  '/'  =>
            //the only two cases, FEXP/EP and FEXP/FEXP, are performed with the quotient rule. If we have FEXP/5, it is simply 1/5.
            l.differentiate(ml)
            value.l.l.differentiate(ml)
            differntiationVal = "((" + l.getDifferentiationVal + ")*(" + value.l.l.getString() + ")-(" + l.getString() + ")*(" + value.l.l.getDifferentiationVal + "))/((" + value.l.l.getString() + ")^2)"
            println(differntiationVal)
        }


      case None =>
        l.differentiate(ml)
        differntiationVal = l.getDifferentiationVal
    }
  }

  def nonProductRuleVar(ml: KernelLink): Unit = {
    //thanks to wolfram, we will only ever see var/EP or var/FEXP, so we only have to do checks for that
    r match {
      case Some(value) =>
        //check if we're multiplying or dividing, that'll change what we do a lot
        value.operation match {
          case '/' =>
                //the only two cases, var/EP and var/FEXP, are performed with the quotient rule. If we have x/5, it is simply 1/5.
                l.differentiate(ml)
                value.l.l.differentiate(ml)
                differntiationVal = "(("+l.getDifferentiationVal+")*("+value.l.l.getString()+")-("+l.getString()+")*("+value.l.l.getDifferentiationVal+"))/("+value.l.l.getString()+"^2)"
                println(differntiationVal)
          case '*' =>
              //we could only have var*EP or var*EXP, var*var will automatically simplify to var^2 and we will only have const*var, not var*const
              //so the only rule that can be applied here is the product rule
            l.differentiate(ml)
            value.l.l.differentiate(ml)
            differntiationVal = "(("+l.getString+"*"+value.l.l.getDifferentiationVal+")"+"    +    ("+l.getDifferentiationVal+"*"+value.l.l.getString()+"))"
        }


      case None =>
        l.differentiate(ml)
        differntiationVal = l.getDifferentiationVal

    }
  }

  def nonProductRuleLN(ml: KernelLink): Unit = {
    //works similarly to nonProductRuleVar
    r match {
      case Some(value) =>
        //check if we're multiplying or dividing, that'll change what we do a bit
        value.operation match {
          case '*' =>
            //when multpliying it will never be ln(x)*5, so let's just apply product rule when this happens
            this.l.differentiate(ml)
            value.l.l.differentiate(ml)
            if (value.l.l.getDifferentiationVal != null && this.l.getDifferentiationVal != null) {
              differntiationVal = "(" + this.l.getDifferentiationVal + ")*(" + value.l.getString + ")+(" + this.l.getString() + ")*(" + value.l.l.getDifferentiationVal + ")"
            }else{
              differntiationVal = null
            }

          case '/' =>
            //we'll handle division later, this is solely so that we can do the general product rule

        }


      case None =>
        l.differentiate(ml)
        differntiationVal = l.getDifferentiationVal

    }
  }

  override def differentiate(ml: KernelLink): Unit = {
    //you will notice this is similar to the integrate function.
    //check for leading/ending constant value
    if(!checkForMoreProducts(1)) {
      l match {
        case _: Const =>
          checkLeadingConstantNonProductRule(ml)
        case _: FExp =>
          nonProductRuleFExp(ml)
        case _: Var =>
          nonProductRuleVar(ml)
        case _: EP =>
          //if we have an EP, there are only a few operations that can currently be happening:
          //EP*EP
          //EP*FEXP
          //EP*Var
          //(We will never have EP*Const thanks to Wolfram, will always be of the form Const*EP)
          //EP/Const
          //EP/Var
          //EP/EP
          //EP/FEXP
          //Or we just have EP alone, in which case just integrate EP

          //let's handle multiplication first, since we do the exact same thing for each case (all are the product rule)
          r match {
            case Some(value) =>
              value.operation match {
                //apply product rule!
                case '*' =>
                  this.l.differentiate(ml)
                  value.l.differentiate(ml)
                  differntiationVal = this.l.getString()+"*"+value.l.getDifferentiationVal+"+"+this.l.getDifferentiationVal+"*"+value.l.getString

                case '/' =>
                  //if the divisor is a Var, EP, or EXP, we apply the general quotient rule. Otherwise, it is  just differentiation the numerator and multiplying it by 1/Divisor
                  value.l.l match {
                    case _: Const =>
                      this.l.differentiate(ml)
                      differntiationVal = this.l.getDifferentiationVal + "*1/"+value.l.l.getString()

                    case _=>
                      //for every other type, we apply the general product rule
                      //first, invert the divisor
                      //value.l.l
                      val invertedDivisor = FExp(EP(T(value.l.l, None), None), EP(T(Const(-1.0, false), None), None))
                      //val invertedDivisorString: String = ml.evaluateToInputForm("Simplify[" + invertedDivisor.getString() + "]", 0)
                      val reParse = new full_expression_parser(invertedDivisor.getString())
                      val finalInvertedExpression = reParse.parseT()
                      println("inverted val "+ finalInvertedExpression.getString)
                      this.l.differentiate(ml)
                      finalInvertedExpression.differentiate(ml)
                      println("inverted dif "+finalInvertedExpression.getDifferentiationVal)
                      differntiationVal = "("+this.l.getString()+")*("+finalInvertedExpression.getDifferentiationVal+")+"+"("+this.l.getDifferentiationVal+")*("+finalInvertedExpression.getString+")"
                  }
              }

            case None =>
              this.l.differentiate(ml)
              differntiationVal = this.l.getDifferentiationVal
          }


        case _: naturalLog =>
          nonProductRuleLN(ml)
      }
    }else{
      applyGeneralProductRule(ml)
    }


  }

  override def getString: String = {
    r match {
      case None =>
        l.getString()

      case _:Option[TE] =>
        l.getString+ r.get.getString
    }
  }

  def checkIfJustConstant: Boolean = {
    this.r match {
      case Some(_) =>
        false

      case None =>
        l match {
          case _: Const =>
            true
          case _ =>
            false
        }
    }
  }

  def checkIfNegativeConstant:Boolean = {
    this.r match {
      //we will never have a constant multiplied by a constant, so we are guaranteed a potential positive value here
      case Some(_) =>
        false

      case None =>
        l match {
          case value: Const =>
            if(value.eulersNum){
              return false
            }
            if(value.v <= 0){
              true
            }else{
              false
            }
          case _: EP =>
            l.asInstanceOf[EP].l.checkIfNegativeConstant
          case _ =>
            false

        }
    }
  }
}
