package grammarClasses

import scala.util.matching.Regex

//grammarClasses.S->grammarClasses.E
//grammarClasses.E-> Term Term_Tail
//Term-> factor factor_tail
//this keeps addition and subtraction towards the top of the tree, so they are done last
//Term_Tail->'+'grammarClasses.E|'-'grammarClasses.E|null
//factor-> '('grammarClasses.E')'|var|num
//this puts multiplication and division to the bottom of the tree, so it is done last
//factor_tail-> '*'Term

//Doing it this way isn't really conducive of a tree structure...here's a better grammar:
//Though this is arguably an ambigiuous grammar, it will get our job done!
//things in brackets represent an optional token
// grammarClasses.S = Statement
// grammarClasses.E = Expression
// grammarClasses.T = Terminal
// grammarClasses.F = Factor
// var = variable
// const = constant value (some number)
//___


class full_expression_parser(input: String) {
  val constregex: Regex = "^(\\-|(\\d(\\.))?)\\d+(\\.\\d+)?".r
  val varregex: Regex = "^[A-Za-z]+".r
  //this will serve as our incrementer in parsing the expression
  private var index = 0
  private var hadError = false

  def parseS(): S = {
    val x =parseE()
    if(input(index)!='\n'){
      println("Error: Did not get end of line character at index "+index)
      hadError = true
    }
    x
  }

  def parseE(): E = {
    //check for first set characters, throw error if none are first set characters
    val negconstregex: Regex = "^\\-[0-9]+(\\.[0-9]+)?".r
    val currStrVal = input.substring(index)
    val constsVal = negconstregex.findAllIn(currStrVal)
    val consts = constregex.findAllIn(currStrVal)
    val varVal = varregex.findAllIn(currStrVal)
    if(input(index) == '('||input(index)=='-'||constsVal.hasNext||varVal.hasNext||consts.hasNext) {
      E(parseT(), parseETail())
    }else{
      println("Error: Expected (, variable, or constant value, instead got "+input(index)+" at "+index)
      hadError = true
      E(T(Const(5, eulersNum = false), None), None)
    }
  }

  def parseETail(): Option[Either[E2, E3]] = {
    //if it is adding, then we have an grammarClasses.E2 class
    skipWhiteSpace()
    val negconstregex: Regex = "^\\-[0-9]+(\\.[0-9]+)?".r
    val currStrVal = input.substring(index)
    val consts = constregex.findAllIn(currStrVal)
    val constsVal = negconstregex.findAllIn(currStrVal)
    val varVal = varregex.findAllIn(currStrVal)
    if(input(index) == '('||input(index)=='-'||constsVal.hasNext||consts.hasNext||varVal.hasNext||input(index) == '+'||input(index) == '-') {
      if (index < input.length && input(index) == '+') {
        //println("index at ETail")
        index += 1; // Advance past +
        skipWhiteSpace()
        //println(index)
        Some(Left(E2(parseE())))
        //if it is subtracting, then we have an grammarClasses.E3 class
      } else if (index < input.length && input(index) == '-') {
        //println("Index at ETail")
        index += 1; // Advance past +
        skipWhiteSpace()
        //println(index)
        Some(Right(E3(parseE())))
      }
      //if neither of the above conditions are met, then we have reached the end of this part of the expression
      else None
    }else{
      None
    }

  }

  def parseT(): T = {
    skipWhiteSpace()
    if (input(index) != '-'){
      T(parseF(), parseTTail())
    }else{
      val negconstregex: Regex = "^\\-[0-9]+(\\.[0-9]+)?".r
      val negvarregex: Regex = "^\\-[A-Za-z]+".r
      val currStrVal = input.substring(index)
      val constsVal = negconstregex.findAllIn(currStrVal)
      val stringVal = negvarregex.findAllIn(currStrVal)
      /*
      The if here used to look like
      if(!constsVal.hasNext && !stringVal.hasNext){
       */

      if(!stringVal.hasNext){
        T(parseF(), parseTTail())
      }else{
        index+=1
        skipWhiteSpace()
        T(Const(-1, eulersNum = false),Some(TE(parseT(), '*')) )
      }

    }
  }

  def parseTTail(): Option[TE] = {
    //if it is multiplying, then we have an grammarClasses.E2 class
    skipWhiteSpace()
    if (index < input.length && input(index) == '*'){
      index+=1; // Advance past *
      skipWhiteSpace()
      Some(TE(parseT(), '*'))
      //if it is dividing, then we have an grammarClasses.E3 class
    }else if(index < input.length && input(index) == '/'){
      index+=1; // Advance past /
      skipWhiteSpace()
      Some(TE(parseT(), '/'))
    }
    //if neither of the above conditions are met, then we have reached the end of this part of the expression
    else None

  }

  def parseSqrt(): F = {
    //index past the bracket
    index+=1
    val rtrn = FExp(parseF(), EP(T(Const(1, eulersNum = false), Some(TE((T(Const(2, eulersNum = false), None)), '/'))), None))
    if(input(index) != ']'){
      println("ERROR: Expected ']'")
    }
    rtrn
  }

  def parseF(): F = {
    // Get the unparsed part of the string.
    skipWhiteSpace()
    val currStr = input.substring(index)
    // Get either the const or var which is there.
    val consts = constregex.findAllIn(currStr)
    //print(consts.hasNext)
    if (consts.hasNext){
      var const: String = consts.next()
      index += const.length()
      skipWhiteSpace()
      //println("index at grammarClasses.F")
      //println(const.toInt)
      //println(index)
      var returnVal: F=null
      returnVal = Const(const.toDouble, eulersNum = false)
      if(index <= input.length-1){
        if(input(index) == '^'){
          index+=1
          skipWhiteSpace()
          if(input(index) != '-') {
            val curPossibleVal = grammarClasses.FExp(returnVal, parseF())
            if (returnVal.asInstanceOf[Const].v == 0)
              returnVal
            else
              curPossibleVal
          }else{
            //If we have a negative value put together with the exponent, there's a few things we need to do.
            //first we'll check if this is just a negative number
            //if the character after the current index is a '(', then we really have (-1*grammarClasses.EP)
            //if the character after the current index is a variable letter, then we really have (-1*var)
            val negconstregex: Regex = "^\\-[0-9]+(\\.[0-9]+)?".r
            val currStrVal = input.substring(index)
            val constsVal = negconstregex.findAllIn(currStrVal)
            if(constsVal.hasNext){
              val stringVal = constsVal.next()
              index+= stringVal.length()
              skipWhiteSpace()
              val curPossibleVal = FExp(returnVal,Const(stringVal.toDouble, eulersNum = false))
              if (returnVal.asInstanceOf[Const].v == 0)
                returnVal
              else
                curPossibleVal
            }else if(input(index+1) == '('){
              index+=1
              skipWhiteSpace()
              val curPossibleVal = FExp(returnVal,EP(T(Const(-1, eulersNum = false),Some(TE(T(parseF(), None), '*'))), None))
              if (returnVal.asInstanceOf[Const].v == 0)
                returnVal
              else
                curPossibleVal

            }else{
              //we have a variable letter
              val varString = varregex.findAllIn(currStrVal)
              index += varString.length
              val curPossibleVal = FExp(returnVal,Var(varString.next()))
              if (returnVal.asInstanceOf[Const].v == 0)
                returnVal
              else
                curPossibleVal
            }


          }
        }else{
          returnVal
        }
      }else{
        returnVal
      }
    }else if(input(index) == '('){
      println("starting parse of parenthesized expression")
      index+=1
      val nested_expression = grammarClasses.EP(parseT(), parseETail())
      if (input(index)!= ')'){
        println("Error: Did not close parentheses at index "+index)
      }
      index+=1
      if(index<= input.length-1){
        if(input(index) == '^'){
          index+=1
          skipWhiteSpace()
          if(input(index) != '-') {
            FExp(nested_expression, parseF())
          }else{
            //If we have a negative value put together with the exponent, there's a few things we need to do.
            //first we'll check if this is just a negative number
            //if the character after the current index is a '(', then we really have (-1*grammarClasses.EP)
            //if the character after the current index is a variable letter, then we really have (-1*var)
            val negconstregex: Regex = "^\\-[0-9]+(\\.[0-9]+)?".r
            val currStrVal = input.substring(index)
            val constsVal = negconstregex.findAllIn(currStrVal)
            if(constsVal.hasNext){
              val stringVal = constsVal.next()
              index+= stringVal.length()
              skipWhiteSpace()
              FExp(nested_expression,Const(stringVal.toDouble, eulersNum = false))
            }else if(input(index+1) == '('){
              index+=1
              FExp(nested_expression,EP(T(Const(-1, eulersNum = false),Some(TE(T(parseF(), None), '*'))), None))
            }else{
              //we have a variable letter
              val varString = varregex.findAllIn(currStrVal)
              index += varString.length
              skipWhiteSpace()
              FExp(nested_expression,Var(varString.next()))
            }


          }
        }else{
          nested_expression
        }
      }else{
         nested_expression
      }
    }
    else {
      val vars = varregex.findAllIn(currStr)
      val varname = vars.next()
      index += varname.length()
      skipWhiteSpace()
      //var name might be Sqrt, if it is, progress past it and make an FEXP node
      if(varname.equals("Sqrt") && input(index) == '['){
        return parseSqrt()
      }

      Var(varname)
      if(index<= input.length-1){
        if(input(index) == '^'){
          index+=1
          skipWhiteSpace()
          if(input(index) != '-') {
            grammarClasses.FExp(Var(varname), parseF())
          }else{
            //If we have a negative value put together with the exponent, there's a few things we need to do.
            //first we'll check if this is just a negative number
            //if the character after the current index is a '(', then we really have (-1*grammarClasses.EP)
            //if the character after the current index is a variable letter, then we really have (-1*var)
            val negconstregex: Regex = "^\\-[0-9]+(\\.[0-9]+)?".r
            val currStrVal = input.substring(index)
            val constsVal = negconstregex.findAllIn(currStrVal)
            if(constsVal.hasNext){
              val stringVal = constsVal.next()
              index+= stringVal.length()
              skipWhiteSpace()
              FExp(Var(varname),Const(stringVal.toDouble, eulersNum = false))
            }else if(input(index+1) == '('){
              index+=1
              FExp(Var(varname),EP(T(Const(-1,eulersNum = false),Some(TE(T(parseF(), None), '*'))), None))
            }else{
              //we have a variable letter
              val varString = varregex.findAllIn(currStrVal)
              index += varString.length
              FExp(Var(varname),Var(varString.next()))
            }


          }
        }else{
          Var(varname)
        }
      }else{
        Var(varname)
      }
    }
  }

  def skipWhiteSpace(): Unit = {
    if(index <= input.length-1) {
      while(input(index) == ' '){
        index=index+1
        skipWhiteSpace()

      }
    }
  }

}