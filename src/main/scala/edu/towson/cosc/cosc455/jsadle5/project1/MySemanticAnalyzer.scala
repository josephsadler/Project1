package edu.towson.cosc.cosc455.jsadle5.project1

import scala.collection.mutable


class MySemanticAnalyzer {

  var newStack = new scala.collection.mutable.Stack[String]


  def checkSemantics() : Unit = {
    var tempString : String = ""
    while (!tempString.equalsIgnoreCase(Constants.DOCB) && !Complier.syntax.parseTree.isEmpty) {
      tempString = Complier.syntax.parseTree.pop()
      tempString match {
        case Constants.DOCE => newStack.push(tempString)
        case Constants.DOCB => newStack.push(tempString)
        case Constants.DEFB =>
          staticVar.foundVar.push(newStack.top)
          newStack.push(tempString)

        case Constants.USEB =>
          var varName : String = newStack.top
          newStack.push(tempString)
          while(!staticVar.foundVar.contains(varName)) {
            if (Complier.syntax.parseTree.isEmpty && !staticVar.foundVar.contains(varName)) {
              println("Static semantic error. Variable '" + varName + "' is not defined")
              System.exit(1)
            }
            checkSemantics()
          }
        case _ => newStack.push(tempString)
      }
    }
    if (tempString.equalsIgnoreCase(Constants.DOCB) && !newStack.top.equalsIgnoreCase(Constants.DOCB)) {
      newStack.push(tempString) //Push '\BEGIN'
    }
  }
}

object staticVar {
  var foundVar = new scala.collection.mutable.Stack[String] //Holds all defined variables
}