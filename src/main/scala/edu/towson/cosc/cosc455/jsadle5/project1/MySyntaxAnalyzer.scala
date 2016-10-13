package edu.towson.cosc.cosc455.jsadle5.project1


class MySyntaxAnalyzer extends SyntaxAnalyzer {

  var parseTree = new scala.collection.mutable.Stack[String]
  override def gittex() = {
      if (Complier.currentToken.equalsIgnoreCase(Constants.DOCB)) {
        parseTree.push(Complier.currentToken)
        Complier.lex.getNextToken()
        variableDefine()
        title()
        body()
        if(Complier.currentToken.equalsIgnoreCase(Constants.DOCE)){
          parseTree.push(Complier.currentToken)
        }
        else {
          println("Syntax error. Expected: '" + Constants.DOCE + "'. Received: '" + Complier.currentToken + "'")
          System.exit(1)
        }
      }
      else {
        println("Syntax error. Expected '" + Constants.DOCB + "'. Received '" + Complier.currentToken + "'")
        System.exit(1)
      }
  }

  override def title(): Unit = ???

  override def body(): Unit = ???

  override def paragraph(): Unit = ???

  override def innerText(): Unit = ???

  override def heading(): Unit = ???

  override def variableDefine(): Unit = ???

  override def variableUse(): Unit = ???

  override def bold(): Unit = ???

  override def italics(): Unit = ???

  override def listItem(): Unit = ???

  override def innerItem(): Unit = ???

  override def link(): Unit = ???

  override def image(): Unit = ???

  override def newline(): Unit = ???
}
