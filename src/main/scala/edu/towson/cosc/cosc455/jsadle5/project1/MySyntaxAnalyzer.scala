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

  override def title(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase(Constants.TITLEB)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      plainText()
      if (Complier.currentToken.equalsIgnoreCase(Constants.BRACKETE)) {
        parseTree.push(Complier.currentToken)
        Complier.lex.getNextToken()
      }
      else {
        println("Syntax error. Expected '" + Constants.BRACKETE + "'. Received '" + Complier.currentToken + "'")
        System.exit(1)
      }
    }
    else {
      println("Syntax error. Expected: '" + Constants.TITLEB + "'. Received: '" + Complier.currentToken + "'")
      System.exit(1)
    }

  }

  override def body(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase(Constants.PARAB)){
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      paragraph()
      body()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.NEWLINE)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      newline()
      body()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
    else {
      innerText()
      body()
    }
  }

  override def paragraph(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase(Constants.DEFB)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      variableDefine()
    }
    innerText()

    if (Complier.currentToken.equalsIgnoreCase(Constants.PARAE)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
    else {
      println("Syntax error. Expected: '" + Constants.PARAE + "'. Received: '" + Complier.currentToken + "'")
      System.exit(1)
    }

  }

  override def innerText(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase(Constants.USEB)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      variableUse()
      innerText()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.HEADING)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      heading()
      innerText()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.BOLD)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      bold()
      innerText()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.ITALICS)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      italics()
      innerText()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.LISTITEM)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      listItem()
      innerText()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.IMAGEB)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      image()
      innerText()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.LINKB)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      link()
      innerText()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.NEWLINE)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      newline()
      innerText()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
    else {
      if (Constants.ALLCONSTANTS.contains(Complier.currentToken)) { //Syntax error
        println("Syntax error. Cannot include: '" + Complier.currentToken + "' in the inner text")
      }
      else { //Text
        parseTree.push(Complier.currentToken)
        Complier.lex.getNextToken()
        innerText()
      }
    }
  }

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

  def plainText(): Unit = ???
}
