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
        System.exit(1)
      }
      else { //Text
        parseTree.push(Complier.currentToken)
        Complier.lex.getNextToken()
        innerText()
      }
    }
  }

  override def heading(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase(Constants.HEADING)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      plainText()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
  }

  override def variableDefine(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase(Constants.DEFB)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      plainText()
      if (Complier.currentToken.equalsIgnoreCase(Constants.EQUALS)) {
        parseTree.push(Complier.currentToken)
        Complier.lex.getNextToken()
        plainText()
        if (Complier.currentToken.equalsIgnoreCase(Constants.BRACKETE)) {
          parseTree.push(Complier.currentToken)
          Complier.lex.getNextToken()
        }
        else {
          println("Syntax error. Expected: '" + Constants.BRACKETE + "'. Received: '" + Complier.currentToken + "'")
          System.exit(1)
        }
      }
      else {
        println("Syntax error. Illegal token in variable definition. Expected: '" + Constants.EQUALS + "'. Received: '" + Complier.currentToken + "'")
        System.exit(1)
      }
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }

  }

  override def variableUse(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase(Constants.USEB)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      plainText()
      if(Complier.currentToken.equalsIgnoreCase(Constants.BRACKETE)) {
        parseTree.push(Complier.currentToken)
        Complier.lex.getNextToken()
      }
      else {
        println("Syntax error. Illegal token in variable definition. Expected: '" + Constants.BRACKETE + "'. Received: '" + Complier.currentToken + "'")
        System.exit(1)
      }
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
  }

  override def bold(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase((Constants.BOLD))) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      plainText()
      if (Complier.currentToken.equalsIgnoreCase(Constants.BOLD)) {
        parseTree.push(Complier.currentToken)
        Complier.lex.getNextToken()
      }
      else {
        println("Syntax error. Illegal token in variable definition. Expected: '" + Constants.BOLD + "'. Received: '" + Complier.currentToken + "'")
        System.exit(1)
      }
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
  }

  override def italics(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase((Constants.ITALICS))) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      plainText()
      if (Complier.currentToken.equalsIgnoreCase(Constants.ITALICS)) {
        parseTree.push(Complier.currentToken)
        Complier.lex.getNextToken()
      }
      else {
        println("Syntax error. Illegal token in variable definition. Expected: '" + Constants.ITALICS + "'. Received: '" + Complier.currentToken + "'")
        System.exit(1)
      }
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
  }

  override def listItem(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase(Constants.LISTITEM)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      innerItem()
      listItem()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
  }

  override def innerItem(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase(Constants.USEB)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      variableUse()
      innerItem()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.BOLD)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      bold()
      innerItem()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.ITALICS)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      italics()
      innerItem()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.LINKB)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      link()
      innerItem()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
    else { //Text
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      innerItem()
    }
  }

  override def link(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase(Constants.LINKB)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      plainText()
      if (Complier.currentToken.equalsIgnoreCase(Constants.BRACKETE)) {
        parseTree.push(Complier.currentToken)
        Complier.lex.getNextToken()
        if (Complier.currentToken.equalsIgnoreCase(Constants.ADDRESSB)) {
          parseTree.push(Complier.currentToken)
          Complier.lex.getNextToken()
          plainText()
          if (Complier.currentToken.equalsIgnoreCase(Constants.ADDRESSE)) {
            parseTree.push(Complier.currentToken)
            Complier.lex.getNextToken()
          }
          else {
            println("Syntax error. Expected: '" + Constants.ADDRESSE + "'. Received: '" + Complier.currentToken + "'")
            System.exit(1)
          }
        }
        else {
          println("Syntax error. Expected: '" + Constants.ADDRESSB + "'. Received: '" + Complier.currentToken + "'")
          System.exit(1)
        }
      }
      else {
        println("Syntax error. Expected: '" + Constants.BRACKETE + "'. Received: '" + Complier.currentToken + "'")
        System.exit(1)
      }
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
  }

  override def image(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase(Constants.IMAGEB)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      plainText()
      if (Complier.currentToken.equalsIgnoreCase(Constants.BRACKETE)) {
        parseTree.push(Complier.currentToken)
        Complier.lex.getNextToken()
        if (Complier.currentToken.equalsIgnoreCase(Constants.ADDRESSB)) {
          parseTree.push(Complier.currentToken)
          Complier.lex.getNextToken()
          plainText()
          if (Complier.currentToken.equalsIgnoreCase(Constants.ADDRESSE)) {
            parseTree.push(Complier.currentToken)
            Complier.lex.getNextToken()
          }
          else {
            println("Syntax error. Expected: '" + Constants.ADDRESSE + "'. Received: '" + Complier.currentToken + "'")
            System.exit(1)
          }
        }
        else {
          println("Syntax error. Expected: '" + Constants.ADDRESSB + "'. Received: '" + Complier.currentToken + "'")
          System.exit(1)
        }
      }
      else {
        println("Syntax error. Expected: '" + Constants.BRACKETE + "'. Received: '" + Complier.currentToken + "'")
        System.exit(1)
      }
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
  }

  override def newline(): Unit = {
    if (Complier.currentToken.equalsIgnoreCase(Constants.NEWLINE)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }
  }

  def plainText(): Unit = {
    if (Complier.currentToken.length == Complier.currentToken.filter(_.isLetterOrDigit).length) { //If all chars are numbers or letters
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
      plainText()
    }
    else if (Complier.currentToken.equalsIgnoreCase(Constants.EMPTY)) {
      parseTree.push(Complier.currentToken)
      Complier.lex.getNextToken()
    }

  }
}
