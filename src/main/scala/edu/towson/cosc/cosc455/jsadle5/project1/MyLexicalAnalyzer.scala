package edu.towson.cosc.cosc455.jsadle5.project1


class MyLexicalAnalyzer extends LexicalAnalyzer {


  var index : Int = -1
  var nextChar : Char = ' '
  var possibleToken : String = ""

  override def addChar(): Unit = {
    possibleToken += nextChar
  }

  override def getChar(): Unit = {
    index +=1
    if (index < Complier.fileContents.length) {
      nextChar = Complier.fileContents.charAt(index)
    }
  }

  override def getNextToken(): Unit = {
    possibleToken = "" //Reset variables
    nextChar = ' '

    getChar()
    nonSpace()


    if (Constants.ANNOTATIONS.contains(nextChar)) { //Special character state
      possibleToken = processAnnotation()
      if (lookup(possibleToken)) {
        Complier.currentToken = possibleToken
      }
      else {
        println("Lexical error: Illegal token received: '" + possibleToken + "'")
        System.exit(1)
      }
    }
    else if (nextChar.isLetterOrDigit) { //Text state
      Complier.currentToken += textState()
    }
    else if (nextChar.equals('\n')) {
      getNextToken() //Skip and get next token
    }
    else {
      println("Lexical error: Illegal character received: '" + nextChar + "'")
      System.exit(1)
    }
  }

  def lookup(token : String): Boolean = {
    return Constants.ALLCONSTANTS.contains(token)
  }

  def processAnnotation() : String = {

    if (nextChar.equals(Constants.asterisk)) { // start '*'
      addChar()
      getChar()
      if (nextChar.equals(Constants.asterisk)) { // start '**'
        addChar()
        possibleToken += textState()
        getChar()
        if (nextChar.equals(Constants.asterisk)) { // end '*'
          addChar()
          getChar()
          if (nextChar.equals(Constants.asterisk)) {// end '**'
            addChar()
          }
          else {
            println("Lexical error. Illegal character in BOLD: '" + nextChar + "'")
            System.exit(1)
          }
        }
        else {
          println("Lexical error. Illegal character in BOLD: '" + nextChar + "'")
          System.exit(1)
        }
      }
      else { //italics *
        possibleToken += textState()
        getChar()
        if (nextChar.equals(Constants.asterisk)) { //* to end
          addChar()
          getChar()
        }
        else {
          println("Lexical error. Illegal character in ITALICS: '" + nextChar + "'")
          System.exit(1)
        }
      }
    }
    else if (nextChar.equals(Constants.plus)) {
      addChar()
      nextChar = '+'
      while (nextChar.equals((Constants.plus))) {
        possibleToken += textState()
        getChar()
      }
    }
    else if (nextChar.equals(Constants.slash)) {
      addChar()
      possibleToken += textState()
    }
    else if (nextChar.equals(Constants.exclamation)) {
      addChar()
      getChar()
      if (nextChar.equals(Constants.bracket)) {
        addChar()
        possibleToken += textState()
      }
      else {
        println("Lexical error. Illegal character after '!'. Received: '" + nextChar + "'")
        System.exit(1)
      }
    }

    return possibleToken
  }

  def textState() : String = {
    var text : String = ""
    getChar()
    while (nextChar.isLetterOrDigit || nextChar.equals('\n')) {
      text += nextChar
      getChar()
    }

    return text
  }

  def nonSpace() : Unit = {   //Calls get char until a non space character is found
    while (nextChar.equals(' ')) {
      getChar()
    }
  }
}
