package edu.towson.cosc.cosc455.jsadle5.project1


class MyLexicalAnalyzer extends LexicalAnalyzer {


  var index : Int = -1
  var nextChar : Char = ' '
  var possibleToken : String = ""

  override def addChar(): Unit = { //Adds character to the potential token
    possibleToken += nextChar
  }

  override def getChar(): Unit = { //Gets next character from file input
    index +=1
    if (index < Complier.fileContents.length) {
      nextChar = Complier.fileContents.charAt(index)
    }
  }

  override def getNextToken(): Unit = { //Forms next token
    possibleToken = "" //Reset token

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
      addChar()
      possibleToken += textState()
      if (nextChar.equals(Constants.brackete)) { //Will decrement index so ending bracket is added
        index -= 1
      }
      Complier.currentToken = possibleToken
    }
    else if (!isCR_LF()) {
      getNextToken() //Skip and get next token
    }
    else {
      println("Lexical error: Illegal character received: '" + nextChar + "'")
      System.exit(1)
    }
  }

  def lookup(token : String): Boolean = { //Returns true if the token is legal
    return Constants.ALLCONSTANTS.contains(token)
  }

  def processAnnotation() : String = { //Processes the annotation characters

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
      possibleToken += textState()

    }
    else if (nextChar.equals(Constants.slash)) {
      addChar()
      possibleToken += textState()
      if (nextChar.equals(Constants.bracket)) { //Will add ending bracket if required
        addChar()
      }
    }
    else if (nextChar.equals((Constants.pound))) {
      addChar()
      possibleToken += textState()
    }
    else if (nextChar.equals(Constants.exclamation)) {
      addChar()
      getChar()
      if (nextChar.equals(Constants.bracket)) {
        addChar()
        //possibleToken += textState()
      }
      else {
        println("Lexical error. Illegal character after '!'. Received: '" + nextChar + "'")
        System.exit(1)
      }
    }
    else if (nextChar.equals(Constants.brackete)) {
      addChar()
    }
    else if (nextChar.equals(Constants.parenE)) {
      addChar()
    }
    else if (nextChar.equals(Constants.parenB)) {
      addChar()
    }

    return possibleToken
  }

  def textState() : String = { //Reads in text until end of word, line or token
    var text : String = ""
    getChar()
    while (!nextChar.isSpaceChar && !isCR_LF() && !Constants.ANNOTATIONS.contains(nextChar)) {
      text += nextChar
      getChar()
    }

    return text
  }

  def isCR_LF() : Boolean = { //Returns true if nextChar is a Carriage return or Line feed
    return (nextChar.equals('\n') || nextChar.equals('\r'))
  }

  def nonSpace() : Unit = {   //Calls get char until a non space character is found
    while (nextChar.equals(' ') || isCR_LF()) {
      getChar()
    }
  }
}
