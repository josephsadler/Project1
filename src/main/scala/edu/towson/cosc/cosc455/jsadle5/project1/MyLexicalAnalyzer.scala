package edu.towson.cosc.cosc455.jsadle5.project1


class MyLexicalAnalyzer extends LexicalAnalyzer {
  override def addChar(): Unit = ???


  var index : Int = -1
  override def getChar(): Char = {
    index +=1
    return Complier.fileContents.charAt(index)
  }

  override def getNextToken(): Unit = {
    var c : Char = getChar()

    while (c.equals(' ')) {
      c = getChar()
    }
    if (Constants.ANNOTATIONS.contains(c)) {
      var token : String = processAnnotation(c)
      if (lookup(token)) {
        Complier.currentToken = token
      }
      else {
        printf("Lexical error: Illegal token received: '" + token + "'")
        System.exit(1)
      }
    }
    else if (c.isLetterOrDigit) {
      Complier.currentToken = textState()
    }
    else {
      printf("Lexical error: Illegal character received: '" + c + "'")
    }
  }

  def lookup(token : String): Boolean = {
    return Constants.ALLCONSTANTS.contains(token)
  }

  def processAnnotation(c : Char) : String = {
    var token : String = ""
    token += c
    var nextChar : Char = ' '

    if (c.equals(Constants.asterisk)) {
      nextChar = getChar()
      token += nextChar
      if (nextChar.equals(Constants.asterisk)) { //bold **
        token += textState()
        nextChar = getChar()
        if (nextChar.equals(Constants.asterisk)) { //first * to end
          token += nextChar
          nextChar = getChar()
          if (nextChar.equals(Constants.asterisk)) {//second * to end
            token += nextChar
          }
          else {
            printf("Lexical error. Illegal character in BOLD: '" + nextChar + "'")
            System.exit(1)
          }
        }
        else {
          printf("Lexical error. Illegal character in BOLD: '" + nextChar + "'")
          System.exit(1)
        }
      }
      else { //italics *
        token += textState()
        nextChar = getChar()
        if (nextChar.equals(Constants.asterisk)) { //only * to end
          token += nextChar
          nextChar = getChar()
        }
        else {
          printf("Lexical error. Illegal character in ITALICS: '" + nextChar + "'")
          System.exit(1)
        }
      }
    }
    else if (c.equals(Constants.plus)) {
      token += textState()

    }
    else if (c.equals(Constants.slash)) {

    }
    else if (c.equals(Constants.exclamation)) {

    }



    return token
  }

  def textState() : String = {
    var text : String = ""
    var c : Char = getChar()
    while (c.isLetterOrDigit) {
      text += c
    }

    return text
  }
}
