package edu.towson.cosc.cosc455.jsadle5.project1


object Complier {

  var fileContents : String = ""
  var currentToken : String = ""
  var lex = new MyLexicalAnalyzer
  var syntax = new MySyntaxAnalyzer

  def main(args : Array[String]) = {
    //check usage (# of args, file extension)
    checkFile(args)

    readFile(args(0))
    println(fileContents)


    lex.getNextToken()

    syntax.gittex()

    //on return, there is a parse tree
    //semantic analyzer does stuff
  }

  def checkFile(args : Array[String]) = {
    if (args.length > 1) {
      println("Usage error, too many arguments")
      System.exit(1);
    }
    else if (args.length == 0) {
      println("Usage error, no arguments provided")
      System.exit(1)
    }

    if (!args(0).endsWith(".mkd")) {
      println("Usage error, wrong file extension")
      System.exit(1);
    }
  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }
}
