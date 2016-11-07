package edu.towson.cosc.cosc455.jsadle5.project1

import scala.collection.mutable


class MySemanticAnalyzer {

  var newStack = new scala.collection.mutable.Stack[String]


  def checkSemantics() : Unit = { //Checks to make sure all variables are valid
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


  def convertToHTML()  = { //Uses the newStack to convert to HTML5
    import java.io._
    val file = new PrintWriter(new File("outputFile.html"))

    while(!newStack.isEmpty) {
      var s : String = newStack.pop()
      s match {
        case Constants.DOCB => file.append("<html>\n")
        case Constants.DOCE => file.append("\n</html>")
        case Constants.TITLEB =>
          file.append("<head>\n<title> ")
          s = newStack.pop()
          while(!s.equalsIgnoreCase(Constants.BRACKETE)) {
            file.append(s + " ")
            s = newStack.pop()
          }
          file.append(" </title>\n</head>\n")

        case Constants.HEADING =>
          file.append("<h1> ")
          s = newStack.pop()
          while (!Constants.ALLCONSTANTS.contains(s)) {
            file.append(s + " ")
            s = newStack.pop()
          }
          newStack.push(s) //Don't skip '\PARAB'
          file.append(" </h1>\n")

        case Constants.PARAB =>
          file.append("<p> ")
          staticVar.currentScope = 1
        case Constants.PARAE => file.append(" </p>\n")
          staticVar.currentScope = 0
        case Constants.LINKB =>
          var linkText : String = ""
          var url : String = ""
          s = newStack.pop()
          while (!s.equalsIgnoreCase(Constants.BRACKETE)) {
            linkText = linkText + s + " "
            s = newStack.pop()
          }
          newStack.pop()
          url += newStack.pop()
          newStack.pop()

          file.append("<a href=\"" + url + "\">" + linkText + "</a> ")

        case Constants.LISTITEM =>
          file.append("\n<li> ")

          s = newStack.pop()
          if(s.contains("\n")) {
            file.append(s + " </li>")
          }
          else if (s.equalsIgnoreCase(Constants.USEB)){
            newStack.push(s) //Push back onto stack so it doesn't get skipped
          }
          else {
            if (!s.equalsIgnoreCase(Constants.USEB)) {
              file.append(s + " ")
              s = newStack.top
              if (s.contains("\n")) {
                s = newStack.pop()
                file.append(s + " </li>")
              }
            }
          }


        case Constants.NEWLINE => file.append("<br>\n")

        case Constants.IMAGEB =>
          var linkText : String = ""
          var url : String = ""
          s = newStack.pop()
          while (!s.equalsIgnoreCase(Constants.BRACKETE)) {
            linkText = linkText + s + " "
            s = newStack.pop()
          }
          newStack.pop()
          url += newStack.pop()
          newStack.pop()

          file.append("<img src=\"" + url + "\" alt=" + linkText + "\">")

        case Constants.BOLD =>
          var innerText : String = ""

          s = newStack.pop()
          while (!s.equalsIgnoreCase(Constants.BOLD)) {
            innerText += s
            s = newStack.pop()
          }
          file.append("<b> " + innerText + " </b>")

        case Constants.ITALICS =>
          var innerText : String = ""

          s = newStack.pop()
          while (!s.equalsIgnoreCase(Constants.ITALICS)) {
            innerText += s
            s = newStack.pop()
          }
          file.append("<i> " + innerText + " </i>")

        case Constants.DEFB =>
          staticVar.varName.enqueue(newStack.pop()) //Store name
          newStack.pop() //Ignore '='
          staticVar.varValue.enqueue(newStack.pop()) //Store variable value
          newStack.pop() //Ignore ']'

        case Constants.USEB =>
          var useVar : String = newStack.pop()

          file.append(staticVar.varValue(staticVar.varName.indexOf(useVar, staticVar.currentScope)) + " ") //Pulls var value

          newStack.pop() //Ignore ']'

        case _ => file.append(s + " ")
      }

    }

    file.close()
    openHTMLFileInBrowser("outputFile.html")

  }

  import java.awt.Desktop
  import java.io.{File, IOException}

  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }





}


object staticVar { //Made static for recursive and debugging purposes
  var foundVar = new scala.collection.mutable.Stack[String] //Holds all defined variables
  var varName = new scala.collection.mutable.Queue[String]
  var varValue = new scala.collection.mutable.Queue[String] //Parallel stacks
  var currentScope : Int = 0 //Determines the scope of variables
                             // 0 -> Outer block
                             // 1 -> Inner block
}