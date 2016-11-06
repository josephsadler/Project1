package edu.towson.cosc.cosc455.jsadle5.project1



object Constants {
  val DOCB : String = "\\BEGIN"
  val DOCE : String = "\\END"
  val TITLEB : String = "\\TITLE["
  val BRACKETE: String = "]"
  val PARAB : String = "\\PARAB"
  val PARAE : String = "\\PARAE"
  val NEWLINE : String = "\\"
  val EMPTY : String = ""
  val BOLD : String = "**"
  val ITALICS : String = "*"
  val LISTITEM : String = "+"
  val LINKB : String = "["
  val ADDRESSB : String = "("
  val ADDRESSE : String = ")"
  val IMAGEB : String = "!["
  val DEFB : String = "\\DEF["
  val EQUALS : String = "="
  val USEB : String = "\\USE["
  val HEADING : String = "#"
  val ALLCONSTANTS : Array[String] = Array(DOCB, DOCE, TITLEB, BRACKETE, PARAB, PARAE, NEWLINE, EMPTY, BOLD, ITALICS,
                                           LISTITEM, LINKB, ADDRESSB, ADDRESSE, IMAGEB, DEFB, EQUALS, USEB, HEADING)


  val asterisk : Char = '*'
  val plus : Char = '+'
  val equals : Char = '='
  val slash : Char = '\\'
  val exclamation :Char = '!'
  val pound : Char = '#'
  val bracket : Char = '['
  val brackete : Char = ']'
  val parenB : Char = '('
  val parenE : Char = ')'
  val ANNOTATIONS : Array[Char] = Array(asterisk, plus, equals, slash, exclamation, pound, bracket, brackete, parenB, parenE)
}
