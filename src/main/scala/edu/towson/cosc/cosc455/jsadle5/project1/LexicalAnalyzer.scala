package edu.towson.cosc.cosc455.jsadle5.project1


trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Unit
  def getNextToken() : Unit
  def lookup(token : String) : Boolean
}
