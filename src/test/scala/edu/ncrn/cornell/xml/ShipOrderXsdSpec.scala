package edu.ncrn.cornell.xml

import org.specs2._

import scala.xml.XML


/**
  * @author Brandon Barker
  *         9/8/2016
  */


class ShipOrderXsdSpec extends Specification { def is = s2"""

 Testing simple ShipOrder XSD
   Testing reading in Russian Doll variant                ${readSimple && foundElem}
   Testing reading in Salami Slice variant                ${/*readSimple2 && foundElem2*/ false}
   Testing reading in Venetian Blind variant              ${/*readSimple3 && foundElem3*/ false}
                                 """


//  No non-terminal paths returned               ${nonTerminalCount == 0}
//  No empty nodes returned by default           ${emptyLeafCount == 0}
//  No "#PCDATA" in XPaths                       ${pathsWithPcdata == 0}
//  Determiend correct number of leaf indices    $indexCheck
//  Large XML file is working!                   $readFile

  val xpathEnumerator = new XpathXsdEnumerator {}

  val xsdRussianDollFile = "/shiporder.xsd"
  val xsdRussianDoll = XML.load(this.getClass.getResourceAsStream(xsdRussianDollFile))
  val russianDollData = xpathEnumerator.enumerate(xsdRussianDoll)
  //
//  val xsdSalamiSliceFile = "/shiporder2.xsd"
//  val xsdSalamiSlice = XML.load(this.getClass.getResourceAsStream(xsdSalamiSliceFile))
//  val SalamiSliceData = xpathEnumerator.enumerate(xsdSalamiSlice)
  //
//  val xsdVenetianBlindFile = "/shiporder3.xsd"
//  val xsdVenetianBlind = XML.load(this.getClass.getResourceAsStream(xsdVenetianBlindFile))
//  val VenetianBlindData = xpathEnumerator.enumerate(xsdVenetianBlind)

  //
  // Check that we are getting multiple known nodes and a known attribute path
  def readSimple = russianDollData.size > 2
  def foundElem = russianDollData.filter(path =>
    path._1 == "/shiporder/orderperson"
  ).head._2 == ""
  //
//  def readSimple2 = SalamiSliceData.size > 2
//  def foundElem2 = SalamiSliceData.filter(path =>
//    path._1 == "/shiporder/orderperson"
//  ).head._2 == ""
//  //
//  def readSimple3 = VenetianBlindData.size > 2
//  def foundElem3 = VenetianBlindData.filter(path =>
//    path._1 == "/shiporder/orderperson"
//  ).head._2 == ""




  //
//  // Scan for error cases
//  val nonTerminalCount = xpathData.map(x => x._1).count(x => x.last == '/')
//  val emptyLeafCount = xpathData.map(x => x._2).count(x => x == "")
//  val pathsWithPcdata = xpathData.map(x => x._1).count(x => x.contains("#PCDATA"))
//  //
//  // Check that indexing is working
//  val divPQnodes = xpathData.map(x => x._1).filter(x => x.startsWith("/div/p/q"))
//  val divPQ12count = divPQnodes.count(x => x.endsWith("[1]") || x.endsWith("[2]"))
//  val indexCheck = divPQ12count == 2 && divPQ12count == divPQnodes.size
//
//  //
//  // Checks based on reading larger XML files
//  //
//  val largeXmlFile = "/ssbv602.xml"
//  val largeXml = XML.load(this.getClass.getResourceAsStream(largeXmlFile))
//  val largeXpathData = xpathEnumerator.enumerate(largeXml)
//  def readFile = largeXpathData.size > xpathData.size
}



