package edu.ncrn.cornell.xml

import org.specs2._

import scala.xml.{Node, XML}
import Util._


/**
  * @author Brandon Barker
  *         9/8/2016
  */


class ShipOrderXsdSpec extends Specification { def is = s2"""

 Testing simple ShipOrder XSD
   Testing reading in Russian Doll variant                $readAndFindRD
   Testing reading in Salami Slice variant                $readAndFindSS
   Testing reading in Venetian Blind variant              $readAndFindVB
                                 """


//  No non-terminal paths returned               ${nonTerminalCount == 0}
//  No empty nodes returned by default           ${emptyLeafCount == 0}
//  No "#PCDATA" in XPaths                       ${pathsWithPcdata == 0}
//  Determiend correct number of leaf indices    $indexCheck
//  Large XML file is working!                   $readFile

  val unwantedPrefixes = List("xsi")

  //TODO: better way to do this based on real namespace lookup in Scala XML?
  def xmlXpathFilter(xpath: String, badList: List[String]) = !badList.exists(x =>
    xpath.contains("/" + x + ":") || xpath.contains("/@" + x + ":")
  )

  val xpathXmlEnumerator = new XpathXmlEnumerator {}
  val xpathXsdEnumerator = new XpathXsdEnumerator {}

  val xsdRussianDollFile = "/shiporder.xsd"
  val xsdSalamiSliceFile = "/shiporder2.xsd"
  val xsdVenetianBlindFile = "/shiporder3.xsd"

  val xmlTestData = xpathXmlEnumerator.enumerate(XML.load(
    this.getClass.getResourceAsStream("/shiporder.xml")
  )).filter(x => xmlXpathFilter(x._1, unwantedPrefixes))

  val xmlTestDataNonUniq = toWildCard(xmlTestData.map{x => x._1})

  val readAndFindRD = readAndFindFromFile(xsdRussianDollFile) must beTrue
  val readAndFindSS = readAndFindFromFile(xsdSalamiSliceFile) must beTrue
  val readAndFindVB = readAndFindFromFile(xsdVenetianBlindFile) must beTrue

  def readAndFindFromFile(fileName: String): Boolean = {
    val xsdXml = XML.load(this.getClass.getResourceAsStream(fileName))
    println(s"!!!! Initiating enumeration of $fileName !!!!")
    val xsdXmlData = xpathXsdEnumerator.enumerate(xsdXml)
    xsdXmlData.foreach(x => println(x)) // DEBUG
    val missingXPaths = xmlTestDataNonUniq.toSet -- xsdXmlData.map(_._1).toSet
    missingXPaths.foreach(x => println(s"missing: $x"))
    //
    // Check that we are getting multiple known nodes and a known attribute path
    def readSimple = xsdXmlData.size > 2
    def foundElem = try {
      xsdXmlData.filter(path =>
        path._1 == "/shiporder/orderperson"
      ).head._2 == "xs:string"
    } catch {
      case ex: NoSuchElementException => false
    }
    readSimple && foundElem && missingXPaths.isEmpty
  }





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



