package edu.ncrn.cornell.xml

import scala.xml.XML
import org.specs2._
import XpathEnumerator._


/**
  * @author Brandon Barker
  *         9/8/2016
  */


class ReadXmlSpec extends Specification { def is = s2"""

 Testing reading in XML
   Small XML snippet is working                 ${readSimple && foundAttrib must beTrue}
   No non-terminal paths returned               ${nonTerminalCount === 0}
   No "#PCDATA" in XPaths                       ${pathsWithPcdata === 0}
   Determiend correct number of leaf indices    ${indexCheck must beTrue}
   No empty nodes returned by default           ${emptyLeafCount === 0}
   No back-to-back indexes (e.g. /[1][2])       ${pathsWithMultidimIndexCount must_===  0}
   Large XML file is working!                   ${readFile  must beTrue}
                                 """


  //TODO: add xpath indexing checks



  //
  // *** Checks based on small XML snippet ***
  //
  val basicXml = <div class="content"><a><b><b><c>123</c></b><c><d>ABC</d></c></b></a><p>
    <q>hello</q><q>,</q></p><r><p>world</p></r><s></s></div>
  //
  val xpathEnumerator = new XpathXmlEnumerator(basicXml.toList)
  val xpathData = xpathEnumerator.enumSimple
  //
  // Check that we are getting multiple known nodes and a known attribute path
  def readSimple = xpathData.size > 2
  def foundAttrib = xpathData.find(path => path._1 === "/div/@class")
    .fold(false)(_._2 === "content" )
  //
  // Scan for error cases
  val nonTerminalCount = xpathData.map(x => x._1).count(x => x.last === '/')
  val pathsWithPcdata = xpathData.map(x => x._1).count(x => x.contains("#PCDATA"))
  //
  // Check that indexing is working
  val divPQnodes = xpathData.map(x => x._1).filter(x => x.startsWith("/div/p/q"))
  val divPQ12count = divPQnodes.count(x => x.endsWith("[1]") || x.endsWith("[2]"))
  val indexCheck = divPQ12count === 2 && divPQ12count === divPQnodes.size

  val smallXmlFile = "/shiporder.xml"
  val smallXml = XML.load(this.getClass.getResourceAsStream(smallXmlFile)).toList
  val smallXpathEnumerator = new XpathXmlEnumerator(smallXml)
  val smallXpathData = smallXpathEnumerator.enumSimple
  smallXpathData.foreach{x => println(x)} // DEBUG

  //
  // Checks based on reading larger XML files
  //
  val largeXmlFile = "/ssbv602.xml"
  val largeXml = XML.load(this.getClass.getResourceAsStream(largeXmlFile)).toList
  val largeXpathEnumerator = new XpathXmlEnumerator(largeXml)
  val largeXpathData = largeXpathEnumerator.enumSimple
  def readFile = largeXpathData.size > xpathData.size

  val emptyLeafCount =
    (xpathData ::: smallXpathData ::: largeXpathData).count(x => x._2 === "")

  // Note, that docs containing e.g., XHTML, can have multidimensional arrays
  // due to something like the following:
  // <p>First para</p><p> Hi <bob> . </p>
  // In this case, we would have /p[2][1] = Hi, /p[2][2] = .
  //TODO: determine if that is valid XPath notation, and write a simple test
  val pathsWithMultidimIndex =
    (xpathData ::: smallXpathData).filter(x => x._1.contains("]["))

  val pathsWithMultidimIndexCount = pathsWithMultidimIndex.size



}



