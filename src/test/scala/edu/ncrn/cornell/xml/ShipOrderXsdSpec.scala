package edu.ncrn.cornell.xml

import org.specs2._

import scala.xml.XML


/**
  * @author Brandon Barker
  *         9/8/2016
  */


class ShipOrderXsdSpec extends Specification { def is = s2"""

 Testing reading in XML
   Small XML snippet is working                 ${readSimple && foundAttrib}
   No non-terminal paths returned               ${nonTerminalCount == 0}
   No empty nodes returned by default           ${emptyLeafCount == 0}
   No "#PCDATA" in XPaths                       ${pathsWithPcdata == 0}
   Determiend correct number of leaf indices    $indexCheck
   Large XML file is working!                   $readFile
                                 """


  //TODO: add xpath indexing checks



  //
  // *** Checks based on small XML snippet ***
  //
  val basicXml = <div class="content"><a><b><b><c>123</c></b><c><d>ABC</d></c></b></a><p>
    <q>hello</q><q>,</q></p><r><p>world</p></r><s></s></div>
  //
  val xpathEnumerator = new XpathXmlEnumerator {}
  val xpathData = xpathEnumerator.enumerate(basicXml)
  //
  // Check that we are getting multiple known nodes and a known attribute path
  def readSimple = xpathData.size > 2
  def foundAttrib = xpathData.filter(path => path._1 == "/div/@class").head._2 == "content"
  //
  // Scan for error cases
  val nonTerminalCount = xpathData.map(x => x._1).count(x => x.last == '/')
  val emptyLeafCount = xpathData.map(x => x._2).count(x => x == "")
  val pathsWithPcdata = xpathData.map(x => x._1).count(x => x.contains("#PCDATA"))
  //
  // Check that indexing is working
  val divPQnodes = xpathData.map(x => x._1).filter(x => x.startsWith("/div/p/q"))
  val divPQ12count = divPQnodes.count(x => x.endsWith("[1]") || x.endsWith("[2]"))
  val indexCheck = divPQ12count == 2 && divPQ12count == divPQnodes.size

  //
  // Checks based on reading larger XML files
  //
  val largeXmlFile = "/ssbv602.xml"
  val largeXml = XML.load(this.getClass.getResourceAsStream(largeXmlFile))
  val largeXpathData = xpathEnumerator.enumerate(largeXml)
  def readFile = largeXpathData.size > xpathData.size
}



