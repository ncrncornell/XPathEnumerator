package edu.ncrn.cornell.xml

import scala.xml.XML
import org.specs2._
import XPathEnumerator._


/**
  * @author Brandon Barker
  *         9/8/2016
  */


class ReadXMLSpec extends Specification { def is = s2"""

 Testing reading in XML
   Small XML snippet is working                 ${readSimple && foundAttrib}
   Large XML file is working!                   $readFile
                                 """


  val basicXml = <div class="content"><a></a><p><q>hello</q></p><r><p>world</p></r><s></s></div>
  val largeXmlFile = "/ssbv602.xml"
  val largeXml = XML.load(this.getClass.getResourceAsStream(largeXmlFile))

  val xpathEnumerator = new XPathEnumerator {}
  val xpathData = xpathEnumerator.uniqueXpaths(pathifyNodes(basicXml))
  def readSimple = xpathData.size > 2
  def foundAttrib = xpathData.filter(path => path._1 == "/div/@class").head._2 == "content"

  val largeXpathData = xpathEnumerator.uniqueXpaths(pathifyNodes(largeXml))
  def readFile = largeXpathData.size > xpathData.size
}



