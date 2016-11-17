package edu.ncrn.cornell.xml

import Util._
import org.specs2._

import scala.xml.XML


/**
  * @author Brandon Barker
  *         9/8/2016
  */


class DdiCodebookSpec extends Specification { def is = s2"""

 Testing DDI-Codebook 2.5
   Passed 4245.xml path check       $readAndFindDDILC32
 """

  val unwantedPrefixes = List("xsi")

  //TODO: better way to do this based on real namespace lookup in Scala XML?
  def xmlXpathFilter(xpath: String, badList: List[String]) = !badList.exists(x =>
    xpath.contains("/" + x + ":") || xpath.contains("/@" + x + ":")
  )

  val xpathXmlEnumerator = new XpathXmlEnumerator(XML.load(
    this.getClass.getResourceAsStream("/DDICodebook2dot5/xml/4245.xml")
  ))

  val entryXsdFile = "/DDICodebook2dot5/xsd/codebook.xsd"
  val xmlTestData = xpathXmlEnumerator.enumSimple
    .filter(x => xmlXpathFilter(x._1, unwantedPrefixes))

  val xmlTestDataNonUniq = toWildCard(xmlTestData.map{x => x._1})

  val readAndFindFromFile = makePairedTester(xmlTestDataNonUniq)
  val readAndFindDDILC32 = readAndFindFromFile(entryXsdFile) must beTrue

}



