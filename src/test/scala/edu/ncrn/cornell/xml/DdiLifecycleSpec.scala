package edu.ncrn.cornell.xml

import Util._
import org.specs2._

import scala.xml.XML


/**
  * @author Brandon Barker
  *         9/8/2016
  */


class DdiLifecycleSpec extends Specification { def is = s2"""

 Testing DDI-Lifecycle 3.2
   Passed ICPSR2079variables.xml path check    $readAndFindDDILC32
 """

  val unwantedPrefixes = List("xsi")

  //TODO: better way to do this based on real namespace lookup in Scala XML?
  def xmlXpathFilter(xpath: String, badList: List[String]) = !badList.exists(x =>
    xpath.contains("/" + x + ":") || xpath.contains("/@" + x + ":")
  )

  val xpathXmlEnumerator = new XpathXmlEnumerator(XML.load(
    this.getClass.getResourceAsStream(
      "/DDILifecycle3dot2/xml/ICPSR2079variables.xml"
    )
  ))

  val entryXsdFile = "/DDILifecycle3dot2/xsd/instance.xsd"
  val xmlTestData = xpathXmlEnumerator.enumSimple
    .filter(x => xmlXpathFilter(x._1, unwantedPrefixes))

  val xmlTestDataNonUniq = toWildCard(xmlTestData.map{x => x._1})

  val readAndFindFromFile = makePairedTester(xmlTestDataNonUniq)
  val readAndFindDDILC32 = readAndFindFromFile(entryXsdFile) must beTrue

}



