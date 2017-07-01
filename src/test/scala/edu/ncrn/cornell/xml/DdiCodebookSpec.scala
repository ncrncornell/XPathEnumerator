package edu.ncrn.cornell.xml

import edu.ncrn.cornell.xml.Util._
import edu.ncrn.cornell.xml.XpathEnumerator.{NodeFilterGeneric, NodeFilters}
import org.specs2._

import scala.xml.{Node, XML}


/**
  * @author Brandon Barker
  *         9/8/2016
  */


class DdiCodebookSpec extends Specification { def is = s2"""

 Testing DDI-Codebook 2.5
   Passed 4245.xml (/codebook only) path check   $readAndFindCodebook |
 """

  //    Passed 4245.xml path check                    $readAndFindDDILC32

  val unwantedPrefixes = List("xsi")

  //TODO: better way to do this based on real namespace lookup in Scala XML?
  def xmlXpathFilter(xpath: String, badList: List[String]) = !badList.exists(x =>
    xpath.contains("/" + x + ":") || xpath.contains("/@" + x + ":")
  )

  val xpathXmlEnumerator = new XpathXmlEnumerator(XML.load(
    this.getClass.getResourceAsStream("/DDICodebook2dot5/xml/4245.xml")
  ).toList)

  val entryXsdFile = "/DDICodebook2dot5/xsd/codebook.xsd"
  val xmlTestData = xpathXmlEnumerator.enumSimple
    .filter(x => xmlXpathFilter(x._1, unwantedPrefixes))

  val xmlTestDataNonUniq = toWildCard(xmlTestData.map{x => x._1})

  val readAndFindFromFile = makePairedTester(xmlTestDataNonUniq)

  // Just do /codebook first
  def codebookFilter(cPath: String, node: Node): Boolean =
    if (
    cPath.startsWith("/codeBook") || cPath === "/"
    ) true
    else {
      println(s"rejecting cPath = $cPath; node.label = ${node.label}")
      false
    }

  //TODO: improve this in the future; right now this is a proof-of-principle
  def notFormOrElementType(cPath: String, node: Node): Boolean = {
    val badTypes = List("ExtLinkType", "LinkType")

    //
    // DEBUG
    //
//    val dbgList = node.attributes.asAttrMap.toList.find(pair => pair._1 === "type").map(pair => pair._2)
//    dbgList match{
//      case Some(list) if list.nonEmpty => println(s"types: $list")
//      case _ =>
//    }
//

    val typeMaybe: Option[String] = node.attributes.asAttrMap.toList
      .find(pair => pair._1 === "type").map(pair => pair._2)
    typeMaybe match {
      case Some(xmlType) => !badTypes.contains(xmlType)
      case None => true
    }
  }


  val testFilters = NodeFilters(List(
    NodeFilterGeneric((x1: String, x2: Node) => false),
    NodeFilterGeneric((x1: String, x2: Node) => false)
  )) // TODO: make a "null" test for this?

  val productionLikeFilters = NodeFilters(List(
    NodeFilterGeneric(notFormOrElementType)
  ))

  //  val readAndFindCodebook = readAndFindFromFile(entryXsdFile, Some(notFormOrElementType)) must beTrue
  val readAndFindCodebook = readAndFindFromFile(entryXsdFile, productionLikeFilters) must beTrue

  //val readAndFindDDILC32 = readAndFindFromFile(entryXsdFile, None) must beTrue

}



