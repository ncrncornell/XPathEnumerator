package edu.ncrn.cornell.xml

import edu.ncrn.cornell.xml.XpathEnumerator.{NodeFilter, NodeFilterGeneric, NodeFilters}

import scala.xml.{Node, XML}

/**
  * Created by Brandon on 9/25/2016.
  */
object Util {

  val elementIndex= "\\[\\d+\\]".r

  /**
    * The same as the singleton variant, but removes resulting duplicates
    */
  def toWildCard(xpaths: Iterable[String]): Iterable[String] = xpaths.map{x =>
    elementIndex.replaceAllIn(x, "")
  }.toSet

  /**
    * Designed to test a given schema over a list of XPaths to validate
    * that they are all present.
    */
  def makePairedTester(expectedXpaths: Iterable[String])
  : (String, NodeFilters) => Boolean =
    (fileName: String, nodeFilters: NodeFilters) => {
      val xsdXml = XML.load(this.getClass.getResourceAsStream(fileName)).toList
      val enumerator: XpathXsdEnumerator = new XpathXsdEnumerator(xsdXml)
      println(s"!!!! Initiating enumeration of $fileName !!!!")
      val xsdXmlData = enumerator.enumerate(enumerator.nonEmpty, nodeFilters)
      xsdXmlData.foreach(x => println(x)) // DEBUG
      val missingXPaths = expectedXpaths.toSet -- xsdXmlData.map(_._1).toSet
      missingXPaths.foreach(x => println(s"missing: $x"))
      missingXPaths.isEmpty
  }

}
