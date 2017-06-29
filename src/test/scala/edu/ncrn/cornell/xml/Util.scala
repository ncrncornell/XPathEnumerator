package edu.ncrn.cornell.xml

import edu.ncrn.cornell.xml.XpathEnumerator.{NodeFilter, NodeFilterPath, NodeFilters}

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
  : (String, Option[(String, Node) => Boolean]) => Boolean =
    (fileName: String, nodeFilter: Option[(String, Node) => Boolean]) => {
      val xsdXml = XML.load(this.getClass.getResourceAsStream(fileName)).toList
      val enumerator: XpathXsdEnumerator = new XpathXsdEnumerator(xsdXml)
      val newNodeFilter: NodeFilter = nodeFilter match {
        case Some(nf) => NodeFilterPath(nf)
        case None =>
          println("DEBUG: setting nf to false")
          NodeFilterPath((_, _) => true)
      }
      println(s"!!!! Initiating enumeration of $fileName !!!!")
      val xsdXmlData = enumerator.enumerate(
        enumerator.nonEmpty, NodeFilters(List(newNodeFilter))
      )
      xsdXmlData.foreach(x => println(x)) // DEBUG
      val missingXPaths = expectedXpaths.toSet -- xsdXmlData.map(_._1).toSet
      missingXPaths.foreach(x => println(s"missing: $x"))
      missingXPaths.isEmpty
  }

}
