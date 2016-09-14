package edu.ncrn.cornell.xml

import scala.xml.Node

/**
  * @author Brandon Barker
  *         9/14/2016
  */
trait XpathEnumerator {

  //var XmlFile = ???

  def enumerate(
    nodes: Seq[(Node, String)], pathData: List[(String, String)] = Nil
  ): List[(String, String)]

}

object XpathEnumerator{

  def cleanXpath(xpath: String) = xpath.replaceFirst("/#PCDATA", "")
  /**
    * Helper function to add XPaths to a node sequence; assume a default of root nodes.
    */
  def pathifyNodes(
    nodes: Seq[Node], parPath: String = "/", nonEmpty: Boolean = true
  ): Seq[(Node, String)] = {
    def nodeIsEmpty(node: Node) = if (nonEmpty && node.child.isEmpty) node.text != "" else true
    nodes.filter(nodeIsEmpty).groupBy(nn => parPath + nn.label).toList.flatMap{
      case(xpath, labNodes) =>
        def xindex(index: Int) = if (labNodes.size > 1) s"[${index + 1}]" else ""
        labNodes.zipWithIndex.map{case (nn, ii) => (nn, xpath + xindex(ii))}
    }
  }

}