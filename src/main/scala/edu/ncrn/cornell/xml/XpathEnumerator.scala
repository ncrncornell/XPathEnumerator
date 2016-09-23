package edu.ncrn.cornell.xml

import scala.xml.Node

/**
  * @author Brandon Barker
  *         9/14/2016
  */
trait XpathEnumerator {

  //TODO: need a way to establish boundary conditions on recursion; for instance,
  //TODO leaf nodes (simpleTypes) in XSD, or elements with text data in XML, are
  //TODO leaf nodes and thus natural boundaries. However, we may only want to traverse
  //TODO up to something in the current namespace, or e disregard traversal beyond
  //TODO specified nodes, etc.

  //var XmlFile = ???

  //TODO: add public interface for enumerate that only takes Seq[Node] as input, along with any optional args

  /**
    *
    * @param nodes The list of root nodes to start from, typically from an XML document
    * @param nonEmpty If true, return only XPaths with nonEmpty values.
    * @return A list of tuples of (XPath, value at XPath)
    */
  def enumerate(
    nodes: Seq[Node], nonEmpty: Boolean
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
    def nodeNotEmpty(node: Node) =
      if (nonEmpty) node.text != "" else true
    nodes.filter(nodeNotEmpty).groupBy(nn => parPath + nn.label).toList.flatMap{
      case(xpath, labNodes) =>
        def xindex(index: Int) = if (labNodes.size > 1) s"[${index + 1}]" else ""
        labNodes.zipWithIndex.map{case (nn, ii) => (nn, xpath + xindex(ii))}
    }
  }

}