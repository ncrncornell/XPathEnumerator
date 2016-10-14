package edu.ncrn.cornell.xml

import scalaz._, Scalaz._

import scala.xml.Node

/**
  * @author Brandon Barker
  *         9/14/2016
  */
trait XpathEnumerator {

  /**
    * The list of root nodes to start from, typically from an XML document
    */
  protected val nodesIn: Seq[Node]

  protected var nodeFilter: Node => Boolean = (_) => true
  protected var nonEmpty: Boolean = true

  //TODO: need a way to establish boundary conditions on recursion; for instance,
  //TODO leaf nodes (simpleTypes) in XSD, or elements with text data in XML, are
  //TODO leaf nodes and thus natural boundaries. However, we may only want to traverse
  //TODO up to something in the current namespace, or e disregard traversal beyond
  //TODO specified nodes, etc.

  //var XmlFile = ???

  //TODO: add public interface for enumerate that only takes Seq[Node] as input, along with any optional args

  /**
    *
    * @param nonEmpty If true, return only XPaths with nonEmpty values.
    * @return A list of tuples of (XPath, value at XPath)
    */
  def enumerate(
    nonEmpty: Boolean, newNodeFilter: Node => Boolean
  ): List[(String, String)]

  /**
    * Typical use case that can be overridden if necessary.
    * @return A list of unfiltered tuples of XPaths and value at
    *         XPath, with the exception that empty values are filtered.
    */
  def enumSimple = enumerate(nonEmpty = true, _ => true)

}

object XpathEnumerator{

  def cleanXpath(xpath: String) = xpath.replaceFirst("/#PCDATA", "")
  /**
    * Helper function to add XPaths to a node sequence; assume a default of root nodes.
    */
  def pathifyNodes(
    nodes: Seq[Node], parPath: String, nonEmpty: Boolean
  ): Seq[(Node, String)] = {
    def nodeNotEmpty(node: Node) =
      if (nonEmpty) node.text =/= "" else true
    nodes.filter(nodeNotEmpty).groupBy(nn => parPath + nn.label).toList.flatMap{
      case(xpath, labNodes) =>
        def xindex(index: Int) = if (labNodes.size > 1) s"[${index + 1}]" else ""
        labNodes.zipWithIndex.map{case (nn, ii) => (nn, xpath + xindex(ii))}
    }
  }

}