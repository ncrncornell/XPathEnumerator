package edu.ncrn.cornell.xml

import edu.ncrn.cornell.Util._

//import cats._
//import cats.instances.all._
//import cats.syntax.eq._

import scala.xml.Node
import XpathEnumerator._

/**
  * @author Brandon Barker
  *         9/14/2016
  */
trait XpathEnumerator {

  /**
    * The list of root nodes to start from, typically from an XML document
    */
  protected val nodesIn: List[Node]

  protected val boundaries: BoundarySchemas = BoundarySchemas(Nil)

  var nonEmpty: Boolean = true

  //TODO: need a way to establish boundary conditions on recursion; for instance,
  //TODO leaf nodes (simpleTypes) in XSD, or elements with text data in XML, are
  //TODO leaf nodes and thus natural boundaries. However, we may only want to traverse
  //TODO up to something in the current namespace, or e disregard traversal beyond
  //TODO specified nodes, etc.

  //var XmlFile = ???

  //TODO: add public interface for enumerate that only takes List[Node] as input, along with any optional args

  /**
    *
    * @param nonEmpty If true, return only XPaths with nonEmpty values.
    * @return A list of tuples of (XPath, value at XPath)
    */
  def enumerate(
    nonEmpty: Boolean, newNodeFilters: NodeFilters
  ): List[(String, String)]

  /**
    * Typical use case that can be overridden if necessary.
    * @return A list of unfiltered tuples of XPaths and value at
    *         XPath, with the exception that empty values are filtered.
    */
  def enumSimple = enumerate(nonEmpty = true, NodeFilters(List(
    NodeFilterGeneric((_, _) => true))
  ))

}

object XpathEnumerator{


  sealed abstract class NodeFilter(fun: (String, Node) => Boolean)
  extends Function2[String, Node, Boolean] {
    override def apply(ident: String, node: Node): Boolean = fun(ident, node)
  }

  final case class NodeFilters(filters: List[NodeFilter])
  extends Function2[String, Node, Boolean] {
    override def apply(ident: String, node: Node): Boolean = {

      val x = 2+2 // DEBUG

      filters.forall(nf => nf(ident, node))
    }
  }

  val noFilter = NodeFilters(List(
    NodeFilterGeneric((x1: String, x2: Node) => true)
  ))

  final case class NodeFilterGeneric(fun: (String, Node) => Boolean)
    extends NodeFilter(fun)



  //TODO: make arguments of returned function HList? Ideal not to have HList in public API, though.
//  implicit def NodeFilterToFunction2(nodeFilter: NodeFilter)
//  : (String, Node) => Boolean =
//    (cPath: String, node: Node) => nodeFilter match {
//      case nf: NodeFilterPath => nf.fun(cPath, node)
//    }

  def cleanXpath(xpath: String) = xpath.replaceFirst("/#PCDATA", "")
  /**
    * Helper function to add XPaths to a node sequence; assume a default of root nodes.
    */
  def pathifyNodes(
    nodes: List[Node], parPath: String, nonEmpty: Boolean
  ): List[(Node, String)] = {
    def nodeNotEmpty(node: Node) =
      if (nonEmpty) node.text =!= "" else true
    nodes.filter(nodeNotEmpty).groupBy(nn => parPath + nn.label).toList.flatMap{
      case(xpath, labNodes) =>
        def xindex(index: Int) = if (labNodes.size > 1) s"[${index + 1}]" else ""
        labNodes.zipWithIndex.map{case (nn, ii) => (nn, xpath + xindex(ii))}
    }
  }

}