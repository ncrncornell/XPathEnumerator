package edu.ncrn.cornell.xml
import scala.annotation.tailrec

import scala.xml.Node

import XPathEnumerator._

/**
  * @author Brandon Barker
  *         9/8/2016
  */
trait XPathEnumerator {

  //var XmlFile = ???

  //TODO: need to support attributes as parental selectors. For each element, we have to have n loops
  // where n is the number of attribute types

  @tailrec
  final def uniqueXpaths(
    nodes: Seq[(Node, String)], pathData: List[(String, String)] = Nil
  ): List[(String, String)] = nodes match {
    case (node, currentPath) +: rest =>
      val newElementData =
        if(node.child.isEmpty) List((currentPath, node.text))
        else Nil
      val newAttributeData = node.attributes.asAttrMap.map{
        case (key, value) => (currentPath + "@" + key, value)
      }.toList
      uniqueXpaths(
        rest ++ node.child.flatMap(ns => pathifyNodes(ns, currentPath)),
        newElementData ::: newAttributeData ::: pathData
      )
    case Seq() => pathData
  }
}

object XPathEnumerator{


  /**
    * Helper function to add XPaths to a node sequence; assume a default of root nodes.
    */
  def pathifyNodes(nodes: Seq[Node], parPath: String = "/"): Seq[(Node, String)] =
    nodes.map{nn => (nn, parPath + nn.label + "/")}

  //  val elementWildcard = "\\[\\*\\]".r
  //  val attributeWildcard = "(.+)\\[@(.+)\\]".r
  //  val wildcard = "\\*".r

  //def apply() ...

}

