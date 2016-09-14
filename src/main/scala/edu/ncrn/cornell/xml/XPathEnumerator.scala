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

  @tailrec
  final def enumerate(
    nodes: Seq[(Node, String)], pathData: List[(String, String)] = Nil
  ): List[(String, String)] = nodes match {
    case (node, currentPath) +: rest =>
      val newElementData =
        if(node.child.isEmpty) List((cleanXpath(currentPath), node.text))
        else Nil
      val newAttributeData = node.attributes.asAttrMap.map{
        case (key, value) => (currentPath + "/@" + key, value)
      }.toList
      enumerate(
        rest ++ pathifyNodes(node.child, currentPath + "/"),
        newElementData ::: newAttributeData ::: pathData
      )
    case Seq() => pathData
  }
}

object XPathEnumerator{


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


  //def apply() ...

}

