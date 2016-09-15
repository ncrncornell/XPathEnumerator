package edu.ncrn.cornell.xml
import scala.annotation.tailrec

import scala.xml.Node

import XpathEnumerator._

/**
  * @author Brandon Barker
  *         9/8/2016
  */
trait XpathXmlEnumerator extends XpathEnumerator {

  @tailrec
  final def enumerateXml(
    nodes: Seq[(Node, String)], pathData: List[(String, String)] = Nil
  ): List[(String, String)] = nodes match {
    case (node, currentPath) +: rest =>
      val newElementData =
        if(node.child.isEmpty) List((cleanXpath(currentPath), node.text))
        else Nil
      val newAttributeData = node.attributes.asAttrMap.map{
        case (key, value) => (currentPath + "/@" + key, value)
      }.toList
      enumerateXml(
        rest ++ pathifyNodes(node.child, currentPath + "/"),
        newElementData ::: newAttributeData ::: pathData
      )
    case Seq() => pathData
  }

  def enumerate(
    nodes: Seq[Node], nonEmpty: Boolean = true
  ): List[(String, String)] = enumerateXml(pathifyNodes(nodes, "/", nonEmpty))



}

object XpathXmlEnumerator {

  //def apply() ...

}

