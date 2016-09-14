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

object XpathXmlEnumerator {

  //def apply() ...

}

