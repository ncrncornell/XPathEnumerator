package edu.ncrn.cornell.xml

import scala.annotation.tailrec

import scala.xml.{Elem, Node}

import XpathEnumerator._

/**
  * @author Brandon Barker
  *         9/14/2016
  */

//TODO: things to deal with: element+name, refs, types, attributes

trait XpathXsdEnumerator extends XpathEnumerator {

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


object XpathXsdEnumerator {

  // Let's model the types of nodes we care about with extractors,
  // returning None if it isn't an appropriate type

  object XsdElement {
    def unapply(arg: Node): Option[String] = arg match {
      case elem: Elem => elem.attributes.asAttrMap.get("name")
    } 

  }
}

