package edu.ncrn.cornell.xml
import scala.annotation.tailrec
import scala.xml.{Node, Utility}
import ScalaXmlExtra._
import XpathEnumerator._

/**
  * @author Brandon Barker
  *         9/8/2016
  */
trait XpathXmlEnumerator extends XpathEnumerator {

  @tailrec
  final def enumerateXml(
    nodes: Seq[(Node, String)], pathData: List[(String, String)] = Nil
  ): List[(String, String)] = nodes.filter(x => nodeFilter(x._1)) match {
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
    nodes: Seq[Node], nonEmpty: Boolean = true,
    newNodeFilter: Node => Boolean = _ => true
  ): List[(String, String)] = {
    nodeFilter = newNodeFilter
    enumerateXml(pathifyNodes(
      nodes.map(x => Utility.trim(x)), "/", nonEmpty
    ))
  }



}

object XpathXmlEnumerator {

  //def apply() ...

}

