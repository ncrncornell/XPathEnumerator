package edu.ncrn.cornell.xml
import scala.annotation.tailrec
import scala.xml.{Node, Utility}
import ScalaXmlExtra._
import XpathEnumerator._

/**
  * @author Brandon Barker
  *         9/8/2016
  */
class XpathXmlEnumerator(
  protected val nodesIn: List[Node]
) extends XpathEnumerator {

  @tailrec
  final def enumerateXml(
    nodes: List[(Node, String)], pathData: List[(String, String)]
  )(implicit nodeFilters: NodeFilters): List[(String, String)] =
    nodes.filter(x => nodeFilters(x._2, x._1)) match {
      case (node, currentPath) +: rest =>
        val newElementData =
          if(node.child.isEmpty) List((cleanXpath(currentPath), node.text))
          else Nil
        val newAttributeData = node.attributes.asAttrMap.map{
          case (key, value) => (currentPath + "/@" + key, value)
        }.toList
        enumerateXml(
          rest ++ pathifyNodes(node.child.toList, currentPath + "/", nonEmpty),
          newElementData ::: newAttributeData ::: pathData
        )
      case Nil => pathData
    }

  def enumerate(
    nonEmpty: Boolean,
    newNodeFilters: NodeFilters
  ): List[(String, String)] = {
    implicit val nodeFilters = newNodeFilters
    this.nonEmpty = nonEmpty
    enumerateXml(pathifyNodes(
      nodesIn.map(x => Utility.trim(x)), "/", nonEmpty
    ), Nil)
  }



}

object XpathXmlEnumerator {

  //def apply() ...

}

