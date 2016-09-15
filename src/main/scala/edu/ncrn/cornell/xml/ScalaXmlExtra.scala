package edu.ncrn.cornell.xml

import scala.xml.Node

/**
  * @author Brandon Barker
  *         9/15/2016
  */

object ScalaXmlExtra {
  implicit class NodeExtra(val node: Node) extends AnyVal{
    def attributeVal(attrName: String): Option[String] =
      node.attributes.asAttrMap.get(attrName)
  }

}
