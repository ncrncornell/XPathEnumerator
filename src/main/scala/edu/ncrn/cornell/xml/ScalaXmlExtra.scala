package edu.ncrn.cornell.xml

import scala.xml.Node

/**
  * @author Brandon Barker
  *         9/15/2016
  */

object ScalaXmlExtra {
  implicit class NodeExtra(val node: Node) extends AnyVal{

    /**
      * Convenience method for looking up attribute values using
      * the attribute name.
      */
    def attributeVal(attrName: String): Option[String] =
      node.attributes.asAttrMap.get(attrName)

    /**
      *
      * @return the qualified name of the node: "prefix:label";
      *         like Node.nameToString but doesnt' require passing
      *         a StringBuilder
      */
    def fullName: String = Option(node.prefix) match {
      case Some(pfx) => pfx + ":" + node.label
      case None => node.label
    }

    def hasLabel: Boolean = !node.label.trim.isEmpty

  }


}
