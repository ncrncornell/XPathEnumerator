package edu.ncrn.cornell.xml

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.xml.{Elem, Node}
import ScalaXmlExtra._
import XpathEnumerator._
import XpathXsdEnumerator._


/**
  * @author Brandon Barker
  *         9/14/2016
  */

//TODO: things to deal with: element+name, refs, types, attributes
//
//TODO: need to assume only non-recursive xpaths are of interest, can probably do this by
//TODO: counting references to particular xml types on an xpath

//TODO: Attempt to support certain "bounded" uses of xs:any, probably just
//TODO: ##local and ##targetNamespace namespaces : http://www.w3schools.com/xml/el_any.asp
//TODO: Need to flesh out possibilities for anyAttribute anyURI, etc.

trait XpathXsdEnumerator extends XpathEnumerator {

  implicit val xpathXsdEnumerator: XpathXsdEnumerator = this

  //
  // Maps from an XPath to a reusable (named) element or type;
  // used for scope determination and node lookup
  //
  var namedTypes: Map[String, Node] = Map()
  var namedElements: Map[String, Node] = Map()

  @tailrec
  final def enumerateXsd(
     nodes: Seq[(Node, String)], pathData: List[(String, String)] = Nil
   ): List[(String, String)] = nodes match {
    case (node, currentPath) +: rest =>
      val newElementData =
        if(node.child.isEmpty) List((cleanXpath(currentPath), node.text))
        else Nil
      val newAttributeData = node.attributes.asAttrMap.map{
        case (key, value) => (currentPath + "/@" + key, value)
      }.toList
      node match {
        case XsdNamedType(label) =>
          namedTypes += (cleanXpath(currentPath) -> node)
        case XsdNamedElement(label) =>
          namedElements += (cleanXpath(currentPath) -> node)
        case _ => ()
      }
      enumerateXsd(
        rest ++ pathifyNodes(node.child, currentPath + "/"),
        newElementData ::: newAttributeData ::: pathData
      )
    case Seq() => pathData
  }

  def enumerate(
    nodes: Seq[Node], nonEmpty: Boolean = false
  ): List[(String, String)] = enumerateXsd(pathifyNodes(nodes, "/", nonEmpty))

}


object XpathXsdEnumerator {

  // Let's model the types of nodes we care about with extractors,
  // returning None if it isn't an appropriate type

  val xsdElems = List("xs:element")
  val xsdAttribs = List("xs:attribute")
  val xsdTypes = List("xs:complexType", "xs:simpleType")
  val xsdNamedNodes = xsdElems ::: xsdAttribs :: xsdTypes


  object XsdNamedType {
    // Note that an unnamed ComplexType can only be used by the parent element,
    // so we don't need to recognize such unnamed cases here.
    def unapply(arg: Node): Option[String] =
    if (xsdTypes.contains(arg.label)) arg.attributeVal("name") else None
  }


  object XsdNamedElement {
    def unapply(arg: Node): Option[String] =
      if (xsdElems.contains(arg.label)) arg.attributeVal("name") else None
  }

  object XsdNamedAttribute {
    def unapply(arg: Node): Option[Node] =
      if (xsdAttribs.contains(arg.label)) Some(arg) else None
  }

  //TODO: this isn't exactly what we want ... within the future, caller.namedElements is
  //TODO: executed immediately, but we need to see if it exists each time when probed
  object XsdNonLocalElement {
    def unapply(arg: Node)(implicit caller: XpathXsdEnumerator): Option[Future[Node]] =
      if (xsdElems.contains(arg.label)) Some(Future(caller.namedElements(arg.label)))
      else if (xsdTypes.contains(arg.label)) Some(Future(caller.namedTypes(arg.label)))
      else None
  }

}

