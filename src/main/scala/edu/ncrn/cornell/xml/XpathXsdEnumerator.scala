package edu.ncrn.cornell.xml

import scala.annotation.tailrec
import scala.xml.{Node, Utility}
import ScalaXmlExtra._
import XpathEnumerator._
import XpathXsdEnumerator._

import scala.util.{Failure, Success, Try}


/**
  * @author Brandon Barker
  *         9/14/2016
  */


//TODO: Attempt to support certain "bounded" uses of xs:any, probably just
//TODO: ##local and ##targetNamespace namespaces : http://www.w3schools.com/xml/el_any.asp
//TODO: Need to flesh out possibilities for anyAttribute anyURI, etc.

// Note: do not use concurrently!

trait XpathXsdEnumerator extends XpathEnumerator {

  //
  // Maps from an XPath to a reusable (named) element or type;
  // used for scope determination and node lookup
  //
  var namedTypes: XpathNodeMap = Map()
  var namedElements: XpathNodeMap = Map()

  //TODO: may need to match on (Node, XPath: String)
  object XsdNonLocalElement {
    //TODO: also looking up arg.fullName is not sufficient... need whole xpath?
    def unapply(arg: Node): Option[(String, Try[Node])] =
      if (xsdElems.contains(arg.fullName)) arg.attributes.asAttrMap match {
        case attrMap if attrMap.contains("ref") =>
          Some(attrMap("ref"), Try(namedElements(attrMap("ref"))))
        case attrMap if attrMap.contains("type") =>
          //TODO: probably an oversimplification; may need a list of simple types
          if (attrMap("type").startsWith("xs:")) None
          else Some(attrMap("name"), Try(namedTypes(attrMap("type"))))
        case _ => None
      }
      else None
  }

  //  object XsdNamedNode {
  //    def unapply(arg: Node): Option[String] =
  //      XsdNamedLocalNode.unapply(arg) orElse XsdNonLocalElement.unapply(arg)
  //  }

  def xsdXpathLabel(node: Node): String = {
    val fullLName = node.fullName // DEBUG
    val nodeStr = node.toString //DEBUG
    node match {
      case XsdNamedLocalNode(label) => label
      case XsdNonLocalElement(nodeMaybe) =>
        val test = nodeMaybe._1
        test // DEBUG
      case _ => ""
    }
  }


  def pathifyXsdNodes(nodes: Seq[Node], parPath: String = "/")
  : Seq[(Node, String)] = {
    nodes.groupBy(nn => parPath + xsdXpathLabel(nn)).toList.flatMap{
      case(xpath, labNodes) =>
        labNodes.zipWithIndex.map{case (nn, ii) => (nn, xpath)}
    }
  }

  //implicit val xpathXsdEnumerator: XpathXsdEnumerator = this

  @tailrec
  final def enumerateXsd(
    nodes: Seq[(Node, String, List[Node])], pathData: List[(String, String)] = Nil
   ): List[(String, String)] = nodes.filter(x => nodeFilter(x._1)) match {
    case (node, currentPath, refNodesVisited) +: rest =>
      val fullName = node.fullName // DEBUG
      node match {
        case XsdNamedType(label) =>
          //TODO probably need a better way to look up namespaces
          namedTypes += (label -> node)
          val newNodes = pathifyXsdNodes(node.child, currentPath + "/")
          enumerateXsd( // Default
            rest ++ newNodes.map(nn => (nn._1, nn._2, refNodesVisited)),
            pathData
          )
        case XsdNamedElement(label) =>
          //TODO probably need a better way to look up namespaces
          namedElements += (label -> node)
          val newElementData =
            if(node.child.isEmpty)
              List((cleanXpath(currentPath), node.attributes.asAttrMap.getOrElse("type", "DEBUG")))
            else Nil
          val newNodes = pathifyXsdNodes(node.child, currentPath + "/")
          enumerateXsd( // Default
            rest ++ newNodes.map(nn => (nn._1, nn._2, refNodesVisited)),
            newElementData ::: pathData
          )
        case XsdNamedAttribute(label) =>
          //val newAttributeData = List((currentPath + "/@" + label, "xs:string"))
          //TODO probably need a better way to look up namespaces
          val newElementData =
            if(node.child.isEmpty)
              List((cleanXpath(currentPath), node.attributes.asAttrMap.getOrElse("type", "DEBUG")))
            else Nil
          enumerateXsd(rest, newElementData ::: pathData)
        case XsdNonLocalElement(label, nodeMaybe) => nodeMaybe match {
          case Success(refnode) =>
            if (refNodesVisited.contains(refnode)) {
              // Skip adding refnode's children; recursive path
              enumerateXsd(rest, (cleanXpath(currentPath), "recursive!") :: pathData)
            }
            else {
              val newElementData =
                List((cleanXpath(currentPath), refnode.attributes.asAttrMap.getOrElse("type", "")))
              val newNodes = pathifyXsdNodes(refnode.child, currentPath + "/")
              enumerateXsd(// Continue with refnode's children instead
                rest ++ newNodes.map(nn => (nn._1, nn._2, refnode :: refNodesVisited)),
                newElementData ::: pathData //TODO: remove adding newElementData here (DEBUG)
              )
            }
          case Failure(e) => //TODO: narrow this down to appropriate error
            enumerateXsd( // Not ready yet, let's try again later:
              rest ++ Seq((node, currentPath, refNodesVisited)),
              pathData
            )
        }
        case _ =>
          val newNodes = pathifyXsdNodes(node.child, currentPath)
            enumerateXsd( // Default; no path change
            rest ++ newNodes.map(nn => (nn._1, nn._2, refNodesVisited)),
            pathData
          )
      }
    case Seq() => pathData
  }

  def enumerate(
    nodes: Seq[Node], nonEmpty: Boolean = false,
    newNodeFilter: Node => Boolean = _ => true
  ): List[(String, String)] = {
    namedTypes = Map()
    namedElements = Map()
    val initNodes = pathifyXsdNodes(nodes.map(x => Utility.trim(x)), "/")
    enumerateXsd(initNodes.map(nn => (nn._1, nn._2, Nil)))
  }

}


object XpathXsdEnumerator {

  type XpathNodeMap = Map[String, Node]

  // Let's model the types of nodes we care about with extractors,
  // returning None if it isn't an appropriate type

  val xsdElems = List("xs:element")
  val xsdAttribs = List("xs:attribute")
  val xsdNamedTypes = List("xs:simpleType", "xs:complexType")
  val xsdNamedNodes = xsdElems ::: xsdAttribs :: xsdNamedTypes


  object XsdNamedType {
    // Note that an unnamed ComplexType can only be used by the parent element,
    // so we don't need to recognize such unnamed cases here.
    def unapply(arg: Node): Option[String] =
    if (xsdNamedTypes.contains(arg.fullName)) arg.attributeVal("name") else None
  }


  object XsdNamedElement {
    def unapply(arg: Node): Option[String] =
      if (xsdElems.contains(arg.fullName) &&
        arg.attributeVal("ref").isEmpty &&
        //TODO: may need to consider simple types more precisely
        (arg.attributeVal("type").isEmpty || arg.attributeVal("type").get.startsWith("xs:"))
      ) arg.attributeVal("name")
      else None
  }

  object XsdNamedAttribute {
    def unapply(arg: Node): Option[String] =
      if (xsdAttribs.contains(arg.fullName)) arg.attributeVal("name") match {
        case Some(name) => Some("@" + name)
        case None => None
      } else None
  }

  object XsdNamedLocalNode {
    def unapply(arg: Node): Option[String] =
      XsdNamedElement.unapply(arg) orElse XsdNamedAttribute.unapply(arg)
  }



  //            blocking{
  //            while(!caller.namedElements.contains(arg.fullName)) {}
  //            caller.namedElements(arg.fullName)
  //          }




}

