package edu.ncrn.cornell.xml

import scala.annotation.tailrec
import scala.xml.{Node, Utility}
import scalaz._, Scalaz._
import shapeless._
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

// TODO: make safe by passing around all state as a state object, with Shapeless Lenses for updates:
// http://stackoverflow.com/questions/3900307/cleaner-way-to-update-nested-structures
// http://stackoverflow.com/questions/9003874/idiomatic-way-to-update-value-in-a-map-based-on-previous-value
class XpathXsdEnumerator(
  protected val nodesIn: Seq[Node]
) extends XpathEnumerator {




  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  object EnumArgsObj {
    case class EnumArgs(
     namedAttributes: XpathNodeMap,
     namedElements: XpathNodeMap,
     namedTypes: XpathNodeMap
   )

    val namedAttribLens = lens[EnumArgs] >> 'namedAttributes
    val namedElementsLens = lens[EnumArgs] >> 'namedElements
    val namedTypesLens = lens[EnumArgs] >> 'namedTypes

    //TODO: why doesn't the Lens .modify method work?
    def updateAttribs(enumArgs: EnumArgs, namedAttributes: XpathNodeMapEntry*) =
      if (namedAttributes.nonEmpty)
        namedAttribLens.set(enumArgs)(namedAttribLens.get(enumArgs) ++ namedAttributes)
      else enumArgs

    def updateElems(enumArgs: EnumArgs, namedElements: XpathNodeMapEntry*) =
      if (namedElements.nonEmpty)
        namedElementsLens.set(enumArgs)(namedElementsLens.get(enumArgs) ++ namedElements)
      else enumArgs

    def updateTypes(enumArgs: EnumArgs, namedTypes: XpathNodeMapEntry*) =
      if (namedTypes.nonEmpty)
        namedTypesLens.set(enumArgs)(namedTypesLens.get(enumArgs) ++ namedTypes)
      else enumArgs
  }
  import EnumArgsObj._

  sealed abstract class NodeWrapAbstract(node: Node, eArgs: EnumArgs)
  sealed case class NodeWrap(node: Node, eArgs: EnumArgs)
    extends NodeWrapAbstract(node, eArgs) {
    def child = node.node.child.map{ch => NodeWrap(ch, eArgs)}
  }
  sealed case class NodeWrapCarry(node: Node, eArgs: EnumArgs, label: String)
    extends NodeWrapAbstract(node, eArgs)


  type NodeRemaining = (NodeWrap, String, List[Node])

  type XpathNodeMapEntry = (String, NodeWrap)
  type XpathNodeMap = Map[String, NodeWrap]

  @SuppressWarnings(Array(
    "org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Nothing"
  ))
  object NodeRemaining {
    val nodeLens = lens[NodeRemaining] >> '_1
    val nodeArgLens = lens[NodeRemaining] >> '_1 >> 'eArgs
  }
  import NodeRemaining._


  //
  // Maps from an XPath to a reusable (named) element or type;
  // used for scope determination and node lookup
  //
  //var namedTypes: XpathNodeMap = Map[String, Node]()
  //var namedElements: XpathNodeMap = Map[String, Node]()
  //var namedAttributes: XpathNodeMap = Map[String, Node]()

  // val debugger = new XsdDebugger()

  object XsdNamedType {
    // Note that an unnamed ComplexType can only be used by the parent element,
    // so we don't need to recognize such unnamed cases here.
    def unapply(node: NodeWrap): Option[(String, EnumArgs)] =
    if (xsdNamedTypes.contains(node.node.fullName)) {
      val label = node.node.attributeVal("name")
      //TODO probably need a better way to look up namespaces
      label.map{labelSome =>
        (labelSome, updateTypes(node.eArgs, labelSome -> node))
      }
    }
    else None
  }


  object XsdNamedElement {
    def unapply(node: NodeWrap): Option[(String, EnumArgs)] =
      if (xsdElems.contains(node.node.fullName) && isLocallyDefined(node.node)) {
        val label = node.node.attributeVal("name")
        //TODO probably need a better way to look up namespaces
        label.map{labelSome =>
          (labelSome, updateElems(node.eArgs, labelSome -> node))
        }
      }
      else None
  }

  object XsdNamedAttribute {
    def unapply(node: NodeWrap): Option[(String, EnumArgs)] =
      if (xsdAttribs.contains(node.node.fullName) && isLocallyDefined(node.node))
        node.node.attributeVal("name").map(name => (name, node.eArgs))
      else None
  }

  object XsdNamedLocalNode {
    def unapply(node: NodeWrap): Option[(String, EnumArgs)] =
      XsdNamedElement.unapply(node) orElse XsdNamedAttribute.unapply(node)
  }


  object XsdNonLocalElement {

    def unapplyMatcher(node: NodeWrap, label: String): Option[(String, Try[NodeWrap])] = {
      def getLabel(elseWise: String) = if (label.length < 1 && label.length > -1) elseWise else label
      if (xsdDataNodes.contains(node.node.fullName)) node.node.attributes.asAttrMap match {
        case attrMap
          if attrMap.contains("ref") && node.eArgs.namedAttributes.contains(attrMap("ref")) =>
          Some(
            getLabel(attrMap("ref")),
            Try(NodeWrap(node.eArgs.namedAttributes(attrMap("ref")).node, node.eArgs))
          )
        case attrMap
          if attrMap.contains("ref") && node.eArgs.namedElements.contains(attrMap("ref")) =>
          Some(
            getLabel(attrMap("ref")),
            Try(NodeWrap(node.eArgs.namedElements(attrMap("ref")).node, node.eArgs))
          )
        case attrMap if attrMap.contains("type") =>
          //TODO: probably an oversimplification; may need a list of simple types
          if (attrMap("type").startsWith("xs:")) None
          else Some(
            getLabel(attrMap("name")),
            Try(NodeWrap(node.eArgs.namedTypes(attrMap("type")).node, node.eArgs))
          )
        case _ => None
      }
      else None
    }

    //TODO: may need to match on (Node, XPath: String)


//    def unapply(arg: Node, eArgs: EnumArgs): Option[(String, Try[Node])] =
//      unapply(NodeWrap(arg, eArgs))
    //
    //TODO: also looking up arg.fullName is not sufficient... need whole xpath?
    @tailrec
    def unapply(arg0: NodeWrapAbstract): Option[(String, Try[NodeWrap])] = {
      arg0 match {
        case argNW @ NodeWrap(arg: Node, eArgs: EnumArgs) =>
          val last = unapplyMatcher(argNW, "")
          last match {
            case Some((labelCarry: String, Success(node: NodeWrap)))
              if !isLocallyDefined(node.node) =>
                // Keep chasing the reference
                //TODO: above, need to check to see if we need worry about non-head elements
                unapply(NodeWrapCarry(node.node.child.head, eArgs, labelCarry))
            case _ => last
          }
        case NodeWrapCarry(arg: Node, eArgs: EnumArgs, label: String) =>
          val last = unapplyMatcher(NodeWrap(arg, eArgs), label)
          last match {
            case Some((labelCarry: String, Success(node: NodeWrap)))
              if !isLocallyDefined(node.node) =>
                // Keep chasing the reference
                //TODO: above, need to check to see if we need worry about non-head elements
                unapply(NodeWrapCarry(node.node.child.head, eArgs, labelCarry))
            case _ => last
          }
      }
    }
  }

  def xsdXpathLabel(node: NodeWrap): String = node match {
    case XsdNamedElement(label, eArgs) => label
    case XsdNamedAttribute(label, eArgs) => "@" + label
      //TODO: Left off here. what are we doing here? do we want label?
    case XsdNonLocalElement(label, Success(nodeMaybe)) =>
      if (xsdAttribs.contains(node.node.fullName)) "@" + label
      else label
    case XsdNonLocalElement(label, Failure(e)) =>
      if (xsdAttribs.contains(node.node.fullName)) "@" + label
      else label
    case _ => ""
  }



  def pathifyXsdNodes(nodes: Seq[NodeWrap], parPath: String)
  : Seq[(NodeWrap, String)] = {
    nodes.groupBy(nn => parPath + xsdXpathLabel(nn)).toList.flatMap{
      case(xpath, labNodes) =>
        labNodes.map{nn => (nn, xpath)}
    }
  }

  @tailrec
  final def enumerateXsd(nodes: Seq[(NodeWrap, String, List[Node])], pathData: List[(String, String)])
  : List[(String, String)] = nodes.filter(nn => nodeFilter(nn._1.node)) match {
    case (node, currentPath, refNodesVisited) +: rest =>
      // debugger.addPath(currentPath)
      node match {
        case XsdNamedType(label, eArgsNew) =>
          val restNew = rest.map(nn => nodeArgLens.set(nn)(eArgsNew))
          enumerateXsd(restNew, pathData)
        case XsdNamedElement(label, eArgsNew) =>
          val newElementData =
            if(node.node.child.isEmpty)
              List((cleanXpath(currentPath), node.node.attributes.asAttrMap.getOrElse("type", "DEBUG")))
            else Nil

          val newNodes = pathifyXsdNodes(
            node.child.map(ch => NodeWrap(ch.node, eArgsNew)), currentPath + "/"
          )
          val restNew = rest.map(nn => nodeArgLens.set(nn)(eArgsNew))
          enumerateXsd(
            restNew ++ newNodes.map(nn => (nn._1, nn._2, refNodesVisited)),
            newElementData ::: pathData
          )
        case XsdNamedAttribute(label, eArgsNew) =>
          //TODO probably need a better way to look up namespaces
          val newElementData =
            if(node.child.isEmpty)
              List((cleanXpath(currentPath), node.node.attributes.asAttrMap.getOrElse("type", "DEBUG")))
            else Nil
          val restNew = rest.map(nn => nodeArgLens.modify(nn)(updateAttribs(_, label -> node)))
          enumerateXsd(restNew, newElementData ::: pathData)
        case XsdNonLocalElement(label, nodeMaybe) => nodeMaybe match {
          case Success(refnode) =>
            if (refNodesVisited.contains(refnode.node)) {
              // Skip adding refnode's children; recursive path
              enumerateXsd(rest, (cleanXpath(currentPath), "recursive!") :: pathData)
            }
            else {
              val newNamedElems: Seq[XpathNodeMapEntry] =
                if (xsdNamedTypes.contains(refnode.node.fullName) &&
                  refnode.node.attributes.asAttrMap.contains("name")) {
                // This will be a named element that forwards to some other node
                //namedElements += (label -> refnode)
                  Seq(label -> refnode)
                }
                else Seq.empty
              val newElementData =
                List((cleanXpath(currentPath),
                  refnode match {
                    case _ if refnode.node.attributes.asAttrMap.contains("type") =>
                      refnode.node.attributes.asAttrMap("type")
                    case _ => refnode.node.child.headOption match {
                      case Some(child) if child.fullName === "xs:restriction" =>
                        child.attributes.asAttrMap.getOrElse("base", "asdf")
                      case _ => ""
                    }
                  }
                  )).filter(ne => !nonEmpty || ne._2.nonEmpty)
              val eArgsNew = updateElems(refnode.eArgs, newNamedElems: _*)
              val newNodes = pathifyXsdNodes(
                refnode.child.map(ch => NodeWrap(ch.node, eArgsNew)),
                currentPath + "/"
              )
              val restNew = rest.map(nn => nodeArgLens.set(nn)(eArgsNew))
              // Continue with refnode's children instead
              enumerateXsd(
                restNew ++ newNodes.map(nn => (nn._1, nn._2, refnode.node :: refNodesVisited)),
                newElementData ::: pathData
              )
            }
          case Failure(e) => //TODO: narrow this down to appropriate error
            // Not ready yet, let's try again later:
            enumerateXsd(rest ++ Seq((node, currentPath, refNodesVisited)), pathData)
        }
        case _ =>
          // Default; no path change
          val newNodes = pathifyXsdNodes(node.child, currentPath)
          enumerateXsd(
            rest ++ newNodes.map(nn => (nn._1, nn._2, refNodesVisited)), pathData
          )
      }
    case Seq() => pathData
  }

  def enumerate(
    nonEmpty: Boolean,
    newNodeFilter: Node => Boolean
  ): List[(String, String)] = {
    this.nonEmpty = nonEmpty
    this.nodeFilter = newNodeFilter
    val initArgs = EnumArgs(Map.empty, Map.empty, Map.empty)
    val wrappedNodes = nodesIn.map(x => Utility.trim(x))
      .map(nn => NodeWrap(nn, initArgs))
    val initNodes = pathifyXsdNodes(wrappedNodes, "/")
    enumerateXsd(initNodes.map(nn => (nn._1, nn._2, Nil)), Nil)
  }

}


object XpathXsdEnumerator {


  // Let's model the types of nodes we care about with extractors,
  // returning None if it isn't an appropriate type

  val xsdElems = List("xs:element")
  val xsdAttribs = List("xs:attribute")
  val xsdNamedTypes = List("xs:simpleType", "xs:complexType")
  val xsdDataNodes = xsdElems ::: xsdAttribs


  def isLocallyDefined(arg: Node) =
    arg.attributeVal("ref").isEmpty && (
      arg.attributeVal("type").isEmpty ||
      arg.attributeVal("type").getOrElse("").startsWith("xs:")
    )

}

