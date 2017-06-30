package edu.ncrn.cornell.xml

import edu.ncrn.cornell.Util._

import scala.collection.breakOut
import scala.annotation.tailrec
import scala.xml.{Node, Utility}
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
  protected val nodesIn: List[Node]
) extends XpathEnumerator {


  import EnumArgs._

  type NodeRemaining = (NodeWrap, String, List[Node])

  type XpathNodeMapEntry = (String, NodeWrap)
  type XpathNodeMap = Map[String, NodeWrap]

  @SuppressWarnings(Array(
    "org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Nothing"
  ))
  private object NodeRemaining {
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

  val debugger = new XsdDebugger()

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

    type NodeMaybe = Option[(String, Try[NodeWrapAbstract])]

    def unapplyMatcher(node: NodeWrap, label: String): NodeMaybe = {
      def getLabel(elseWise: String) = if (label.length === 0) elseWise else label
      node.node.attributes.asAttrMap match {
        case attrMap
          if attrMap.contains("ref") && node.eArgs.filteredElements.contains(attrMap("ref")) =>
          println("oh boy!")
          Some(
            getLabel(attrMap("ref")),
            Try(FilteredNode(node.eArgs.filteredElements(attrMap("ref")).node, node.eArgs))
          )
        case attrMap
          if xsdAttribs.contains(node.node.fullName) && attrMap.contains("ref")
            && node.eArgs.namedAttributes.contains(attrMap("ref")) =>
          Some(
            getLabel(attrMap("ref")),
            Try(NodeWrap(node.eArgs.namedAttributes(attrMap("ref")).node, node.eArgs))
          )
        case attrMap
          if xsdElems.contains(node.node.fullName) && attrMap.contains("ref")
            && node.eArgs.namedElements.contains(attrMap("ref")) =>
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
        case attrMap =>
          //println(attrMap)
          println(node.eArgs.filteredElements)
          None
      }
    }

    //TODO: may need to match on (Node, XPath: String)


    /**
      *
      * @param arg0 The potentially non-locally defined node for whose
      *             definition we are searching.
      * @return The xpath of the node definition, along with wrapped node,
      *         if we were able to find it.
      */
    @tailrec
    def unapply(arg0: NodeWrapAbstract): NodeMaybe = {
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

  object XsdUnion {
    // XsdExtension is really a special case of a non-local node;
    // it is special in that it is a unon of the current children and the
    // reference children
    def unapply(node: NodeWrap): Option[Try[NodeWrap]] =
    //TODO: supporting extension seems  to have incurred a noticeable
    //TODO: performance hit, try to optimize.
    //TODO: (this mainly appeaers to be a result of high-memory use)
      if (node.node.fullName === "xs:extension")
        node.node.attributeVal("base") map {baseType =>
          Try(node.eArgs.namedTypes(baseType))
        }
      else if (node.node.fullName === "xs:attributeGroup")
        node.node.attributeVal("ref") map {group =>
          Try(node.eArgs.namedTypes(group))
        }
      else if (node.node.fullName === "xs:group")
        node.node.attributeVal("ref") map { group =>
          Try(node.eArgs.namedTypes(group))
        }
      else None

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



  def pathifyXsdNodes(nodes: List[NodeWrap], parPath: String)
  : List[(NodeWrap, String)] = {
    nodes.groupBy(nn => parPath + xsdXpathLabel(nn)).toList.flatMap{
      case(xpath, labNodes) =>
        labNodes.map{nn => (nn, xpath)}
    }
  }

  @tailrec
  final def enumerateXsd(nodes: List[(NodeWrap, String, List[Node])], pathData: List[(String, String)])
  (implicit nodeFilters: NodeFilters)
  : List[(String, String)] = {
    val (keptRawNodes, filteredRawNodes) = nodes.partition(nn => nodeFilters(nn._2, nn._1.node))
    val filteredNodes: List[FilteredElementEntry] = filteredRawNodes.map(nn => (nn._2, nn._1.node))
    val keptNodes = keptRawNodes.map{nn =>
      val eArgsWithFilt = updateFiltered(nn._1.eArgs, filteredNodes: _*)
      nodeArgLens.set(nn)(eArgsWithFilt)
    }
    keptNodes match {
      case (node, currentPath, refNodesVisited) +: rest =>
        //debugger.addPathNode(currentPath, node)
        debugger.addPath(currentPath)
        debugger.progressCount(1000)
        //println(s"current path is: $currentPath of type ${node.node.fullName}") // DEBUG
        node match {
          case XsdNamedType(label, eArgsNew) =>
            val restNew = rest.map(nn => nodeArgLens.set(nn)(eArgsNew))
            debugger.printOnProgressCount("calling enumerateXsd at XsdNamedType", 1000) // DEBUG
            enumerateXsd(restNew, pathData)
          case XsdNamedElement(label, eArgsNew) =>
            val newElementData =
              if (node.node.child.isEmpty)
                List((cleanXpath(currentPath), node.node.attributes.asAttrMap.getOrElse("type", "DEBUG")))
              else Nil

            val newNodes = pathifyXsdNodes(
              node.child.map(ch => NodeWrap(ch.node, eArgsNew))(breakOut), currentPath + "/"
            )
            val restNew = rest.map(nn => nodeArgLens.set(nn)(eArgsNew))
            debugger.printOnProgressCount("calling enumerateXsd at XsdNamedElement", 1000) // DEBUG
            enumerateXsd(
              restNew ++ newNodes.map(nn => (nn._1, nn._2, refNodesVisited)),
              newElementData ::: pathData
            )
          case XsdNamedAttribute(label, eArgsNew) =>
            //TODO probably need a better way to look up namespaces
            val newElementData =
              if (node.child.isEmpty)
                List((cleanXpath(currentPath), node.node.attributes.asAttrMap.getOrElse("type", "DEBUG")))
              else node.child.find(child => child.node.fullName === "xs:simpleType") match {
                case Some(smplTpeNode) => smplTpeNode.node.child
                  .find(gchild => gchild.node.fullName === "xs:restriction") match {
                  case Some(baseTpeNode) => List((
                    cleanXpath(currentPath),
                    baseTpeNode.attributes.asAttrMap.getOrElse("base", "asdf")
                  ))
                  case None => Nil
                }
                case None => Nil
              }
            val restNew = rest.map(nn => nodeArgLens.modify(nn)(updateAttribs(_, label -> node)))
            debugger.printOnProgressCount("calling enumerateXsd at XsdNamedAttribute", 1000) // DEBUG
            enumerateXsd(restNew, newElementData ::: pathData)
          case XsdUnion(Success(refNode)) =>
            // Similar to default case except we add new nodes from reference
            val newNodes = pathifyXsdNodes(node.child.toList, currentPath) ++
              pathifyXsdNodes(refNode.child.toList, currentPath)
            if (currentPath === "/") {
              println("calling enumerateXsd at XsdUnion")
            } // DEBUG
            enumerateXsd(
              rest ++ newNodes.map(nn => (nn._1, nn._2, refNodesVisited)), pathData
            )
          case XsdNonLocalElement(label, nodeMaybe) => nodeMaybe match {
            case Success(refNodeMaybe) =>
              if (refNodesVisited.contains(refNodeMaybe.node)) {
                debugger.printOnProgressCount("calling enumerateXsd at XsdNonLocalElement", 1000) // DEBUG
                // Skip adding refnode's children; recursive path
                enumerateXsd(rest, (cleanXpath(currentPath), "recursive!") :: pathData)
              }
              else {
                refNodeMaybe match {
                  case badNode: FilteredNode =>
                    // Hit a filtered node; do not include
                    val newNodes = pathifyXsdNodes(node.child.toList, currentPath)
                    debugger.printOnProgressCount("calling filtered node", 1000) // DEBUG
                    enumerateXsd(
                      rest ++ newNodes.map(nn => (nn._1, nn._2, refNodesVisited)), pathData
                    )
                  case refNode: NodeWrap =>
                    val newNamedElems: List[XpathNodeMapEntry] =
                      if (xsdNamedTypes.contains(refNode.node.fullName) &&
                        refNode.node.attributes.asAttrMap.contains("name")) {
                        // This will be a named element that forwards to some other node
                        List(label -> refNode)
                      }
                      else Nil
                    val newElementData =
                      List((cleanXpath(currentPath),
                        refNode match {
                          case _ if refNode.node.attributes.asAttrMap.contains("type") =>
                            refNode.node.attributes.asAttrMap("type")
                          case _ => refNode.node.child.headOption match {
                            case Some(child) if child.fullName === "xs:restriction" =>
                              child.attributes.asAttrMap.getOrElse("base", "asdf")
                            case _ => ""
                          }
                        }
                      )).filter(ne => !nonEmpty || ne._2.nonEmpty) :::
                        (if (refNode.node.attributeVal("mixed").getOrElse("false").toBoolean)
                          List((cleanXpath(currentPath), "xs:string"))
                        else Nil)
                    val eArgsNew = updateElems(refNode.eArgs, newNamedElems: _*)
                    val newNodes = pathifyXsdNodes(
                      refNode.child.map(ch => NodeWrap(ch.node, eArgsNew))(breakOut),
                      currentPath + "/"
                    )
                    val restNew = rest.map(nn => nodeArgLens.set(nn)(eArgsNew))
                    // Continue with refnode's children instead
                    debugger.printOnProgressCount("calling enumerateXsd at XsdNonLocalElement (2)", 1000) // DEBUG
                    enumerateXsd(
                      restNew ++ newNodes.map(nn => (nn._1, nn._2, refNode.node :: refNodesVisited)),
                      newElementData ::: pathData
                    )
                }
              }
            case Failure(e) => //TODO: narrow this down to appropriate error
              debugger.printOnProgressCount("calling enumerateXsd at XsdNonLocalElement (3)", 1000) // DEBUG
              // Not ready yet, let's try again later:
              enumerateXsd(rest ++ List((node, currentPath, refNodesVisited)), pathData)
          }
          case _ =>
            // Default; no path change
            val newNodes = pathifyXsdNodes(node.child.toList, currentPath)
            debugger.printOnProgressCount("calling enumerateXsd at Default", 1000) // DEBUG
            enumerateXsd(
              rest ++ newNodes.map(nn => (nn._1, nn._2, refNodesVisited)), pathData
            )
        }
      case Nil => pathData
    }
  }

  def enumerate(
    nonEmpty: Boolean,
    newNodeFilters: NodeFilters
  ): List[(String, String)] = {
    this.nonEmpty = nonEmpty
    implicit val nodeFilters: NodeFilters = newNodeFilters
    val initArgs = EnumArgs(Map.empty, Map.empty, Map.empty, Map.empty)
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
  //TODO: probably this is a bad idea due to potential name clash between types/groups:
  val xsdNamedTypes = List("xs:simpleType", "xs:complexType", "xs:attributeGroup", "xs:group")
  val xsdUnions = List("xs:attributeGroup", "xs:group", "xs:extension")
  val xsdDataNodes: List[String] = xsdElems ::: xsdAttribs


  def isLocallyDefined(arg: Node) =
    arg.attributeVal("ref").isEmpty && (
      arg.attributeVal("type").isEmpty ||
      arg.attributeVal("type").getOrElse("").startsWith("xs:")
    )

}

