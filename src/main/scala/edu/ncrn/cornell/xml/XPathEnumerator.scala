package edu.ncrn.cornell.xml
import scala.annotation.tailrec

import scala.xml.Node

/**
  * @author Brandon Barker
  *         9/8/2016
  */
trait XPathEnumerator {

  //var XmlFile = ???

  @tailrec
  final def uniqueXpaths(
    nodes: Seq[Node], pathData: List[(String, Option[String])] = Nil
  ):  List[(String, Option[String])] = nodes match {
    case nn +: rest =>
      val newElementData =
        if(nn.child.isEmpty){
          if (nn.text == "") (pathData.head._1 + nn.label, Some(""))
          else (pathData.head._1 + nn.label + " ", Some(nn.text))
        }
        else if (pathData.nonEmpty) (pathData.head._1 + nn.label + "/", None)
        else ("/", None)
      val newAttributeNodes = nn.attributes.asAttrMap.keys.flatMap{kk =>
        nn.attribute(kk)
      }.flatten.toSeq
      uniqueXpaths(rest ++ newAttributeNodes ++ nn.child, newElementData :: pathData)
    case Seq() => pathData
  }

}

object XPathEnumerator{

  //  val elementWildcard = "\\[\\*\\]".r
  //  val attributeWildcard = "(.+)\\[@(.+)\\]".r
  //  val wildcard = "\\*".r

  //def apply() ...

  val x = <div class="content"><a></a><p><q>hello</q></p><r><p>world</p></r><s></s></div>

}

