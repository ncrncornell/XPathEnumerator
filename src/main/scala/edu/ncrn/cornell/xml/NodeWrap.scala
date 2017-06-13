package edu.ncrn.cornell.xml

import scala.xml.Node

sealed abstract class NodeWrapAbstract(node: Node, eArgs: EnumArgs) {
  // Otherwise recursing over eArgs.toString will result in OOMs in
  // e.g., debuggers
  override def toString = s"node = ${node.toString()}"
}

final case class NodeWrap(node: Node, eArgs: EnumArgs)
  extends NodeWrapAbstract(node, eArgs) {
  def child = node.child.map{ch => NodeWrap(ch, eArgs)}
}

final case class NodeWrapCarry(node: Node, eArgs: EnumArgs, label: String)
  extends NodeWrapAbstract(node, eArgs)
