package edu.ncrn.cornell.xml

import scala.xml.Node

sealed abstract class NodeWrapAbstract(val node: Node, val eArgs: EnumArgs) {
  // Otherwise recursing over eArgs.toString will result in OOMs in
  // e.g., debuggers
  override def toString = s"node = ${node.toString()}"
  def child = node.child.map{ch => NodeWrap(ch, eArgs)}
}

final case class NodeWrap(override val node: Node, override val eArgs: EnumArgs)
  extends NodeWrapAbstract(node, eArgs) {
}

final case class NodeWrapCarry(override val node: Node, override val eArgs: EnumArgs, label: String)
  extends NodeWrapAbstract(node, eArgs)

final case class FilteredNode(override val node: Node, override val eArgs: EnumArgs)
  extends NodeWrapAbstract(node, eArgs)