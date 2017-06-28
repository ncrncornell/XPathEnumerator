package edu.ncrn.cornell.xml

import edu.ncrn.cornell.Util._

import org.specs2.Specification

import scala.xml.Node

/**
  * Created by Brandon Barker on 6/28/17.
  */

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
class NodeEqualitySpec extends Specification { def is = s2"""
 Testing various forms of equality of Nodes
   Passed == check for identically constructed nodes $eqCheckN1N2
   Passed == check for identical nodes               $eqCheckN3N4
   Passed === check for identically constructed nodes $eqCheckN1N2_tc
   Passed === check for identical nodes               $eqCheckN3N4_tc
   Passed ref neq check for identically constructed nodes  $eqCheckN1N2_ref
   Passed ref neq check for identical nodes                $eqCheckN3N4_ref
   Passed != check for obviously different nodes           $obviouslyDiff
  """

  import NodeEqualitySpec._

  val myNode1 = <Outer><Inner>asdf</Inner></Outer>
  val myNode2 = <Outer><Inner>asdf</Inner></Outer>
  val myNode3 = <Inner>asdf</Inner>
  val myNode4 = myNode2.child.head

  val myNode5 = <Something></Something>



  val eqCheckN1N2 = myNode1 == myNode2 must beTrue
  val eqCheckN3N4 = myNode3 == myNode4 must beTrue
  val eqCheckN1N2_tc = carefulEq(myNode1, myNode2) must beTrue
  val eqCheckN3N4_tc = carefulEq(myNode3, myNode4) must beTrue

  val eqCheckN1N2_ref = myNode1 eq myNode2 must beFalse
  val eqCheckN3N4_ref = myNode3 eq myNode4 must beFalse

  val obviouslyDiff = myNode5 != myNode1 must beTrue


}

object NodeEqualitySpec {
  def carefulEq(node1: Node, node2: Node): Boolean = node1 === node2
}
