package edu.ncrn.cornell.xml

import org.specs2.Specification

/**
  * Created by Brandon on 9/25/2016.
  */

import Util._

class UtilSpec extends Specification { def is = s2"""

 Testing Util functions
   ToWildCard removes one index                           $oneIndexSub
   ToWildCard removes two indices                         $twoIndicesSub
   ToWildCard removed duplicate                           $condenseTwoToOne
   ToWildCard recuded correctly                           $condenseTwoCorrect
                                 """



  val oneIndex = "/shiporder/item[1]/quantity"
  val twoIndices = "/shiporder/item[1]/quantity[2]"
  val indicesRemoved = "/shiporder/item/quantity"

  def oneIndexSub = toWildCard(oneIndex) must_== indicesRemoved
  def twoIndicesSub = toWildCard(twoIndices) must_== indicesRemoved

  val condenseTwo = toWildCard(List(oneIndex, twoIndices))
  def condenseTwoToOne = condenseTwo.size must_== 1
  def condenseTwoCorrect = condenseTwo.head must_== indicesRemoved
}
