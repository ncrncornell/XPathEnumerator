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
   ddiExample1 removed correctly                          $ddiExample1Sub
   ddiExample2 removed correctly                          $ddiExample2Sub
                                 """

  val oneIndex = "/shiporder/item[1]/quantity"
  val twoIndices = "/shiporder/item[1]/quantity[2]"
  val indicesRemoved = "/shiporder/item/quantity"

  def oneIndexSub = toWildCard(List(oneIndex)) === Set(indicesRemoved)
  def twoIndicesSub = toWildCard(List(twoIndices)) === Set(indicesRemoved)

  val condenseTwo = toWildCard(List(oneIndex, twoIndices))
  def condenseTwoToOne = condenseTwo.size must_=== 1
  def condenseTwoCorrect = condenseTwo.head must_=== indicesRemoved

  val ddiExample1 = "/codeBook/stdyDscr/stdyInfo/subject/keyword[25]"
  val ddiExample2 = "/codeBook/stdyDscr/stdyInfo/subject/keyword[14]/@vocab"
  val ddiExample1Ir = "/codeBook/stdyDscr/stdyInfo/subject/keyword"
  val ddiExample2Ir= "/codeBook/stdyDscr/stdyInfo/subject/keyword/@vocab"

  def ddiExample1Sub = toWildCard(List(ddiExample1)) === Set(ddiExample1Ir)
  def ddiExample2Sub = toWildCard(List(ddiExample2)) === Set(ddiExample2Ir)



}
