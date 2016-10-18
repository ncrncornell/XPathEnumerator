package edu.ncrn.cornell.xml

import scala.collection.mutable
/**
  * @author Brandon Barker
  *         10/18/2016
  */

/**
  * This class is intended to be used to help with debugging XpathXsdEnumerator
  * or its variants; it is not part of the public API.
  */
@SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures",
  "org.wartremover.warts.NonUnitStatements"
))
private[xml] class XsdDebugger {
  val pathCounter: mutable.Map[String, Int] = mutable.Map()

  def addPath(xpath: String): Unit = {
    if (pathCounter.contains(xpath)) {
      val currentCount: Int = pathCounter(xpath)
      if (currentCount > 200) {
        println(s"$xpath count is $currentCount")
      }
      pathCounter += (xpath -> (currentCount + 1))
    } else pathCounter += (xpath -> 0)

  }

}
