package edu.ncrn.cornell.xml

/**
  * Created by Brandon on 9/25/2016.
  */
object Util {

  val elementIndex= "\\[\\d\\]".r

  /**
    * Transforms parameterized or indexed XPaths to wildcard XPaths
    */
  def toWildCard(xpath: String): String = elementIndex.replaceAllIn(xpath, "")


  /**
    * The same as the singleton variant, but removes resulting duplicates
    */
  def toWildCard(xpaths: Iterable[String]): Iterable[String] = xpaths.map{x =>
    toWildCard(x)
  }.toSet
}
