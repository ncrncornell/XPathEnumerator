package edu.ncrn.cornell.xml

import scala.xml.Node
import shapeless._

import EnumArgs._

/**
  *
  * @param namedAttributes
  * @param namedElements
  * @param namedTypes
  * @param filteredElements
  */
final case class EnumArgs(
  namedAttributes: XpathNodeMap,
  namedElements: XpathNodeMap,
  namedTypes: XpathNodeMap,
  filteredElements: FilteredElements
)

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
object EnumArgs {
  type NodeRemaining = (NodeWrap, String, List[Node])
  type XpathNodeMapEntry = (String, NodeWrap)
  type XpathNodeMap = Map[String, NodeWrap]
  type FilteredElementEntry = (String, Node)
  type FilteredElements = Map[String, Node]

  private val namedAttribLens = lens[EnumArgs] >> 'namedAttributes
  private val namedElementsLens = lens[EnumArgs] >> 'namedElements
  private val namedTypesLens = lens[EnumArgs] >> 'namedTypes
  private val filteredElementsLens = lens[EnumArgs] >> 'filteredElements

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

  def updateFiltered(enumArgs: EnumArgs, newFilteredElems: FilteredElementEntry*) =
    if (newFilteredElems.nonEmpty)
      filteredElementsLens.set(enumArgs)(filteredElementsLens.get(enumArgs) ++ newFilteredElems)
    else enumArgs

}