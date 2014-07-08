package org.dbpedia.extraction.scripts.mappingimport.mappingelements

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

/**
 * Utility class providing a mkString equivalent for getting the mapping syntax of {@code MappingElement} lists.
 *
 * @author Daniel Fleischhacker <daniel@informatik.uni-mannheim.de>
 */
class MappingElementList(val elements: List[MappingElement]) extends MappingElement {
  def mkMappingString(start: String, sep: String, end: String): String = {
    val sb = new StringBuilder()
    var first = true

    sb.append(start)
    for (x <- elements) {
      if (first) {
        sb.append(x.getMappingSyntax())
        first = false
      }
      else {
        sb.append(sep)
        sb.append(x.getMappingSyntax())
      }
    }

    sb.append(end)

    sb.toString()
  }

  def mkMappingString(sep: String): String = {
    mkMappingString("", sep, "")
  }

  /**
   * Return this element represented in the mapping Wiki syntax
   *
   * @return mapping Wiki syntax representation of this element
   */
  override def getMappingSyntax(): String = {
    mkMappingString("\n")
  }

  override def toString(): String = {
    elements.mkString("\n")
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[MappingElementList]

  override def equals(other: Any): Boolean = other match {
    case that: MappingElementList =>
      (that canEqual this) &&
        elements == that.elements
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(elements)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object MappingElementList {
  implicit def wrapMappingElementList(elements: List[MappingElement]) = new MappingElementList(elements)
  implicit def wrapMappingElementList(elements: ListBuffer[MappingElement]) = new MappingElementList(elements.toList)
}