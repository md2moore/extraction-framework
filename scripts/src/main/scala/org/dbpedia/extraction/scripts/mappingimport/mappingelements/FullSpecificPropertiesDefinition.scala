package org.dbpedia.extraction.scripts.mappingimport.mappingelements

import MappingElementList.wrapMappingElementList

/**
 * Created by Daniel Fleischhacker <daniel@informatik.uni-mannheim.de> on 7/4/2014.
 */
class FullSpecificPropertiesDefinition(val specificProperties: List[SpecificPropertiesDefinition])
  extends MappingElement {
  override def toString(): String = {
    specificProperties.mkString("Specific Properties: ", ",", "")
  }

  override def getMappingSyntax(): String = {
    val prefix = "specificProperties = "
    // we try to indent all definitions following the first one so that they match the first one
    specificProperties.mkMappingString(prefix, "\n" + (" " * (prefix.length + 2)), "")
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[FullSpecificPropertiesDefinition]

  override def equals(other: Any): Boolean = other match {
    case that: FullSpecificPropertiesDefinition =>
      (that canEqual this) &&
        specificProperties == that.specificProperties
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(specificProperties)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
