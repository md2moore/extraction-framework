package org.dbpedia.extraction.scripts.mappingimport.mappingelements

/**
 * Created by Daniel Fleischhacker <daniel@informatik.uni-mannheim.de> on 7/4/2014.
 */
class SpecificPropertiesDefinition(val ontologyProperty: String, val unit: String) extends MappingElement {
  override def toString(): String = {
    s"""$ontologyProperty specified with type $unit"""
  }

  override def getMappingSyntax(): String = {
    s"""{{SpecificProperty | ontologyProperty = $ontologyProperty | unit = $unit }}"""
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[SpecificPropertiesDefinition]

  override def equals(other: Any): Boolean = other match {
    case that: SpecificPropertiesDefinition =>
      (that canEqual this) &&
        ontologyProperty == that.ontologyProperty &&
        unit == that.unit
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(ontologyProperty, unit)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
