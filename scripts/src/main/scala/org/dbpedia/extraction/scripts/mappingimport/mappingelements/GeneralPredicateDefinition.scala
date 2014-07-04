package org.dbpedia.extraction.scripts.mappingimport.mappingelements

/**
 * Created by Daniel Fleischhacker <daniel@informatik.uni-mannheim.de> on 7/4/2014.
 */
class GeneralPredicateDefinition(val predicateName: String, val values: List[String]) extends MappingElement {
  override def toString(): String = {
    predicateName + " = " + values.mkString(", ")
  }

  override def getMappingSyntax(): String = {
    predicateName + " = " + values.mkString(", ")
  }
}
