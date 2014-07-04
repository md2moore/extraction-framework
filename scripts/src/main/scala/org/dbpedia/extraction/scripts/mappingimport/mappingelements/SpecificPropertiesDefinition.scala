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
}
