package org.dbpedia.extraction.scripts.mappingimport.mappingelements

/**
 * Created by Daniel Fleischhacker <daniel@informatik.uni-mannheim.de> on 7/4/2014.
 */
class LabelDefinition(val languageCode: String, val labelText: String) extends MappingElement {
  override def toString(): String = {
    s"""\"$labelText"@$languageCode"""
  }

  override def getMappingSyntax(): String = {
    s"""{{label|$languageCode|$labelText}}"""
  }
}
