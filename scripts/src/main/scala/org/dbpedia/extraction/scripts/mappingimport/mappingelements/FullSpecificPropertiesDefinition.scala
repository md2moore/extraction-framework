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
}
