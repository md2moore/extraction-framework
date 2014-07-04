package org.dbpedia.extraction.scripts.mappingimport.mappingelements

import MappingElementList.wrapMappingElementList

/**
 * Created by Daniel Fleischhacker <daniel@informatik.uni-mannheim.de> on 7/4/2014.
 */
class FullLabelsDefinition(val labels: List[LabelDefinition]) extends MappingElement {
  override def toString(): String = {
    labels.mkString("Labels: ", ",", "")
  }

  override def getMappingSyntax(): String = {
    labels.mkMappingString("labels=\n", "\n", "")
  }
}
