package org.dbpedia.extraction.scripts.mappingimport.mappingelements

import MappingElementList.wrapMappingElementList

/**
 * Created by Daniel Fleischhacker <daniel@informatik.uni-mannheim.de> on 7/4/2014.
 */
class FullCommentsDefinition(val labels: List[CommentDefinition]) extends MappingElement {
  override def toString(): String = {
    labels.mkString("Comments: ", ",", "")
  }

  override def getMappingSyntax(): String = {
    labels.mkMappingString("comments =\n\t", "\n\t", "")
  }
}
