package org.dbpedia.extraction.scripts.mappingimport.mappingelements

import MappingElementList.wrapMappingElementList

/**
 * Created by Daniel Fleischhacker <daniel@informatik.uni-mannheim.de> on 7/4/2014.
 */
class ClassDefinition(val templateContents: List[MappingElement]) extends MappingElement {
  override def toString(): String = {
    templateContents.mkString("\t", "\n\t", "\n")
  }

  override def getMappingSyntax(): String = {
    templateContents.mkMappingString("{{Class\n| ", "\n| ", "\n}}")
  }
}
