package org.dbpedia.extraction.scripts.mappingimport.mappingelements

import MappingElementList.wrapMappingElementList

import scala.collection.mutable.ListBuffer

/**
 * Created by Daniel Fleischhacker <daniel@informatik.uni-mannheim.de> on 7/4/2014.
 */
class DatatypePropertyDefinition(templateContents: ListBuffer[MappingElement]) extends RootNode(templateContents) {
  override def toString(): String = {
    templateContents.mkString("\t", "\n\t", "\n")
  }

  override def getMappingSyntax(): String = {
    templateContents.mkMappingString("{{DatatypeProperty\n| ", "\n| ", "\n}}")
  }
}
