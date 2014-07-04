package org.dbpedia.extraction.scripts.mappingimport.mappingelements

/**
 * Represents an element which can be expressed in the mappings Wiki syntax.
 *
 * @author Daniel Fleischhacker <daniel@informatik.uni-mannheim.de>
 */
trait MappingElement {
  /**
   * Return this element represented in the mapping Wiki syntax
   *
   * @return mapping Wiki syntax representation of this element
   */
  def getMappingSyntax() : String
}
