package org.dbpedia.extraction.scripts.mappingimport.mappingelements

/**
 * Created by Daniel Fleischhacker <daniel@informatik.uni-mannheim.de> on 7/4/2014.
 */
class CommentDefinition(val languageCode: String, val commentText: String) extends MappingElement {
  override def toString(): String = {
    s"""\"$commentText"@$languageCode"""
  }

  override def getMappingSyntax(): String = {
    s"""{{label|$languageCode|$commentText}}"""
  }
}
