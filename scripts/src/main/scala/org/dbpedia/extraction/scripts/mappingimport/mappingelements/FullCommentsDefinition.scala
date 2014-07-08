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

  def canEqual(other: Any): Boolean = other.isInstanceOf[FullCommentsDefinition]

  override def equals(other: Any): Boolean = other match {
    case that: FullCommentsDefinition =>
      (that canEqual this) &&
        labels == that.labels
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(labels)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
