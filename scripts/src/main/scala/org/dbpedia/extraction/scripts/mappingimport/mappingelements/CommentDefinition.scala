package org.dbpedia.extraction.scripts.mappingimport.mappingelements

/**
 * Created by Daniel Fleischhacker <daniel@informatik.uni-mannheim.de> on 7/4/2014.
 */
class CommentDefinition(val languageCode: String, val commentText: String) extends MappingElement {
  override def toString(): String = {
    s"""\"$commentText"@$languageCode"""
  }

  override def getMappingSyntax(): String = {
    s"""{{comment|$languageCode|$commentText}}"""
  }


  def canEqual(other: Any): Boolean = other.isInstanceOf[CommentDefinition]

  override def equals(other: Any): Boolean = other match {
    case that: CommentDefinition =>
      (that canEqual this) &&
        languageCode == that.languageCode &&
        commentText == that.commentText
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(languageCode, commentText)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
