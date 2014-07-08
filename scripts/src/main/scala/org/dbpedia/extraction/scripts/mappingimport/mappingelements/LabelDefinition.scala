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


  def canEqual(other: Any): Boolean = other.isInstanceOf[LabelDefinition]

  override def equals(other: Any): Boolean = other match {
    case that: LabelDefinition =>
      (that canEqual this) &&
        languageCode == that.languageCode &&
        labelText == that.labelText
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(languageCode, labelText)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
