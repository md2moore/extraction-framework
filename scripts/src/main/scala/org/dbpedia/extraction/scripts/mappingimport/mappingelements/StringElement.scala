package org.dbpedia.extraction.scripts.mappingimport.mappingelements

/**
 * Created by Daniel Fleischhacker (daniel@informatik.uni-mannheim.de) on 7/4/2014.
 */
class StringElement(val content: String) extends MappingElement{
  /**
   * Return this element represented in the mapping Wiki syntax
   *
   * @return mapping Wiki syntax representation of this element
   */
  override def getMappingSyntax(): String = content

  override def toString() : String = content

  def canEqual(other: Any): Boolean = other.isInstanceOf[StringElement]

  override def equals(other: Any): Boolean = other match {
    case that: StringElement =>
      (that canEqual this) &&
        content == that.content
    case _ => false
  }

  override def hashCode(): Int = {
    content.hashCode
  }
}
