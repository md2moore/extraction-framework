package org.dbpedia.extraction.scripts.mappingimport.mappingelements

import scala.collection.mutable.ListBuffer

/**
 * @author Daniel Fleischhacker (daniel@informatik.uni-mannheim.de)
 */
abstract class RootNode(val templateContents: ListBuffer[MappingElement]) extends MappingElement {

  def canEqual(other: Any): Boolean = other.isInstanceOf[RootNode]

  override def equals(other: Any): Boolean = other match {
    case that: RootNode =>
      (that canEqual this) &&
        templateContents == that.templateContents
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(templateContents)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
