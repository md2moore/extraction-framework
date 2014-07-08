package org.dbpedia.extraction.scripts.mappingimport.mappingelements


import scala.collection.mutable.ListBuffer

/**
 * Created by Daniel Fleischhacker <daniel@informatik.uni-mannheim.de> on 7/4/2014.
 */
class GeneralPredicateDefinition(val predicateName: String, val values: ListBuffer[String]) extends MappingElement {
  override def toString(): String = {
    predicateName + " = " + values.mkString(", ")
  }

  override def getMappingSyntax(): String = {
    predicateName + " = " + values.mkString(", ")
  }


  def canEqual(other: Any): Boolean = other.isInstanceOf[GeneralPredicateDefinition]

  override def equals(other: Any): Boolean = other match {
    case that: GeneralPredicateDefinition =>
      (that canEqual this) &&
        predicateName == that.predicateName &&
        values == that.values
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(predicateName, values)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object GeneralPredicateDefinition {
  def unapply(predicateDefinition: GeneralPredicateDefinition): Option[(String, ListBuffer[String])] = Some(
    (predicateDefinition.predicateName, predicateDefinition.values))
}