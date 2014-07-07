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
}

object GeneralPredicateDefinition {
  def unapply(predicateDefinition: GeneralPredicateDefinition): Option[(String, ListBuffer[String])] = Some(
    (predicateDefinition.predicateName, predicateDefinition.values))
}