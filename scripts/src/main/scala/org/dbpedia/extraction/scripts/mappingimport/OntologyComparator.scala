package org.dbpedia.extraction.scripts.mappingimport

import org.dbpedia.extraction.ontology.{OntologyDatatypeProperty, OntologyObjectProperty, OntologyEntity, OntologyClass}

/**
 * Utility object to compare ontology objects as generated by the extraction framework when parsing the Wiki syntax.
 */
object OntologyComparator {
  /**
   * Compares the two given ontology entities regarding equality. To be equal the two entities must have the same
   * lists of related entities (e.g., same base classes, ranges etc.). For comparing these lists, the method resorts
   * to the default ontology entity comparison based on the entity names.
   *
   * @param a first entity
   * @param b second entity
   * @return true if entities are the same, otherwise false
   */
  def equals(a: OntologyEntity, b: OntologyEntity) : Boolean = {
    (a,b) match {
      case (a: OntologyClass, b: OntologyClass) =>
        a.baseClasses == b.baseClasses &&
        a.disjointWithClasses == b.disjointWithClasses &&
        a.equivalentClasses == b.equivalentClasses &&
        a.isExternalClass == b.isExternalClass &&
        a.uri == b.uri &&
        a.relatedClasses == b.relatedClasses &&
        a.comments == b.comments &&
        a.labels == b.labels &&
        a.name == b.name
      case (a: OntologyObjectProperty, b: OntologyObjectProperty) =>
        a.range == b.range &&
        a.uri == b.uri &&
        a.domain == b.domain &&
        a.name == b.name &&
        a.comments == b.comments &&
        a.equivalentProperties == b.equivalentProperties &&
        a.isExternalProperty == b.isExternalProperty &&
        a.isFunctional == b.isFunctional
      case (a:OntologyDatatypeProperty, b:OntologyDatatypeProperty) =>
        a.range == b.range &&
        a.uri == b.uri &&
        a.domain == b.domain &&
        a.name == b.name &&
        a.comments == b.comments &&
        a.equivalentProperties == b.equivalentProperties &&
        a.isExternalProperty == b.isExternalProperty &&
        a.isFunctional == b.isFunctional
      case _ => false
    }
  }
}
