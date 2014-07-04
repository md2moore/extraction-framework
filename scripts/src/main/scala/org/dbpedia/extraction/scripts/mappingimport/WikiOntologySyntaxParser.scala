package org.dbpedia.extraction.scripts.mappingimport

import java.io.{Reader, StringReader}

import org.dbpedia.extraction.scripts.mappingimport.mappingelements._

import scala.util.parsing.combinator.RegexParsers

import scala.language.implicitConversions

/**
 * Provides methods to parse the ontology definition syntax from the mappings Wiki and transfer it into a representation
 * consisting of {@code MappingElement} objects.
 *
 * @author Daniel Fleischhacker <daniel@informatik.uni-mannheim.de>
 */
object WikiOntologySyntaxParser extends RegexParsers {
  // we only support the templates used for ontology definitions
  def rootNode = classTemplate | datatypePropertyTemplate | objectPropertyTemplate

  // general character sequences
  def templateStartTag = "{{"

  def templateEndTag = "}}"

  def languageCode = """[a-z]{2,3}""".r

  /*
    General elements used in multiple templates
   */
  // label template
  def labelsDefinition = "labels" ~ "=" ~> rep(singleLabelDefinition) ^^
    { case labels: List[LabelDefinition] => new FullLabelsDefinition(labels)}

  def singleLabelDefinition = "{{" ~ "label" ~ "|" ~> languageCode ~ "|" ~ actualLabel <~ "}}" ^^
    { case lc ~ "|" ~ al => new LabelDefinition(lc, al)}

  def actualLabel = """[^}]*""".r

  // comment template
  def commentsDefinition = "comments" ~ "=" ~> rep(singleCommentDefinition) ^^
    { case comments: List[CommentDefinition] => new FullCommentsDefinition(comments)}

  def singleCommentDefinition = "{{" ~ "comment" ~ "|" ~> languageCode ~ "|" ~ actualComment <~ "}}" ^^
    { case lc ~ "|" ~ ac  => new CommentDefinition(lc, ac)}

  def actualComment =  rep(commentText | bracketEnclosedComment) ^^ {_.mkString("")}
  def commentText = """[^}{]+""".r
  def bracketEnclosedComment : Parser[String] = "{" ~ actualComment ~ "}" ^^ {case "{"~content~"}" => "{"+content.toString+"}"}

  // general predicate definitions, i.e., predicateName = value1[, value2, ...]
  def generalPredicateDefinition = predicateName ~ "=" ~ repsep(predicateTextObject, ",") ^^
    { case name ~ "=" ~ values => new GeneralPredicateDefinition(name, values)}

  def predicateName = """[^=|\s]*""".r

  def predicateTextObject = """[^,\s}]*""".r ^^ {
    _.toString
  }

  /*
    Elements used in Class templates
   */
  def classTemplate = templateStartTag ~ "Class" ~ "|" ~> repsep(classTemplateContents, "|") <~ templateEndTag ^^
    { case contents: List[MappingElement] => new ClassDefinition(contents)}

  def classTemplateContents = labelsDefinition | specificPropertiesDefinition | commentsDefinition |
    generalPredicateDefinition

  // specific properties. this one is not a general predicate since it contains additional templates
  def specificPropertiesDefinition = "specificProperties" ~ "=" ~> rep(singleSpecificPropertyDefinition) ^^
    { case props: List[SpecificPropertiesDefinition] => new FullSpecificPropertiesDefinition(props)}

  def singleSpecificPropertyDefinition = "{{" ~ "SpecificProperty" ~ "|" ~ "ontologyProperty" ~ "=" ~>
    ontologyPropertyName ~ "|" ~ "unit" ~ "=" ~ unitName <~ "}}" ^^
    { case propertyName ~ "|" ~ "unit" ~ "=" ~ unitName => new SpecificPropertiesDefinition(propertyName, unitName)}

  def ontologyPropertyName = """[^ |]*""".r

  def unitName = """[^ }]*""".r

  /*
    Elements used in ObjectProperty templates
   */
  def objectPropertyTemplate =
    templateStartTag ~ "ObjectProperty" ~ "|" ~> repsep(objectPropertyTemplateContents, "|") <~ templateEndTag ^^
      { case contents: List[MappingElement] => new ObjectPropertyDefinition(contents)}

  def objectPropertyTemplateContents = labelsDefinition | commentsDefinition | generalPredicateDefinition

  /*
    Elements used in DatatypeProperty templates
   */
  def datatypePropertyTemplate =
    templateStartTag ~ "DatatypeProperty" ~ "|" ~> repsep(datatypePropertyTemplateContents, "|") <~ templateEndTag ^^
      { case contents: List[MappingElement] => new DatatypePropertyDefinition(contents)}

  def datatypePropertyTemplateContents = labelsDefinition | commentsDefinition | generalPredicateDefinition

  /**
   * Wraps a string in a string reader. Mainly used while testing parsers on strings
   * @param in string value to wrap in string reader
   * @return string reader containing the given string value
   */
  implicit def wrapString(in: String): Reader = new StringReader(in)

  /**
   * Tries to parse the given string as a ontology element definition represented in the Mappings wiki syntax.
   *
   * @param content string content to parse
   * @param skipNonRelevant if true, the parser tries to find the first relevant template in the given input
   * @return result of the parsing run
   */
  def parse(content: String, skipNonRelevant : Boolean = true) : Option[MappingElement] = {
    if (skipNonRelevant) {
      var result = Option.empty
      var index = content.indexOf("{{Class")
      if (index == -1) {
        index = content.indexOf("{{ObjectProperty")
      }
      if (index == -1) {
        index = content.indexOf("{{DatatypeProperty")
      }
      val firstPart = content.substring(0, index)
      val contentReader = new StringReader(content.substring(index))
      val parseResult = parse(rootNode, contentReader)
      parseResult match {
        case Success(res, i) => Some(new
            MappingElementList(List(new StringElement(firstPart), res, new StringElement(content.substring(index + i.offset)))))
        case _ => Option.empty
      }

    }
    else {
      val parsingResult = parseAll(rootNode, new StringReader(content))
      if (parsingResult.isEmpty) {
        Option.empty
      }
      else {
        Some(parsingResult.get)
      }
    }
  }

  def main(args: Array[String]) {
    val parseResult = parse("""'''{{Reserved for DBpedia}}'''
                                           |
                                           |{{DatatypeProperty
                                           || labels =
                                           |{{label|en|has abstract}}
                                           |{{label|el|έχει περίληψη}}
                                           |{{label|sr|апстракт}}
                                           || comments =
                                           |{{comment|en|Reserved for DBpedia. {{Reserved for DBpedia}}}}
                                           |{{comment|el|Προορίζεται για την DBpedia. {{Reserved for DBpedia}}}}
                                           || rdfs:range = rdf:langString
                                           |
                                           |
                                           |
                                           |
                                           |
                                           |}}""".stripMargin)
    println(parseResult)
    println(parseResult.get.getMappingSyntax())
  }
}



















