package org.dbpedia.extraction.scripts.mappingimport

import java.io.{Reader, StringReader}

import org.dbpedia.extraction.scripts.mappingimport.mappingelements._

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

import scala.language.implicitConversions

/**
 * Provides methods to parse the ontology definition syntax from the mappings Wiki and transfer it into a representation
 * consisting of [[org.dbpedia.extraction.scripts.mappingimport.mappingelements.MappingElement]] objects.
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
    { case lc ~ "|" ~ ac => new CommentDefinition(lc, ac)}

  def actualComment = rep(commentText | bracketEnclosedComment) ^^ {
    _.mkString("")
  }

  def commentText = """[^}{]+""".r

  def bracketEnclosedComment: Parser[String] = "{" ~ actualComment ~ "}" ^^
    { case "{" ~ content ~ "}" => "{" + content.toString + "}"}

  // general predicate definitions, i.e., predicateName = value1[, value2, ...]
  def generalPredicateDefinition = predicateName ~ "=" ~ repsep(predicateTextObject, ",") ^^ {
    case name ~ "=" ~ values => val listBuffer = ListBuffer[String]()
      listBuffer ++= values
      new GeneralPredicateDefinition(name, listBuffer)
  }

  def predicateName = """[^=|\s]*""".r

  def predicateTextObject = """[^,}\n]*""".r ^^ {
    _.toString
  }

  def htmlCommentDefinition = """<!--.*?-->""".r ^^ { c =>
    new StringElement(c.toString)
  }

  /*
    Elements used in Class templates
   */
  def classTemplate = templateStartTag ~ "Class" ~ "|" ~> repsep(classTemplateContents, "|") <~ templateEndTag ^^
    {
      case contents: List[MappingElement] => val buffer = new ListBuffer[MappingElement]()
      buffer ++= contents
      new ClassDefinition(buffer)
    }

  def classTemplateContents = labelsDefinition | specificPropertiesDefinition | commentsDefinition |
    generalPredicateDefinition | htmlCommentDefinition

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
      {
        case contents: List[MappingElement] => val buffer = new ListBuffer[MappingElement]()
        buffer ++= contents
        new ObjectPropertyDefinition(buffer)
      }

  def objectPropertyTemplateContents = labelsDefinition | commentsDefinition | generalPredicateDefinition |
    htmlCommentDefinition

  /*
    Elements used in DatatypeProperty templates
   */
  def datatypePropertyTemplate =
    templateStartTag ~ "DatatypeProperty" ~ "|" ~> repsep(datatypePropertyTemplateContents, "|") <~ templateEndTag ^^
      {
        case contents: List[MappingElement] => val buffer = new ListBuffer[MappingElement]()
        buffer ++= contents
        new DatatypePropertyDefinition(buffer)
      }

  def datatypePropertyTemplateContents = labelsDefinition | commentsDefinition | generalPredicateDefinition |
    htmlCommentDefinition

  /**
   * Wraps a string in a string reader. Mainly used while testing parsers on strings
   * @param in string value to wrap in string reader
   * @return string reader containing the given string value
   */
  implicit def wrapString(in: String): Reader = new StringReader(in)

  /**
   * Tries to parse the given string as a ontology element definition represented in the Mappings wiki syntax.
   *
   * If `skipNonRelevant` is true, first the start of a class, object property or datatype property tag
   * is searched. If such a tag is found, all text occurring before this tag is cut away and preserved as a
   * [[org.dbpedia.extraction.scripts.mappingimport.mappingelements.StringElement]]. Afterwards the remaining
   * text is parsed and all text after the completely parsed text is again preserved as a StringElement. All
   * resulting elements are aggregated in a
   * [[org.dbpedia.extraction.scripts.mappingimport.mappingelements.MappingElementList]] object.
   *
   * If `skipNonRelevant` is false, the full `content` is parsed directly and the result is returned.
   *
   * @param content string content to parse
   * @param skipNonRelevant if true, the parser tries to find the first relevant template in the given input
   * @return result of the parsing run
   */
  def parse(content: String, skipNonRelevant: Boolean = true): Option[MappingElement] = {
    if (skipNonRelevant) {
      var index = indexOf(content, """\{\{\s*?Class""".r)
      if (index == -1) {
        index = indexOf(content, """\{\{\s*?ObjectProperty""".r)
      }
      if (index == -1) {
        index = indexOf(content, """\{\{\s*?DatatypeProperty""".r)
      }
      if (index == -1) {
        throw new ParseException("Unable to find relevant part in content:\n" + content)
      }
      val firstPart = content.substring(0, index)
      val contentReader = new StringReader(content.substring(index))
      val parseResult = parse(rootNode, contentReader)
      parseResult match {
        case Success(res, i) => Some(new
            MappingElementList(
              List(new StringElement(firstPart), res, new StringElement(content.substring(index + i.offset).trim))))
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

  /**
   * Returns the first index in `content` at which the regular expression `regexp` is found.
   * If the given expression is not contained, -1 is returned.
   *
   * @param content content in which the expression is searched
   * @param regexp regular expression to search
   */
  def indexOf(content: String, regexp: Regex): Int = {
    val matcher = regexp.findFirstMatchIn(content)
    if (matcher.isEmpty) {
      -1
    }
    else {
      matcher.get.start
    }
  }

  def main(args: Array[String]) {
    val parseResult = parse( """{{ObjectProperty
                               || labels =
                               |{{label|en|mentor}}
                               |{{label|de|Mentor}}
                               |{{label|fr|maître}}
                               |{{label|fr|mentor}}
                               || rdfs:domain = Artist
                               || rdfs:range = Artist
                               || comments =
                               |{{comment|en|A wise and trusted counselor or teacher<ref>http://en.wiktionary
                               |.org/wiki/mentor</ref>}}
                               |{{comment|fr|Celui qui sert de guide, de conseiller à quelqu’un. <ref>http://fr
                               |.wiktionary.org/wiki/mentor</ref>}}
                               |}}
                               |
                               |<references/>""".stripMargin)
    println(parseResult)
    println(parseResult.get.getMappingSyntax())
  }
}



















