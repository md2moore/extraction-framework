package org.dbpedia.extraction.scripts.mappingimport

import net.sourceforge.jwbf.core.contentRep.Article
import net.sourceforge.jwbf.mediawiki.bots.MediaWikiBot
import org.dbpedia.extraction.ontology.io.OntologyReader
import org.dbpedia.extraction.ontology.{Ontology, RdfNamespace}
import org.dbpedia.extraction.scripts.mappingimport.mappingelements._
import org.dbpedia.extraction.sources.WikiPage
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.wikiparser._

import scala.collection.mutable.ListBuffer
import scala.io.{Codec, Source}

/**
 * Imports mappings from a file into the mappings Wiki using the Mediawiki API.
 *
 * == Current Limitations ==
 * The current implementation is limited to only import the following mapping types into the Wiki:
 * - subClassOf
 * - subPropertyOf
 * However, it is easily possible to extend this to support other mapping types.
 *
 * @param mappingsWikiURL URL of the mapping Wiki to work on
 * @param user user name to authenticate in the wiki
 * @param password password to authenticate in the wiki
 *
 * @author Daniel Fleischhacker <daniel@informatik.uni-mannheim.de>
 */
class ImportMappingsIntoWiki(val mappingsWikiURL: String = Language.Mappings.apiUri, val user: String,
                             val password: String) {
  private val wiki = new MediaWikiBot(mappingsWikiURL)

  // pattern used to find triples in the input file
  private val ntriplesPattern = """^<([^>]+)>\s*<([^>]+)>\s*<([^>]+)>\s*\.$""".r("subject", "predicate", "object")

  // contains all predicate types currently supported by this program and whether they are class or property predicates
  private val classPredicates = Set("http://www.w3.org/2000/01/rdf-schema#subClassOf",
    "http://www.w3.org/2002/07/owl#equivalentClass")
  private val propertyPredicates = Set("http://www.w3.org/2000/01/rdf-schema#subPropertyOf")

  /**
   * Processes the given ntriples file by adding all predicates defined in this file to the wiki pages corresponding to
   * the subjects.
   *
   * @param fileName path of file to process
   */
  def importNTriplesFile(fileName: String) : String = {
    val fileSource = Source.fromFile(fileName)(Codec.UTF8)

    val manualOperationLog = new StringBuilder()

    fileSource.getLines().foreach { line =>
      val matcher = ntriplesPattern.findFirstMatchIn(line)
      matcher match {
        case Some(ntriplesPattern(s, p, o)) if s.startsWith("http://dbpedia.org/ontology") && isClassStatement(p) =>
          addClassDetail(s, p, o, manualOperationLog)
        case Some(ntriplesPattern(s, p, o)) if s.startsWith("http://dbpedia.org/ontology") && isPropertyStatement(p) =>
          addPropertyDetail(s, p, o, manualOperationLog)
        case _ =>
      }
    }

    manualOperationLog.toString()
  }

  /**
   * Adds the triple represented by `s`, `p` and `o` to the wiki page for the class subject `s`.
   *
   * @param s subject of the triple
   * @param p predicate of the triple
   * @param o object of the triple
   */
  private def addClassDetail(s: String, p: String, o: String, manualOperationLog: StringBuilder): Unit = {
    val shortPredicate = RdfNamespace.shortenWithPrefix(p)
    val shortObject = RdfNamespace.shortenWithPrefix(o)
    val className = s.replace("http://dbpedia.org/ontology/", "")
    val article = wiki.getArticle(s"OntologyClass:$className")

    if (article.getText.isEmpty) {
      println(s"No article found for class $className, skipping")
    }
    else if (!canSafelyModify(article)) {
      println(s"Not able to safely modify $className, skipping")
      manualOperationLog.append(
        s"""* add predicate $shortPredicate with content $shortObject to page http://mappings.dbpedia.org/OntologyClass:$className""")
    }
    else {
      println(s"Processing class $className")
      val shortPredicate = RdfNamespace.shortenWithPrefix(p)
      val shortObject = RdfNamespace.shortenWithPrefix(o)
      val elements = WikiOntologySyntaxParser.parse(article.getText)
      val gpd = findGeneralPredicateDefinition(elements.get, shortPredicate)
      gpd match {
        case Some(definition) => if (!definition.values.contains(shortObject)) {
          definition.values += shortObject
        }
        case _ => println("Not found general predicate definition for predicate, creating new one")
          val classDef: ClassDefinition = elements.get match {
            case i: ClassDefinition => i
            case l: MappingElementList => l.elements.collectFirst { case i: ClassDefinition => i}.get
          }
          classDef.templateContents += new GeneralPredicateDefinition(shortPredicate, ListBuffer(shortObject))
      }
      println(elements.get.getMappingSyntax())
    }
  }

  /**
   * Adds the triple represented by `s`, `p` and `o` to the wiki page for the property subject `s`.
   * This method downloads the corresponding article and checks whether the property is an object or a datatype
   * property before calling either [[addObjectPropertyDetail( )]] or [[addDatatypePropertyDetail( )]].
   *
   * @param s subject of the triple
   * @param p predicate of the triple
   * @param o object of the triple
   */
  private def addPropertyDetail(s: String, p: String, o: String, manualOperationLog: StringBuilder): Unit = {
    val shortPredicate = RdfNamespace.shortenWithPrefix(p)
    val shortObject = RdfNamespace.shortenWithPrefix(o)
    val propertyName = s.replace("http://dbpedia.org/ontology/", "")
    val article = wiki.getArticle(s"OntologyProperty:$propertyName")

    if (article.getText.isEmpty) {
      println(s"No article found for class $propertyName, skipping")
    }
    else if (!canSafelyModify(article)) {
      println(s"Not able to safely modify $propertyName, skipping")
      manualOperationLog.append(
        s"""* add predicate $shortPredicate with content $shortObject to page http://mappings.dbpedia.org/OntologyProperty:$propertyName """)
    }
    else {
      println(s"Processing class $propertyName")
      val elements = WikiOntologySyntaxParser.parse(article.getText)
      val gpd = findGeneralPredicateDefinition(elements.get, shortPredicate)

      if (article.getText.contains("DatatypeProperty")) {
        addDatatypePropertyDetail(elements, shortPredicate, shortObject, gpd)
      }
      else if (article.getText.contains("ObjectProperty")) {
        addObjectPropertyDetail(elements, shortPredicate, shortObject, gpd)
      }
    }
  }

  /**
   * Adds the triple represented by `s`, `p` and `o` to the wiki page for the object property subject `s`.
   */
  private def addObjectPropertyDetail(elements: Option[MappingElement], shortPredicate: String, shortObject: String,
                                      gpd: Option[GeneralPredicateDefinition]): Unit = {
    val gpd = findGeneralPredicateDefinition(elements.get, shortPredicate)
    gpd match {
      case Some(definition) => if (!definition.values.contains(shortObject)) {
        definition.values += shortObject
      }
      case _ => println("Not found general predicate definition for predicate, creating new one")
        val classDef: ObjectPropertyDefinition = elements.get match {
          case i: ObjectPropertyDefinition => i
          case l: MappingElementList => l.elements.collectFirst { case i: ObjectPropertyDefinition => i}.get
        }
        classDef.templateContents += new GeneralPredicateDefinition(shortPredicate, ListBuffer(shortObject))
    }
    println(elements.get.getMappingSyntax())
  }

  /**
   * Adds the triple represented by `s`, `p` and `o` to the wiki page for the datatype property subject `s`.
   */
  private def addDatatypePropertyDetail(elements: Option[MappingElement], shortPredicate: String, shortObject: String,
                                        gpd: Option[GeneralPredicateDefinition]): Unit = {
    val gpd = findGeneralPredicateDefinition(elements.get, shortPredicate)
    gpd match {
      case Some(definition) => if (!definition.values.contains(shortObject)) {
        definition.values += shortObject
      }
      case _ => println("Not found general predicate definition for predicate, creating new one")
        val classDef: DatatypePropertyDefinition = elements.get match {
          case i: DatatypePropertyDefinition => i
          case l: MappingElementList => l.elements.collectFirst { case i: DatatypePropertyDefinition => i}.get
        }
        classDef.templateContents += new GeneralPredicateDefinition(shortPredicate, ListBuffer(shortObject))
    }
    println(elements.get.getMappingSyntax())
  }

  /**
   * Recursively searches `element` and its child nodes until it finds a
   * [[org.dbpedia.extraction.scripts.mappingimport.mappingelements.GeneralPredicateDefinition]] with the name `name`.
   *
   * @param element element to start search at
   * @param name name of general predicate definition to search
   * @return the general predicate definition with the given name or en empty option
   */
  private def findGeneralPredicateDefinition(element: MappingElement,
                                             name: String): Option[GeneralPredicateDefinition] = {
    element match {
      case GeneralPredicateDefinition(n, v) if n == name => Some(element.asInstanceOf[GeneralPredicateDefinition])
      case l: MappingElementList => l.elements.map(findGeneralPredicateDefinition(_, name))
        .collectFirst { case Some(i) if i.isInstanceOf[GeneralPredicateDefinition] => i}
      case e: ClassDefinition => e.templateContents.map(findGeneralPredicateDefinition(_, name))
        .collectFirst { case Some(i) if i.isInstanceOf[GeneralPredicateDefinition] => i}
      case e: ObjectPropertyDefinition => e.templateContents.map(findGeneralPredicateDefinition(_, name))
        .collectFirst { case Some(i) if i.isInstanceOf[GeneralPredicateDefinition] => i}
      case e: DatatypePropertyDefinition => e.templateContents.map(findGeneralPredicateDefinition(_, name))
        .collectFirst { case Some(i) if i.isInstanceOf[GeneralPredicateDefinition] => i}
      case _ => Option.empty
    }
  }

  /**
   * Returns whether the given predicate is a class predicate. The classification is based on the type
   * of the entity used in the subject position of the predicate.
   *
   * @param predicate predicate to check
   */
  private def isClassStatement(predicate: String): Boolean = {
    classPredicates.contains(predicate)
  }

  /**
   * Returns whether the given predicate is a class predicate. The classification is based on the type
   * of the entity used in the subject position of the predicate.
   *
   * @param predicate predicate to check
   */
  private def isPropertyStatement(predicate: String): Boolean = {
    propertyPredicates.contains(predicate)
  }

  /**
   * Checks if the parser is able to preserve the semantics of the given article when generating the mapping syntax.
   * This method is mainly used as a security measure before introducing changes into the wiki to guarantee that we
   * do not lose information. If this method returns false, it is strictly recommended to not apply any modification
   * methods based on the output of the parser!
   *
   * @param article article to check
   * @return true if the parser can preserve the semantics, otherwise false
   */
  private def canSafelyModify(article: Article): Boolean = {
    val original = article.getText
    val generated = try WikiOntologySyntaxParser.parse(article.getText) catch {
      case e: ParseException => Option.empty[MappingElement]
    }
    if (generated.isEmpty) {
      false
    }
    else {
      isSemanticallyEquivalent(original, generated.get.getMappingSyntax())
    }
  }

  /**
   * Checks if the given original string and the generated string lead to the same ontology entity when parsed by the
   * ontology generation parser.
   *
   * @param original the original mapping syntax representation
   * @param generated the generated mapping syntax representation
   * @return true if the parsing led to semantically equivalent result, otherwise false
   */
  private def isSemanticallyEquivalent(original: String, generated: String): Boolean = {
    // parse both string representations, the class name is not relevant since it is only encoded in the page name
    val originalOntology = parseOntology("Dummy", original)
    val newOntology = parseOntology("Dummy", generated)

    // extract the parsed entity from the ontology objects, this is ugly but makes the method applicable to classes and
    // properties
    val originalEntity = (originalOntology.classes ++ originalOntology.properties).toList.head._2
    val newEntity = (newOntology.classes ++ newOntology.properties).toList.head._2

    OntologyComparator.equals(originalEntity, newEntity)
  }

  /**
   * Uses the mappings parser as employed by the extraction framework to retrieve the parse content of the given
   * article.
   *
   * @param title title of the page to parse
   * @param content content to parse
   * @return ontology resulting from the parsing process
   */
  private def parseOntology(title: String, content: String): Ontology = {
    val ontologyReader = new OntologyReader()
    val node = WikiParser.getInstance()
      .apply(new WikiPage(new WikiTitle(title, Namespace.OntologyClass, Language.Mappings), content))
    ontologyReader.read(Set(node.get).toIterable)
  }

  /**
   * Tries to shorten the given URI using a namespace identifier from RdfNamespace
   * @param uri URI to shorten
   * @return shortened URI or the original URI if no shortening possible
   */
  private def shortenURI(uri: String): String = {
    if (uri.startsWith("http://dbpedia.org/ontology/")) {
      uri.replace("http://dbpedia.org/ontology/", "")
    }
    else {
      RdfNamespace.shortenWithPrefix(uri)
    }
  }
}

object ImportMappingsIntoWiki {
  def main(args: Array[String]) {
    require(args != null && args.length == 3,
      """
        |This script imports the triples contained in the given N-triples file into the mappings wiki.
        |The supported predicates are currently limited to subClass and equivalentClass statements and
        |subproperty statements. For all of these statement types, the subject is required to be in the
        |DBpedia ontology namespace and the corresponding wiki page has to exist.
        |
        |For safety reasons, operations will not be performed if the parser cannot handle parts of the
        |statements contained in the Wiki page. In these cases, a message is printed giving instructions
        |for manual modification.
        |
        |Usage:
        | <username> <password> <file>
        |
        | username - username with editing rights for the mapping wiki
        | password - password for the wiki account
        | file - N-triples file containing the triples to import
      """.stripMargin)
    val importer = new ImportMappingsIntoWiki(user = args(0), password = args(1))

    val manualLog = importer.importNTriplesFile(args(2))

    println("Modifications which have to be performed manually:" + manualLog)
  }
}