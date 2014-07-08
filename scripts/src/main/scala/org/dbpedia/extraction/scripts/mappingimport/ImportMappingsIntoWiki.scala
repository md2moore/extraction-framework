package org.dbpedia.extraction.scripts.mappingimport

import java.util.logging.{Level, Logger}

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
 *
 * @author Daniel Fleischhacker <daniel@informatik.uni-mannheim.de>
 */
class ImportMappingsIntoWiki(val mappingsWikiURL: String = Language.Mappings.apiUri) {
  private val wiki = new MediaWikiBot(mappingsWikiURL)

  private var username: Option[String] = None
  private var password: Option[String] = None

  /**
   * Number of milliseconds to pause after a wiki modification operation
   */
  var sleepTime : Int = 0

  /**
   * Number of lines to skip in the import file. This is helpful if the import process stops in the middle of the
   * import.
   */
  var skipLines : Option[Int] = None

  // pattern used to find triples in the input file
  private val ntriplesPattern = """^<([^>]+)>\s*<([^>]+)>\s*<([^>]+)>\s*\.$""".r("subject", "predicate", "object")

  // contains all predicate types currently supported by this program and whether they are class or property predicates
  private val classPredicates = Set("http://www.w3.org/2000/01/rdf-schema#subClassOf",
    "http://www.w3.org/2002/07/owl#equivalentClass")
  private val propertyPredicates = Set("http://www.w3.org/2000/01/rdf-schema#subPropertyOf")

  // disable logging from ontology reader to reduce noise in stdout
  Logger.getLogger(classOf[OntologyReader].getName).setLevel(Level.SEVERE)

  /**
   * Processes the given ntriples file by adding all predicates defined in this file to the wiki pages corresponding to
   * the subjects.
   *
   * @param fileName path of file to process
   */
  def importNTriplesFile(fileName: String): String = {
    val source = Source.fromFile(fileName)(Codec.UTF8)

    val manualOperationLog = new StringBuilder()

    var lineNumber = 0

    source.getLines().foreach { line =>
      lineNumber += 1
      if (skipLines.getOrElse(0) < lineNumber) {
        val matcher = ntriplesPattern.findFirstMatchIn(line)
        matcher match {
          case Some(ntriplesPattern(s, p, o)) if s.startsWith("http://dbpedia.org/ontology") =>
            addStatement(s, p, o, manualOperationLog)
          case _ =>
        }
      }
    }

    manualOperationLog.toString()
  }

  /**
   * The summary message generator for this importer. Summary message generators get the triple which was added to
   * the wiki as parameter and are used to generate a message which describing the change in the Wiki's article page log.
   */
  var generateSummaryMessage: (String, String, String) => String = generateDefaultSummaryMessage

  /**
   * Adds the triple represented by `s`, `p` and `o` to the wiki page for the subject `s`.
   *
   * @param s subject of the triple
   * @param p predicate of the triple
   * @param o object of the triple
   */
  private def addStatement(s: String, p: String, o: String, manualOperationLog: StringBuilder): Unit = {
    val entityName = s.replace("http://dbpedia.org/ontology/", "")
    val articleName = if (isClassStatement(p)) {
      s"OntologyClass:$entityName"
    }
    else if (isPropertyStatement(p)) {
      s"OntologyProperty:$entityName"
    }
    else {
      println(s"No article found for class $entityName, skipping")
      return
    }

    val article = wiki.getArticle(articleName)
    val shortPredicate = RdfNamespace.shortenWithPrefix(p)
    val shortObject = RdfNamespace.shortenWithPrefix(o)

    if (article.getText.isEmpty) {
      println(s"No article found for with name $articleName, skipping")
    }
    else if (!canSafelyModify(article)) {
      println(s"Not able to safely modify $articleName, skipping")
      manualOperationLog.append(
        s"""* add predicate $shortPredicate with content $shortObject to page http://mappings.dbpedia
           |.org/$articleName\n""".stripMargin)
    }
    else {
      println(s"Processing $articleName")
      val elements = WikiOntologySyntaxParser.parse(article.getText)
      val gpd = findGeneralPredicateDefinition(elements.get, shortPredicate)
      gpd match {
        case Some(definition) => if (!definition.values.contains(shortObject)) {
          definition.values += shortObject
        }
        case _ => println("Not found general predicate definition for predicate, creating new one")
          val rootDef: RootNode = elements.get match {
            case i: RootNode => i
            case l: MappingElementList => l.elements.collectFirst { case i: RootNode => i}.get
          }
          rootDef.templateContents += new GeneralPredicateDefinition(shortPredicate, ListBuffer(shortObject))
      }

      val mappingSyntax: String = elements.get.getMappingSyntax()
      val originalElements = WikiOntologySyntaxParser.parse(article.getText)

      if (originalElements == elements) {
        println("No changes performed")
      }
      else {
        if (!validateOntologyPage(mappingSyntax)) {
          println(s"!!!!!!!!!!!!!!!!! ERROR: $articleName no longer validates after modification")
        }
        else {
          checkAuth()

          article.setText(mappingSyntax)
          article.save(generateSummaryMessage(s,p,o))
        }
      }
    }
  }

  /**
   * Default implementation for generating summary messages
   */
  def generateDefaultSummaryMessage(s: String, p: String, o: String) : String = {
    s"Automatic import of triple <$s> <$p> <$o>"
  }

  /**
   * Simple replacing summary generator which takes a message string and replaces all occurrences
   * of the patterns $SUBJECT$, $OBJECT$ and $PREDICATE$ by the values of the current triple.
   */
  def generateReplacingSummaryMessage(message: String, s: String, p: String, o: String) : String = {
    message.replaceAll("\\$SUBJECT\\$", s).replaceAll("\\$PREDICATE\\$", p).replaceAll("\\$OBJECT\\$", o)
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
      case e: RootNode => e.templateContents.map(findGeneralPredicateDefinition(_, name))
        .collectFirst { case Some(i) if i.isInstanceOf[GeneralPredicateDefinition] => i}
      case _ => Option.empty
    }
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
   * Validates the given page using the ontology reader.
   *
   * @param content content to validate
   * @return true if the parse is able to parse the given content otherwise false
   */
  private def validateOntologyPage(content: String): Boolean = {
    try {
      parseOntology("dummy", content)
      true
    }
    catch {
      case e: Exception => false
    }
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
   * Sets the authentication data for the wiki.
   * @param user username to authenticate in the wiki
   * @param passwd password to authenticate in the wiki
   */
  def setAuthenticationData(user: String, passwd: String): Unit = {
    username = Some(user)
    password = Some(passwd)
    wiki.login(username.get, password.get)

    if (!wiki.isLoggedIn) {
      throw new RuntimeException("Invalid credentials for mappings wiki")
    }
  }

  /**
   * Checks whether there is a user authenticated in the wiki and throws a RuntimeException if not.
   * @throws RuntimeException if we are not authenticated in the wiki
   */
  private def checkAuth(): Unit = {
    if (!wiki.isLoggedIn) {
      throw new RuntimeException("Not logged in into wiki")
    }
  }
}

object ImportMappingsIntoWiki {
  def main(args: Array[String]) {
    require(args != null && (args.length == 3 || args.length == 4 || args.length == 5),
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
        |The optional parameter message is used as a template for the summary message when saving a
        |modified article. In this message, the variables $SUBJECT$, $OBJECT$ and $PREDICATE$ can be used
        |which are replaced by the actual triple.
        |
        |Usage:
        | <username> <password> <file> [<message> <skiplines>]
        |
        | username - username with editing rights for the mapping wiki
        | password - password for the wiki account
        | file - N-triples file containing the triples to import
        | message - summary message to write in modified articles' logs or NULL to use default message
        | skiplines - number of lines to skip in the import file
      """.stripMargin)
    val importer = new ImportMappingsIntoWiki()

    // we wait 5 secs between two modifications
    importer.sleepTime = 5000

    if (args.length == 4 && args(3) != "NULL") {
      importer.generateSummaryMessage = importer.generateReplacingSummaryMessage(args(3), _, _, _)
    }
    if (args.length == 5) {
      importer.skipLines = Some(args(4).toInt)
    }

    importer.setAuthenticationData(user = args(0), passwd = args(1))

    val manualLog = importer.importNTriplesFile(args(2))

    println(
      "\n\n********************************************************************\n\tModifications which have to be " +
        "performed manually:" +
        manualLog)
  }
}