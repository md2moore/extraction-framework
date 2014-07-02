package org.dbpedia.extraction.util

import java.io.{InputStreamReader, BufferedReader}
import java.net.URL
import RichReader.wrapReader
import java.util.Locale
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.HashMap
import org.dbpedia.extraction.ontology.DBpediaNamespace
import org.dbpedia.extraction.ontology.RdfNamespace

import scala.io.{Codec, Source}

/**
 * Represents a MediaWiki instance and the language used on it. Initially, this class was
 * only used for xx.wikipedia.org instances, but now we also use it for mappings.dbpedia.org
 * and www.wikidata.org. For each language, there is only one instance of this class.
 * TODO: rename this class to WikiCode or so, distinguish between enwiki / enwiktionary etc.
 *
 * @param wikiCode "en", "de", "mappings", "wikidata", ...
 * @param isoCode "en", "de", ...
 * @param dbpediaDomain Specific DBpedia domain for this language, e.g. "en.dbpedia.org".
 * May be null, e.g. for mappings.
 * @param dbpediaUri Specific DBpedia base URI for this language, e.g. "http://en.dbpedia.org".
 * May be null, e.g. for mappings.
 * @param resourceUri Specific resource namespace for this language, e.g. "http://en.dbpedia.org/resource/"
 * or "http://www.wikidata.org/entity/". May be null, e.g. for mappings. The value is not a string.
 * Use resourceUri.append("Xy"), not string concatenation.
 * @param propertyUri Specific property namespace for this language, e.g. "http://en.dbpedia.org/property/"
 * or "http://www.wikidata.org/entity/". May be null, e.g. for mappings. The value is not a string.
 * Use propertyUri.append("xy"), not string concatenation.
 * @param baseUri URI prefix for this wiki, e.g. "http://be-x-old.wikipedia.org",
 * "http://commons.wikimedia.org", "http://mappings.dbpedia.org".
 * @param apiUri API URI for this wiki, e.g. "http://be-x-old.wikipedia.org/w/api.php",
 * "http://commons.wikimedia.org/w/api.php", "http://mappings.dbpedia.org/api.php".
 */
class Language private(
  val wikiCode: String,
  val isoCode: String,
  val dbpediaDomain: String,
  val dbpediaUri: String,
  val resourceUri: RdfNamespace,
  val propertyUri: RdfNamespace,
  val baseUri: String,
  val apiUri: String
)
{
    val locale = new Locale(isoCode)

    /**
     * Wikipedia dump files use this prefix (with underscores), e.g. be_x_old, but
     * Wikipedia domains use the wikiCode (with dashes), e.g. http://be-x-old.wikipedia.org
     */
    val filePrefix = wikiCode.replace('-', '_')

    /**
     */
    override def toString = "wiki="+wikiCode+",locale="+locale.getLanguage

    // no need to override equals() and hashCode() - there is only one object for each value, so equality means identity.
}

object Language extends (String => Language)
{
  implicit val wikiCodeOrdering = Ordering.by[Language, String](_.wikiCode)

  val map: Map[String, Language] = locally {

    def language(code : String, iso: String): Language = {
      new Language(
        code,
        iso,
        code+".dbpedia.org",
        "http://"+code+".dbpedia.org",
        new DBpediaNamespace("http://"+code+".dbpedia.org/resource/"),
        new DBpediaNamespace("http://"+code+".dbpedia.org/property/"),
        "http://"+code+".wikipedia.org",
        "http://"+code+".wikipedia.org/w/api.php"
      )
    }

    val languages = new HashMap[String,Language]

    // parse the list of language codes from file, the Locale class is not really suited for testing
    // the language codes. Currently, we use a local copy of the list file.
    var isoCodes = mutable.Set[String]()

    val isoCodesSource = Source.fromURL("http://www.loc.gov/standards/iso639-2/ISO-639-2_utf-8.txt")(Codec.UTF8)
    try {
      isoCodesSource.getLines().foreach { line =>
        if (!line.startsWith("#") && !line.trim.isEmpty) {
          // we need the first three fields separately
          val elements = line.split("\\|", 4)
          isoCodes += elements(0) // add three-letter code
          isoCodes += elements(2) // add two-letter code
        }
      }
    }
    finally {
      isoCodesSource.close()
    }


    // Maps Wikipedia language codes which do not follow ISO-639-1, to a related ISO-639-1 code.
    // See: http://s23.org/wikistats/wikipedias_html.php (and http://en.wikipedia.org/wiki/List_of_Wikipedias)
    // Mappings are mostly based on similarity of the languages and in some cases on the regions where a related language is spoken.
    // See NonIsoLanguagesMappingTest and run it regularly.
    // TODO: move these to a config file
    // TODO: is this map still necessary? Since JDK 7, Locale officially handles three-letter codes.
    var nonIsoCodes: HashMap[String, String] = HashMap()
    val nonIsoSource = Source
      .fromInputStream(getClass.getResourceAsStream("/org/dbpedia/extraction/util/nonIsoCodes.tsv"))

    try {
      nonIsoSource.getLines().foreach { line =>
        if (!line.startsWith("#")) {
          val elements = line.split("\t", 3)
          require(elements.size >= 2) // this should hold as long as the input file is valid

          nonIsoCodes += elements(0) -> elements(1)
        }
      }
    }
    finally {
      nonIsoSource.close()
    }

    for (iso <- isoCodes) languages(iso) = language(iso, iso)

    // We could throw an exception if the mapped ISO code is not in the set of ISO codes, but then
    // this class (and thus the whole system) wouldn't load, and that set may change depending on
    // JDK version, and the affected wiki code may not even be used. Just silently ignore it.
    // TODO: let this loop build a list of codes with bad mappings and throw the exception later.
    for ((code, iso) <- nonIsoCodes) if (isoCodes.contains(iso)) languages(code) = language(code, iso)

    languages("commons") =
      new Language(
        "commons",
        "en",
        // TODO: do DBpedia URIs make sense here? Do we use them at all? Maybe use null instead.
        "commons.dbpedia.org",
        "http://commons.dbpedia.org",
        new DBpediaNamespace("http://commons.dbpedia.org/resource/"),
        new DBpediaNamespace("http://commons.dbpedia.org/property/"),
        "http://commons.wikimedia.org",
        "http://commons.wikimedia.org/w/api.php"
      )

    languages("wikidata") =
      new Language(
        "wikidata",
        "en",
        // TODO: do DBpedia URIs make sense here? Do we use them at all? Maybe use null instead.
        "wikidata.dbpedia.org",
        "http://wikidata.dbpedia.org",
        RdfNamespace.WIKIDATA,
        RdfNamespace.WIKIDATA,
        "http://www.wikidata.org",
        "http://www.wikidata.org/w/api.php"
      )





    languages("mappings") =
      new Language(
        "mappings",
        "en",
        // No DBpedia / RDF namespaces for mappings wiki.
        "mappings.dbpedia.org",
        "http://mappings.dbpedia.org",
        RdfNamespace.MAPPINGS,
        RdfNamespace.MAPPINGS,
        "http://mappings.dbpedia.org",
        "http://mappings.dbpedia.org/api.php"
      )

    languages.toMap
  } // toMap makes immutable

  /**
   * English Wikipedia
   */
  val English = map("en")

  /**
   * DBpedia mappings wiki
   */
  val Mappings = map("mappings")

  /**
   * Wikimedia commons
   */
  val Commons = map("commons")

  /**
   * Wikimedia Wikidata
   */
  val Wikidata = map("wikidata")

  /**
   * Gets a language object for a Wikipedia language code.
   * Throws IllegalArgumentException if language code is unknown.
   */
  def apply(code: String) : Language = map.getOrElse(code, throw new IllegalArgumentException("unknown language code "+code))

  /**
   * Gets a language object for a Wikipedia language code, or None if given code is unknown.
   */
  def get(code: String) : Option[Language] = map.get(code)

  /**
   * Gets a language object for a Wikipedia language code, or the default if the given code is unknown.
   */
  def getOrElse(code: String, default: => Language) : Language = map.getOrElse(code, default)
}
