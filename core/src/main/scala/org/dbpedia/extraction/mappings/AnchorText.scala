package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.destinations.{DBpediaDatasets, Quad}
import org.dbpedia.extraction.wikiparser._
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.util.{Language, ExtractorUtils}
import scala.language.reflectiveCalls
import scala.collection.mutable.ListBuffer

/**
 * Extracts internal links between DBpedia instances from the internal page links between
 * Wikipedia articles. The page links might be useful for structural analysis, data mining 
 * or for ranking DBpedia instances using Page Rank or similar algorithms.
 */
class AnchorText (
                           context : {
                             def ontology : Ontology
                             def language : Language
                           }
                           )
  extends PageNodeExtractor {
  val wikiPageWikiLinkProperty = context.ontology.properties("wikiPageWikiLink")

  override val datasets = Set(DBpediaDatasets.AnchorText)

  override def extract(node: PageNode, subjectUri: String, pageContext: PageContext): Seq[Quad] = {
    if (node.title.namespace != Namespace.Main && !ExtractorUtils.titleContainsCommonsMetadata(node.title))
      return Seq.empty

    val list = AnchorText.collectInternalLinks(node)
    var buffer = new ListBuffer[Quad]()
    for (nodes <- list) {
      if(nodes.children.length > 1){
        if(!getUri(nodes.destination).contains("File:")){
          val concated = nodes.children.map(_.toPlainText).mkString("")
          buffer += new Quad(context.language, DBpediaDatasets.AnchorText, getUri(nodes.destination), wikiPageWikiLinkProperty, concated, nodes.sourceUri, context.ontology.datatypes("rdf:langString"))
        }
        for(child <- nodes.children){
          child match
          {
            case intlink : InternalLinkNode => buffer += new Quad(context.language, DBpediaDatasets.AnchorText, getUri(intlink.destination), wikiPageWikiLinkProperty, intlink.children(0).toPlainText, nodes.sourceUri, context.ontology.datatypes("rdf:langString"))
            case _ =>
          }
        }
      }
      else {
        for (child <- nodes.children) {
          //if (child.isInstanceOf[InternalLinkNode]) {

          //}
            buffer += new Quad(context.language, DBpediaDatasets.AnchorText, getUri(nodes.destination), wikiPageWikiLinkProperty, child.toPlainText, nodes.sourceUri, context.ontology.datatypes("rdf:langString"))
        }
      }
    }
    //list.map(link => new Quad(context.language, DBpediaDatasets.PageLinks, subjectUri, wikiPageWikiLinkProperty, node.children(0).toWikiText, link.sourceUri, null))
    val list2 = buffer.toList
    list2
    //recurse(list)
  }

  private def getUri(destination: WikiTitle): String = {
    context.language.resourceUri.append(destination.decodedWithNamespace)
  }

  /*
  private def recurse(list : List[Node]) : Seq[Quad] =
  {
    var buffer = new ListBuffer[Quad]()
    for(nodes <- list){
      for(child <- nodes.children){
        if(child.isInstanceOf[InternalLinkNode]){
          recurse(child.children)
        }
        else{
          buffer += new Quad(context.language, DBpediaDatasets.PageLinks, getUri(nodes.destination), wikiPageWikiLinkProperty, child.toPlainText, nodes.sourceUri, context.ontology.datatypes("rdf:langString"))
        }
      }
    }
    val list2 = buffer.toList
    list2
  }
}
*/
}
object AnchorText {

  def collectInternalLinks(node : Node) : List[InternalLinkNode] =
  {
    node match
    {
      case linkNode : InternalLinkNode => List(linkNode)
      case _ => node.children.flatMap(collectInternalLinks)
    }
  }
}