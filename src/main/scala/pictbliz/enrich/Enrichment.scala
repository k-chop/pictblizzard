package pictbliz.enrich

trait Enrichment {

  object all extends EnrichAll

  object bufferedimage extends ToRichBufferedImage


  trait EnrichAll extends ToRichBufferedImage// with ...
}
