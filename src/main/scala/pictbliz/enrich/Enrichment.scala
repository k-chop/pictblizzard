package pictbliz.enrich

trait Enrichment {

  object all extends EnrichAll

  object bufferedimage extends ToRichBufferedImage

  object packedcolor extends ToPackedColorInt

  trait EnrichAll extends ToRichBufferedImage with ToPackedColorInt
}
