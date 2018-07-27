package hammerlab

trait url {
  implicit val UrlOps = org.hammerlab.url.UrlOps _
}

object url extends url
