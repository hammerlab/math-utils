package org.hammerlab

import java.net.{ HttpURLConnection, URL }

object url {
  implicit class UrlOps(val url: URL) extends AnyVal {
    def size: Long = {
      val conn = url.openConnection().asInstanceOf[HttpURLConnection]
      conn.setRequestMethod("HEAD")
      val size = conn.getContentLengthLong
      conn.disconnect()
      size
    }
  }
}
