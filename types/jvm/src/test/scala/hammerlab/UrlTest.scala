package hammerlab

import java.net.URL

import cats.effect._
import hammerlab.url._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.server.blaze._

class UrlTest
  extends hammerlab.Suite {

  val response = "This is the response"
  val port = 8000
  val service = HttpService[IO] {
    case  GET -> Root / "test" ⇒ Ok(response)
    case HEAD -> Root / "test" ⇒ Ok(response)
  }

  val builder =
    BlazeBuilder[IO]
      .bindHttp(port, "localhost")
      .mountService(service, "/")
      .start

  val server = builder.unsafeRunSync()

  test("localhost") {
    ===(
      new URL(s"http://localhost:$port/test").size,
      response.length
    )
  }

  override protected def afterAll(): Unit = {
    server.shutdown.unsafeRunSync()
    super.afterAll()
  }
}
