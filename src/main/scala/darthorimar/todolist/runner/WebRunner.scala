package darthorimar.todolist.runner

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

import scala.concurrent.ExecutionContextExecutor
import scala.io.{Source, StdIn}
import akka.http.scaladsl.server.directives.ContentTypeResolver.Default

class WebRunner extends Runner {
  private def renderHtmlTemplate(content: String) =
    Source.fromResource("index.html").getLines.mkString("\n")
      .replaceAllLiterally("{{content}}", content)

  override def run(text: String): Unit = {
    implicit val system: ActorSystem = ActorSystem("my-system")
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher
    val route =
      path("") {
        get {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, renderHtmlTemplate(text)))
        }
      } ~
        path("static" / Segment) { name =>
          getFromResource(name)
        }
    println("Starting web server...")
    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress ENTER to stop")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}
