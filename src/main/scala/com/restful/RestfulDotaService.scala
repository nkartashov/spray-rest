package com.restful

import akka.actor.Actor
import com.restful.rendering.{AbstractRenderer, HeroesRenderer, MatchesRenderer, TeamsRenderer}
import spray.http.MediaTypes
import spray.http.StatusCodes.Found
import spray.routing._

class RestfulDotaActor extends Actor with RestfulDotaService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(routes)
}

// this trait defines our service behavior independently from the service actor
trait RestfulDotaService extends HttpService {

  def wrapInHtmlIndex(text: String) =
    "<html>" + "<head></head><body><h1>Welcome to RESTful Doto server</h1><br>" +
      """Here's a menu for you <a href="/heroes">Heroes</a>,
        |<a href="/teams">Teams</a>,
        |<a href="/matches">Matches</a><br>""".stripMargin + text + "</body>" + "</html>"

  val index = path("") {
    get {
      getFromResource("pages/index.html")
    }
  }

  def renderedPaths[T](pathString: String, renderer: AbstractRenderer[T], objects: List[T]) = {
    path(pathString) {
      get {
        respondWithMediaType(MediaTypes.`text/html`) {
          complete(wrapInHtmlIndex(renderer.html(objects)))
        } ~
          respondWithMediaType(MediaTypes.`text/xml`) {
            complete(renderer.xml(objects))
          } ~
          respondWithMediaType(MediaTypes.`application/json`) {
            complete(renderer.json(objects))
          } ~
          respondWithMediaType(MediaTypes.`text/plain`) {
            complete(renderer.txt(objects))
          }
      }
    }
  }

  val images = pathPrefix("img") {
    get {
      getFromResourceDirectory("img")
    }
  }

  val heroes = renderedPaths("heroes", HeroesRenderer, Data.heroObjects)

  val matches = renderedPaths("matches", MatchesRenderer, Data.matchObjects)

  val teams = renderedPaths("teams", TeamsRenderer, Data.teamObjects)

  val unmatchedRedirect = unmatchedPath { ump =>
    redirect("http://spray.io", Found)
  }

  val routes = index ~ images ~ heroes ~ matches ~ teams ~ unmatchedRedirect
}