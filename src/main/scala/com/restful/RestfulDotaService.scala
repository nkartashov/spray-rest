package com.restful

import akka.actor.Actor
import com.restful.entitites.TaskManager
import com.restful.rendering.{AbstractRenderer, HeroesRenderer, MatchesRenderer, TeamsRenderer}
import spray.http.MediaTypes
import spray.http.StatusCodes.Found
import spray.routing._

import scala.collection.JavaConversions.enumerationAsScalaIterator


class RestfulDotaActor extends Actor with RestfulDotaService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(routes)
}

trait RestfulDotaService extends HttpService {
  val applicationAddress = "http://young-waters-9522.herokuapp.com/"

  def wrapInHtmlIndex(text: String) =
    "<html>" + "<head></head><body><h1>Welcome to RESTful Doto server</h1><br>" +
      """Here's a menu for you <a href="/heroes">Heroes</a>,
        |<a href="/teams">Teams</a>,
        |<a href="/matches">Matches</a><br>""".stripMargin + text + "</body>" + "</html>"

  val index = path("") {
    get {
      respondWithMediaType(MediaTypes.`text/html`) {
        complete(wrapInHtmlIndex(
          s"""A little guide:<br>
             | This is a little site for The International 4 Dota 2 tournament
             |<ul>
             |<li> /heroes - all the heroes used in TI4 </li>
             |<li> /teams - all the teams, which participated in TI4 </li>
             |<li> /matches - all the matches of TI4 </li>
             |<li> /heroes/heroId - gives you matches in which the hero with heroId was used </li>
             |<li> /teams/teamId - gives you matches in which the team with teamId participated </li>
             |<li> /tasks/taskId - GET shows the result of the task with taskId <br> DELETE deletes the said task </li>
             | all of the above are available by clicking on links or manually typing the address in HTML, XML, JSON or plaintext
             |<li> /tasks - shows currently available tasks </li>
             |<li> /tasks/teamId/heroId - PUT add the task to list all matches in which both team with teamId and hero with heroId participated </li>
             |<li> /tasks/taskId/teamId/heroId - POST modifies the task with taskId </li>
             |</ul>
           """.stripMargin))
      }
    }
  }

  def renderObjects[T](renderer: AbstractRenderer[T], objects: List[T]) = {
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

  def renderedPaths[T](pathString: String, renderer: AbstractRenderer[T], objects: List[T]) = {
    get {
      path(pathString) {
        logRequest(pathString) {
          renderObjects(renderer, objects)
        }
      }
    }
  }

  val images = pathPrefix("img") {
    get {
      logRequest("images") {
        getFromResourceDirectory("img")
      }
    }
  }

  val heroes = renderedPaths("heroes", HeroesRenderer, Data.heroObjects) ~
    path("heroes" / IntNumber) {
      heroId =>
        logRequest("heroes" + "/" + heroId) {
          get {
            renderObjects(MatchesRenderer, Data.matchObjects.filter(_.heroParticipates(heroId)))
          }
        }
    }

  val matches = renderedPaths("matches", MatchesRenderer, Data.matchObjects)

  val teams = renderedPaths("teams", TeamsRenderer, Data.teamObjects) ~
    path("teams" / IntNumber) {
      teamId =>
        logRequest("teams" + "/" + teamId) {
          get {
            renderObjects(MatchesRenderer, Data.matchObjects.filter(_.participates(teamId)))
          }
        }
    }

  // Three paths
  // int - GET & DELETE, int means taskId
  // int, int - PUT, first int - teamId, second - heroId
  // int, int, int - POST, first + second use case
  def noTaskWithId(taskId: Int) = respondWithMediaType(MediaTypes.`text/html`) {
    complete(wrapInHtmlIndex(s"No task with id $taskId"))
  }

  def successfulDelete(taskId: Int) = respondWithMediaType(MediaTypes.`text/html`) {
    complete(wrapInHtmlIndex(s"Successfully deleted task with id $taskId"))
  }

  def successfulAdd(taskId: Int) = respondWithMediaType(MediaTypes.`text/html`) {
    complete(wrapInHtmlIndex(s"Successfully added task with id $taskId"))
  }

  def successfulPost(taskId: Int) = respondWithMediaType(MediaTypes.`text/html`) {
    complete(wrapInHtmlIndex(s"Successfully changed task with id $taskId"))
  }

  val tasks = get {
    path("tasks" / IntNumber) {
      taskId =>
        logRequest("tasks" + "/" + taskId) {
          TaskManager.getTask(taskId) match {
            case Some(task) => renderObjects(MatchesRenderer, task.resultingMatches())
            case _ => noTaskWithId(taskId)
          }
        }
    } ~
      path("tasks") {
        logRequest("tasks") {
          complete(wrapInHtmlIndex(s"Present task ids: <br>" + "<ul>" +
            TaskManager.taskMap.keys().toList.sorted.map(i => s"<li>$i</li>").mkString("") + "</ul>"))
        }
      }
  } ~
    delete {
      path("tasks" / IntNumber) {
        taskId =>
          logRequest("tasks" + "/" + taskId) {
            TaskManager.removeTask(taskId) match {
              case Some(_) => successfulDelete(taskId)
              case _ => noTaskWithId(taskId)
            }
          }
      }
    } ~
    put {
      path("tasks" / IntNumber / IntNumber) {
        (teamId, heroId) =>
          logRequest("tasks" + "/" + teamId + "/" + heroId)
          successfulAdd(TaskManager.newTask(teamId, heroId).taskId)
      }
    } ~
    post {
      path("tasks" / IntNumber / IntNumber / IntNumber) {
        (taskId, teamId, heroId) =>
          logRequest("tasks" + "/" + taskId + "/" + teamId + "/" + heroId) {
            TaskManager.updateTask(taskId, teamId, heroId) match {
              case None => noTaskWithId(taskId)
              case Some(task) => successfulPost(taskId)
            }
          }
      }
    }

  val unmatchedRedirect = unmatchedPath {
    ump =>
      logRequest("unmatched") {
        redirect(applicationAddress, Found)
      }
  }

  val stats = path("stats") {
    get {
      complete("")
    }
  }

  val routes = index ~ images ~ heroes ~ matches ~ teams ~ tasks ~ stats ~ unmatchedRedirect
}