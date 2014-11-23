package com.restful


import akka.actor.{ActorSystem, Props}
import akka.io.IO
import spray.can.Http

import scala.util.Properties

object Boot extends App {

  // we need an ActorSystem to host our application in
  implicit val system = ActorSystem("on-spray-can")

  // create and start our service actor
  val service = system.actorOf(Props[RestfulDotaActor], "demo-service")

  val port = Properties.envOrElse("PORT", "8080").toInt // for Heroku compatibility

  // start a new HTTP server on port 8080 with our service actor as the handler
  IO(Http) ! Http.Bind(service, "0.0.0.0", port)
}