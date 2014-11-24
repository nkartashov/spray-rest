package com.restful.entitites

import com.restful.Data

/**
 * User: nikita_kartashov
 * Date: 22.11.2014
 * Time: 21:47
 */
case class Task(taskId: Int, teamId: Int, heroId: Int) {
  def resultingMatches() = Data.matchObjects.filter(m => m.participates(teamId) && m.heroParticipates(heroId))

  override def equals(obj: scala.Any): Boolean = obj match {
    case Task(otherId,_,_) => taskId == otherId
    case _ => false
  }

  override def hashCode(): Int = {
    val prime = 31
    taskId + (teamId + heroId * prime) * prime
  }
}

object Task {
  var taskId = 0
  def newTask(teamId: Int, heroId: Int) = synchronized {
    val task = new Task(taskId, teamId, heroId)
    taskId += 1
    task
  }
}
