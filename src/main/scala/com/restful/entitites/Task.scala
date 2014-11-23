package com.restful.entitites

import com.restful.Data

/**
 * User: nikita_kartashov
 * Date: 22.11.2014
 * Time: 21:47
 */
case class Task(taskId: Int, teamId: Int, heroId: Int) {
  def matchesWithTeamAndHero() = Data.matchObjects.filter(m => m.participates(teamId) && m.heroParticipates(heroId))
}

object Task {
  var taskId = 0
  def newTask(teamId: Int, heroId: Int) = {
    val task = new Task(taskId, teamId, heroId)
    taskId += 1
    task
  }
}
