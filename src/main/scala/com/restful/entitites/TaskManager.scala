package com.restful.entitites

import java.util.concurrent.ConcurrentHashMap

/**
 * User: nikita_kartashov
 * Date: 23.11.2014
 * Time: 20:54
 */
object TaskManager {
  val taskMap = new ConcurrentHashMap[Int, Task]

  def newTask(teamId: Int, heroId: Int) = {
    val task = Task.newTask(teamId, heroId)
    taskMap.put(task.taskId, task)
    task
  }

  def updateTask(taskId: Int, teamId: Int, heroId: Int) = {
    Option(taskMap.get(taskId)) match {
      case None => None
      case Some(task) =>
        val updatedTask = Task(task.taskId, teamId, heroId)
        taskMap.replace(taskId, updatedTask)
        Some(updatedTask)
    }
  }

  def getTask(taskId: Int) = Option(taskMap.get(taskId))

  def removeTask(taskId: Int) = Option(taskMap.remove(taskId))
}
