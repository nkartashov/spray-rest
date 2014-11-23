package com.restful

import com.restful.entitites.{Hero, Match, Team}

import scala.io.Source

/**
 * User: nikita_kartashov
 * Date: 22.11.2014
 * Time: 21:50
 */
object Data {
  def heroObjects: List[Hero] = heroes

  def teamObjects: List[Team] = teams

  def matchObjects: List[Match] = matches

  lazy val heroes = readHeroes

  lazy val teams = readTeams

  lazy val matches = readMatches


  def resource(file: String) = Source.fromURL(getClass.getClassLoader.getResource(file))

  def readHeroes = {
    val heroesFile = resource("heroes.txt")

    def lineToHero(line: String, index: Int) = line.split(',').toList match {
      case List(name, iconPath) => Hero(index, name, iconPath)
    }
    heroesFile.getLines().toList.zipWithIndex.map((lineToHero _).tupled)
  }

  def readTeams = {
    val teamsFile = resource("teams.txt")
    teamsFile.getLines().toList.zipWithIndex.map(t => Team(t._2, t._1))
  }

  def readMatches = {
    val heroesInTeam = 5
    val matchesFile = resource("matches.txt")
    def lineToMatch(line: String, index: Int) =
      line.split(',').toList match {
        case winString :: timeString :: radiantTeamIdString :: rest =>
          val radiantTeamHeroesIds = rest.take(heroesInTeam).map(_.toInt)
          rest.drop(heroesInTeam) match {
            case direTeamIdString :: theVeryRest =>
              val direTeamHeroesIds = theVeryRest.map(_.toInt)
              Some(Match(index, winString.toBoolean, timeString.toInt, radiantTeamIdString.toInt,
              direTeamIdString.toInt, radiantTeamHeroesIds, direTeamHeroesIds))
            case _ => None
          }
        case _ => None
      }
    matchesFile.getLines().toList.zipWithIndex.map((lineToMatch _).tupled).flatten
  }
}
