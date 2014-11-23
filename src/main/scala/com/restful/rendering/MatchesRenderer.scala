package com.restful.rendering

import com.restful.entitites.{Hero, Team, Match}

/**
 * User: nikita_kartashov
 * Date: 23.11.2014
 * Time: 14:47
 */
object MatchesRenderer extends AbstractRenderer[Match] {
  val itemsInTimeUnit = 60

  def timeToHumanFormat(time: Int) = (1 to 3).reverse.map(
    i => (time % Math.pow(itemsInTimeUnit, i).toInt / Math.pow(itemsInTimeUnit, i - 1).toInt).toString).mkString(":")

  def teamToHtml(team: Team, heroes: List[Hero]): String =
    s"""<a href="/matches/${team.teamId}">${team.teamName}</a>""" + "<br>" +
      heroes.map(h => s"""<a href="/heroes/${h.heroId}"><img height=24 width=43 src="/img/${h.iconSourcePath}"></a>""").mkString("")

  def teamToXml(team: Team, heroes: List[Hero]): String =
    s"<teamName>${team.teamName}</teamName>" + heroes.map(h => "<hero>" +
      s"<heroId>${h.heroId}</heroId>" +
      s"<screenName>${h.heroName}</screenName>" +
      s"<image>${h.iconSourcePath}</image></hero>").mkString("\n")

  def teamToJson(team: Team, heroes: List[Hero]): String =
  s"""{"teamId": ${team.teamId}, "teamName": "${team.teamName}", "heroes": [${heroes.map(h =>
    s"""{"heroId": ${h.heroId},""" +
    s""""screenName": "${h.heroName}",""" +
    s""""image": "${h.iconSourcePath}"}""").mkString(",")}]}"""

  def teamToTxt(team: Team, heroes: List[Hero]): String = s"${team.teamName} " + heroes.map(h => h.heroName).mkString(" ")

  override def html(objects: List[Match]): String =
    "<table>" + "<tr>\n<th>Match Id</th>\n<th>Length</th>\n<th>Winner</th>\n<th>Radiant</th><th>Dire</th>\n</tr>" +
      objects.map(m => "<tr>" +
        s"<td>${m.matchId}</td>" +
        s"<td>${timeToHumanFormat(m.time)}</td>" +
        s"""<td>${
          (m.win match {
            case true => "Radiant"
            case _ => "Dire"
          }) + " won"
        }</td>""" +
        s"<td>${teamToHtml(m.radiantTeam, m.radiantHeroes)}</td>" +
        s"<td>${teamToHtml(m.direTeam, m.direHeroes)}</td>" +
        "</tr>").mkString("\n") +
      "</table>"

  override def xml(objects: List[Match]): String = "<matches>" + objects.map(m =>
    "<match>" +
      s"<matchId>${m.matchId}</matchId>" +
      s"<time>${m.time}</time>" +
      s"<winner>${m.win}</winner>" +
      s"<radiant>${teamToXml(m.radiantTeam, m.radiantHeroes)}</radiant>" +
      s"<dire>${teamToXml(m.direTeam, m.direHeroes)}</dire>" + "</match>"
  ).mkString("\n") + "</matches>"

  override def json(objects: List[Match]): String = "{\"matches\":[" + objects.map(m =>
    s"""{"matchId": ${m.matchId},""" +
    s""""time": ${m.time},""" +
    s""""winner": ${m.win},""" +
    s""""radiant": ${teamToJson(m.radiantTeam, m.radiantHeroes)},""" +
    s""""dire": ${teamToJson(m.direTeam, m.direHeroes)}}""").mkString(",") + "]}"

  override def txt(objects: List[Match]): String = objects.map(m =>
  s"${m.matchId} ${m.time} ${m.win} ${teamToTxt(m.radiantTeam, m.radiantHeroes)} ${teamToTxt(m.direTeam, m.direHeroes)}").mkString("\n")
}
