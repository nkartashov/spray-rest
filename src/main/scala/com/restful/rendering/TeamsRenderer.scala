package com.restful.rendering

import com.restful.entitites.Team

/**
 * User: nikita_kartashov
 * Date: 22.11.2014
 * Time: 22:10
 */
object TeamsRenderer extends AbstractRenderer[Team] {
  override def html(objects: List[Team]): String =
    "<table>" + "<tr><th>Team Id</th><th>Team Name</th></tr>" +
      objects.map(t => "<tr>" +
        s"<td>${t.teamId}</td>" +
        s"""<td><a href="/teams/${t.teamId}">${t.teamName}</a></td></tr>""").mkString("\n") +
      "</table>"

  override def xml(objects: List[Team]): String = "<teams>" + objects.map(t => "<team>" +
    s"<teamId>${t.teamId}</teamId>" +
    s"<teamName>${t.teamName}</teamName>" +
    "</team>").mkString("\n") + "</teams>"

  override def json(objects: List[Team]): String = "{\"teams\": [" + objects.map(t => "{" +
    s""""teamId": ${t.teamId},\n""" +
    s""""teamName": "${t.teamName}"\n""" +
    "}").mkString(",\n") + "]}"

  override def txt(objects: List[Team]): String = objects.map(t =>
    s"${t.teamId} ${t.teamName}").mkString("\n")
}
