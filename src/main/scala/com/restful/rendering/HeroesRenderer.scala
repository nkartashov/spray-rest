package com.restful.rendering

import com.restful.entitites.Hero

/**
 * User: nikita_kartashov
 * Date: 22.11.2014
 * Time: 22:35
 */
object HeroesRenderer extends AbstractRenderer[Hero] {
  override def html(objects: List[Hero]): String =
    "<table>" + "<tr><th>Hero Id</th><th>Screen name</th><th>Image</th></tr>" +
      objects.map(h => "<tr>" +
        s"<td>${h.heroId}</td>" +
        s"""<td><a href="/heroes/${h.heroId}">${h.heroName}</a></td>""" +
        s"""<td><img height=36 width=64 src="/img/${h.iconSourcePath}"></td></tr>""").mkString("") +
      "</table>"

  override def xml(objects: List[Hero]): String = "<heroes>" + objects.map(h => "<hero>" +
    s"<heroId>${h.heroId}</heroId>" +
    s"<screenName>${h.heroName}</screenName>" +
    s"<image>${h.iconSourcePath}</image>" +
    "</hero>").mkString("") + "</heroes>"

  override def json(objects: List[Hero]): String = "{\"heroes\": [" + objects.map(h => "{" +
    s""""heroId": "${h.heroId}",\n""" +
    s""""screenName": "${h.heroName}",""" +
    s""""image": "${h.iconSourcePath}"""" +
    "}").mkString(",") + "]}"

  override def txt(objects: List[Hero]): String = objects.map(h =>
    s"${h.heroId} ${h.heroName} ${h.iconSourcePath}").mkString("\n")
}
