package com.restful.entitites

import com.restful.Data

/**
 * User: nikita_kartashov
 * Date: 22.11.2014
 * Time: 18:07
 */
case class Match(matchId: Int, win: Boolean, time: Int,
                 radiantId: Int, direId: Int,
                 radiantHeroesIds: List[Int],
                 direHeroesIds: List[Int]) {
  def radiantTeam = Data.teamObjects(radiantId)
  def direTeam = Data.teamObjects(direId)
  def radiantHeroes = radiantHeroesIds.map(Data.heroObjects(_)).toList
  def direHeroes = direHeroesIds.map(Data.heroObjects(_)).toList
  def participates(teamId: Int) = radiantId == teamId || direId == teamId
  def heroParticipates(heroId: Int) = radiantHeroesIds.contains(heroId) || direHeroesIds.contains(heroId)
  def radiantWins = win
}
