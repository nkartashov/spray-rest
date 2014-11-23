package com.restful.rendering

/**
 * User: nikita_kartashov
 * Date: 22.11.2014
 * Time: 22:14
 */
trait AbstractRenderer[T] {
  def html(objects: List[T]): String
  def xml(objects: List[T]): String
  def json(objects: List[T]): String
  def txt(objects: List[T]): String
}
