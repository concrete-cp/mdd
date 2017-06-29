package mdd

/**
  * Created by vion on 26/06/17.
  */

sealed trait Starrable

case class ValueStar(v: Int) extends Starrable

case object Star extends Starrable
