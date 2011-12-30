//package blokus

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 14, 2010
 * Time: 8:03:42 PM
 * To change this template play File | Settings | File Templates.
 */

abstract class Rotation
case class Zero extends Rotation()
case class Ninety extends Rotation()
case class OneEighty extends Rotation()
case class TwoSeventy extends Rotation()

object Rotation {
  def all = List(Zero(), Ninety(), OneEighty(), TwoSeventy())
}