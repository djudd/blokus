//package blokus

import collection.mutable.LinkedHashMap

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 14, 2010
 * Time: 11:09:23 PM
 * To change this template play File | Settings | File Templates.
 */

abstract class Shift(left: Int, down: Int) {
  def shift(x: Int, y: Int) = (x+left, y+down)
  def clockwise : Shift
  def opposite : Shift
  def orientation : Orientation
}

case class Down extends Shift(0,1) {
  def clockwise = Left()
  def opposite = Up()
  def orientation = Vertical()
}
case class Up extends Shift(0,-1) {
  def clockwise = Right()
  def opposite = Down()
  def orientation = Vertical()
}
case class Left extends Shift(-1,0) {
  def clockwise = Up()
  def opposite = Right()
  def orientation = Horizontal()
}
case class Right extends Shift(1,0) {
  def clockwise = Down()
  def opposite = Left()
  def orientation = Horizontal()
}

object Shift {
  type Path = Seq[Shift]
  type Placement = Seq[Path]

  def toCoords(x: Int, y: Int, placement: Placement) : Seq[(Int,Int)] = {
    for(path <- placement) yield toPoint(x,y,path)
  }

  def toPoint(x: Int, y: Int, path: Path) : (Int,Int) = {
      return path.foldLeft((x,y))((result,shift) => {
        val (i,j) = result
        shift.shift(i,j)
      })
  }

  def rotateClockwise(placement: Placement, rot: Rotation) : Placement = {
    rot match {
      case Zero() => placement
      case Ninety() =>
        placement.map(
          path => {
            path.map(
              shift => shift.clockwise
            )
          }
        )
      case OneEighty() => rotateClockwise(rotateClockwise(placement, Ninety()), Ninety())
      case TwoSeventy() => rotateClockwise(rotateClockwise(placement, OneEighty()), Ninety())
    }
  }

  def flipAcross(placement: Placement, orient: Orientation) : Placement = {
    placement.map(
      path => {
        path.map(
          shift => (if (shift.orientation.equals(orient)) shift else shift.opposite)
        )
      }
    )
  }

  def translate(placement: Placement, translation: Path) = {
    placement.map(
      path => {
        dedup(translation ++ path)
      }
    )
  }

  def allTranslationsOverlappingOrigin(placement: Placement) : Set[Placement] = {
    rotateClockwise(placement, OneEighty()).map(
      path => {
        translate(placement, path)
      }
    ).toSet
  }

  def allRotations(placement: Placement) : Set[Placement] = {
    Rotation.all.map(rot => Shift.rotateClockwise(placement, rot)).toSet
  }

  def allFlips(placement: Placement) : Set[Placement] = {
    Set(placement, Shift.flipAcross(placement, Horizontal()), Shift.flipAcross(placement, Vertical()))
  }

  def allPlacements(placement: Placement) : Set[Placement] = {
    allRotations(placement).map(
      rotatedPlacement => allFlips(rotatedPlacement).map(
        flippedPlacement => allTranslationsOverlappingOrigin(flippedPlacement)
      ).flatten
    ).flatten.toSet
  }

  def dedup(path: Path) : Path = {
    val byOrientation: Iterable[Seq[Shift]] = path.groupBy(shift => shift.orientation).values
    byOrientation.map(
      shifts => {
        val opposites: List[Seq[Shift]] = shifts.groupBy(shift => shift).values.toList
        opposites match {
          case List(neg: Seq[Shift], pos: Seq[Shift]) => neg.length - pos.length match {
            case diff if diff > 0 => neg.take(diff)
            case diff if diff < 0 => pos.take(-diff)
            case _ => Nil
          }
          case List(all: Seq[Shift]) => all
          case Nil => Nil
        }
      }
    ).flatten.toSeq
  }

}