import java.lang.StringBuilder

//package blokus

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 14, 2010
 * Time: 3:51:34 PM
 * To change this template play File | Settings | File Templates.
 */

abstract class Piece(val id: Int) {
  val placementSets: Set[Set[(Int,Int)]] =
      Shift.allPlacements(shifts).map(
        placement => Shift.toCoords(0, 0, placement).toSet
      ).toSet

  val placements: List[List[(Int,Int)]] =
      placementSets.map(
        placement => placement.toList
      ).toList

  val size: Int = shifts().size

  def hasPlacement(candidate: Seq[(Int,Int)]) : Boolean = {
    placementSets.contains(candidate.toSet)
  }
  
  protected def shifts() : Seq[Seq[Shift]]
}

case class One extends Piece(1) {
  override def shifts() = List(Nil)
}

abstract class IPiece(length: Int, id: Int) extends Piece(id) {
  override def shifts() : Seq[Seq[Shift]] = {
    for(i <- 1 to length) yield (for (j <- 1 to i-1) yield Down())
  }
}

case class Two() extends IPiece(2, 2)
case class Three() extends IPiece(3, 3)
case class ShortI() extends IPiece(4, 5)
case class LongI() extends IPiece(5, 10)

case class X extends Piece(11) {
  override def shifts() = List(Nil, List(Right()), List(Down()), List(Left()), List(Up()))
}

case class Square extends Piece(6) {
  override def shifts() = List(Nil, List(Right()), List(Down()), List(Right(), Down()))
}

case class CrookedThree() extends Piece(4) {
  override def shifts() = List(Nil, List(Right()), List(Down()))
}

case class ShortL() extends Piece(7) {
  override def shifts() = List(Nil, List(Right()), List(Down()), List(Down(), Down()))
}

case class LongL() extends Piece(12) {
  override def shifts() = List(Nil, List(Right()), List(Down()), List(Down(), Down()), List(Down(), Down(), Down()))
}

case class P extends Piece(13) {
  override def shifts() = List(Nil, List(Right()), List(Down()), List(Right(), Down()), List(Down(), Down()))
}

case class ShortT() extends Piece(8) {
  override def shifts() = List(Nil, List(Right()), List(Left()), List(Down()))
}

case class LongT() extends Piece(14) {
  override def shifts() = List(Nil, List(Right()), List(Left()), List(Down()), List(Down(), Down()))
}

case class ShortZ() extends Piece(9) {
  override def shifts() = List(Nil, List(Right()), List(Right(), Down()), List(Right(), Down(), Right()))
}

case class LongZ() extends Piece(15) {
  override def shifts() = List(Nil, List(Up()), List(Up(), Right()), List(Down()), List(Down(), Left()))
}

case class Y() extends Piece(16) {
  override def shifts() = List(Nil, List(Right()), List(Up()), List(Down()), List(Down(), Down()))
}

case class N() extends Piece(17) {
  override def shifts() = List(Nil, List(Right()), List(Right(), Down()), List(Right(), Down(), Right()), List(Right(), Down(), Right(), Right()))
}

case class F() extends Piece(18) {
  override def shifts() = List(Nil, List(Left()), List(Down()), List(Up()), List(Up(), Right()))
}

case class V() extends Piece(19) {
  override def shifts() = List(Nil, List(Up()), List(Up(), Up()), List(Left()), List(Left(), Left()))
}

case class W() extends Piece(20) {
  override def shifts() = List(Nil, List(Left()), List(Left(), Up()), List(Down()), List(Down(), Right()))
}

case class U() extends Piece(21) {
  override def shifts() = List(Nil, List(Left()), List(Left(), Up()), List(Right()), List(Right(), Up()))
}

object Piece {

  def all = List(
    One(), Two(), Three(), CrookedThree(),
    Square(), ShortI(), ShortT(), ShortL(), ShortZ(),
    P(), LongI(), LongT(), LongL(), LongZ(),
    F(), X(), V(), U(), Y(), N(), W()
  )

  // hacky code generation for c version
  def main(args: Array[String]) = {
//    for (i<-0 to 19) yield {
//      for (j<-0 to 19) yield {
        for (piece<-Piece.all) {
          for (corner<-(-1,-1) :: (-1,1) :: (1,1) :: (1,-1) :: Nil) {
            lhs(piece,corner)
            rhs("NULL")
            for (placement<-piece.placements) {
              val (x,y) = corner
//              val validCorner = (x+i,y+j) match {
//                case (-1,-1) => true
//                case (-1,20) => true
//                case (20,-1) => true
//                case (20,20) => true
//                case _ => !offBoard(i,j,corner)
//              }
              val validForCorner = !placement.contains(corner) && !placement.contains((x,0)) && !placement.contains((0,y))
//              val onboard = placement.find(point => offBoard(i, j, point)).isEmpty
              if (validForCorner) {
                output(piece, corner, placement)
              }
            }
          }
        }
//      }
//    }
  }

  def output(piece: Piece, corner: (Int,Int), placement: List[(Int,Int)]): Unit = {
    lhs(piece,corner)
    rhs("addPlacement("+cellStr(placement)+","+arrayIdx(piece,corner)+")");
  }

  def cellStr(placement: List[(Int,Int)]): String = {
    placement match {
      case Nil => "NULL"
      case _ => {
        val (x, y) = placement.head
        new StringBuilder()
          .append("addCell(")
          .append(x)
          .append(",")
          .append(y)
          .append(",")
          .append(cellStr(placement.tail))
          .append(")")
          .toString
      }
    }
  }

  def lhs(piece: Piece, corner: (Int,Int)) {
    print(arrayIdx(piece,corner)+" = ")
  }

  def arrayIdx(piece: Piece,corner: (Int,Int)): String = {
    val c = corner match {
          case (-1,-1) => 0
          case (-1,1) => 1
          case (1,1) => 2
          case (1,-1) => 3
    }
    val p = piece.id-1
    "placements["+p+"]["+c+"]"
  }

  def rhs(v: String) {
    println(v + ";")
  }

//  private def offBoard(i: Int, j: Int, point: (Int, Int)): Boolean = {
//    val (x, y) = point
//    (x+i < 0 || y+j < 0 || x+i > 19 || y+j > 19)
//  }

}