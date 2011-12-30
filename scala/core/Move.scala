package blokus

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 28, 2010
 * Time: 3:57:53 PM
 * To change this template use File | Settings | File Templates.
 */

class Move(
        val piece: Piece,
        private val offset: (Int,Int),
        private val placement: List[(Int,Int)]
) {

  def validate {
    if (!piece.hasPlacement(placement))
      throw new IllegalPlacementException(piece + " does not have placement " + placement)
  }

  private val (i,j) = offset
  
  val cells = placement.map({case (x,y) => (x+i, y+j)})

  override def toString() = piece.toString + ": " + cells.toString

  override def equals(other: Any): Boolean = {
    piece.equals(other.asInstanceOf[Move].piece) && cells.equals(other.asInstanceOf[Move].cells)
  }
}

object Move {
  def apply(piece: Piece, offset: (Int,Int), placement: List[(Int,Int)]): Move = {
    new Move(piece, offset, placement)
  }

}