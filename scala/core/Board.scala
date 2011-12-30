package blokus

import core.{Diagonal, Straight, Edge}
import edu.uci.ics.jung.graph.{DirectedGraph, DirectedSparseGraph, Graph}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 14, 2010
 * Time: 3:49:10 PM
 * To change this template play File | Settings | File Templates.
 */

class Board(val board: Array[Array[Byte]] = Board.empty) {

  val size: Int = Board.size
  val ownershipBits = Board.ownershipBits
  val validityBits = Board.validityBits

  def place(player: Player, move: Move) {
    move.validate
    val cells = move.cells
    validate(cells, player)
    assign(player, cells)
  }

  def validate(cells: Seq[(Int, Int)], player: Player) {
    if (!onBoardAndValidFor(cells, player))
      throw new IllegalPlacementException(player + " cannot place piece at " + cells)

    val corners = getAvailableCorners(player)
    if (cells.find(point => corners.contains(point)).isEmpty)
      throw new IllegalPlacementException(player + " cannot place piece at " + cells)
  }

  /**
   *  Public for testing only. Does not validate move! Performs copy-on-write.
   */
  def assign(player: Player, cells: Seq[(Int,Int)]) {
    val neighbors = cells.flatMap({case (x,y) => sides(x,y)})

    val rowsCopied = setOwner(cells, player, Nil)
    setValidity(neighbors, player, rowsCopied)
  }

  private def setOwner(cells: Seq[(Int, Int)], player: Player, alreadyCopied: List[Int]): List[Int] = {
    cells.foldLeft(alreadyCopied)((rowsCopied, point) => {
      val (i, j) = point
      val res = copyIfNotYetCopied(i, rowsCopied)
      board(i)(j) = (validityBits | player.ownershipBit).asInstanceOf[Byte] // invalid for all plus owned by player
      res
    })
  }

  private def setValidity(neighbors: Seq[(Int, Int)], player: Player, alreadyCopied: List[Int]): List[Int] = {
    neighbors.foldLeft(alreadyCopied)((rowsCopied, point) => {
      val (i, j) = point
      val current = board(i)(j)
      if ((current & player.validityBit) == 0) {
        val res = copyIfNotYetCopied(i, rowsCopied)
        board(i)(j) = (current | player.validityBit).asInstanceOf[Byte] // whatever it was already plus invalid for player
        res
      }
      else
        rowsCopied
    })
  }
  
  private def copyIfNotYetCopied(row: Int, alreadyCopied: List[Int]): List[Int] = {
    if(alreadyCopied.contains(row))
      return alreadyCopied
    else {
      board(row) = copy(board(row))
      return row :: alreadyCopied
    }
  }

  private def copy(arr: Array[Byte]): Array[Byte] = {
    val copy = Array.ofDim[Byte](size)
    arr.copyToArray(copy)
    copy
  }

  def getAvailableCorners(player: Player) : Seq[(Int,Int)] = {
    if (isMarkedInvalid(player.corner, player))
      // middle of game
      allCells.filter(point => isMarkedValid(point, player) && touchesCorner(player, point))
    else
      // player's first move
      List(player.corner)
  }

  private def touchesCorner(player: Player, point: (Int, Int)): Boolean = {
    val (x, y) = point
    corners(x,y).find(point => hasOwner(point, player)).isDefined
  }

  def onBoardAndValidFor(cells: Seq[(Int, Int)], player: Player): Boolean = {
    cells.find(point => offBoard(point) || isMarkedInvalid(point, player)).isEmpty
  }

  /**
   *  Does not validate move!
   */
  def afterMove(player: Player, move: Move): Board = {
    val child = shallowCopy
    child.assign(player, move.cells)
    return child
  }

  private def shallowCopy(): Board = {
    val copy = Array.ofDim[Byte](size, size)
    board.copyToArray(copy)
    new Board(copy)
  }

  def accessibleGraph(player: Player): DirectedGraph[(Int,Int),Edge] = {
    val g = new DirectedSparseGraph[(Int,Int),Edge]

    validCells(player).foreach(point => {
      val (x,y) = point
      sides(x,y)
              .filter(side => isMarkedValid(side, player))
              .foreach(side => g.addEdge(Straight(point, side), point, side))
      corners(x,y)
              .filter(corner => isMarkedValid(corner, player))
              .foreach(corner => g.addEdge(Diagonal(point, corner), point, corner))
    })

    g
  }

  @inline
  private def offBoard(point: (Int, Int)): Boolean = {
    val (x, y) = point
    x < 0 || y < 0 || x >= size || y >= size
  }

  @inline
  private def isMarkedInvalid(point: (Int,Int), player: Player): Boolean = {
    val (x,y) = point
    (board(x)(y) & player.validityBit) != 0
  }

  @inline
  private def isMarkedValid(point: (Int,Int), player: Player): Boolean = {
    val (x,y) = point
    (board(x)(y) & player.validityBit) == 0
  }

  @inline
  private def hasOwner(point: (Int, Int), player: Player): Boolean = {
    val (x, y) = point
    (board(x)(y) & player.ownershipBit) != 0
  }

  def validCells(player: Player): Seq[(Int,Int)] = allCells.filter(point => isMarkedValid(point, player))

  private def allCells = (for(i<-0 to size-1) yield { for (j<-0 to size-1) yield (i,j) }).flatten

  private def corners(x: Int, y:Int): Seq[(Int,Int)] = {
    var result:List[(Int,Int)] = Nil
    if(x > 0 && y > 0)
      result = (x-1,y-1) :: result
    if(x > 0 && y < size-1)
      result = (x-1,y+1) :: result
    if(x < size-1 && y > 0)
      result = (x+1,y-1) :: result
    if(x < size-1 && y < size-1)
      result = (x+1,y+1) :: result
    return result
  }

  private def sides(x: Int, y:Int): Seq[(Int,Int)] = {
    var result:List[(Int,Int)] = Nil
    if(x > 0)
      result = (x-1,y) :: result
    if(x < size-1)
      result = (x+1,y) :: result
    if(y > 0)
      result = (x,y-1) :: result
    if(y < size-1)
      result = (x,y+1) :: result
    return result
  }

  override def toString(): String = {
    board.foldLeft("")(
      (res, row) =>
        res + "\n" + row.foldLeft("")(
          (res, owner) => res + "\t" + (owner & ownershipBits match {
            case p if p != 0 => Player.name(p.toByte)
            case _ => "."
          })
        )
    )
  }

}

object Board {
  val size = 20
  val ownershipBits = 15
  val validityBits = 15 << 4

  def empty(): Array[Array[Byte]] = {
    return Array.ofDim[Byte](size, size)
  }
}