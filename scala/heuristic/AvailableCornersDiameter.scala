package blokus.heuristic

import blokus.{Player, Node}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 4, 2010
 * Time: 1:17:43 PM
 * To change this template use File | Settings | File Templates.
 */

class AvailableCornersDiameter extends Calculator {

  def value(node: Node, player: Player): Int = {
    val corners = node.board.getAvailableCorners(player)
    corners.map(corner => maxDistance(corner, corners)).max
  }

  def maxDistance(point: (Int,Int), points: Seq[(Int,Int)]): Int = {
    val (x,y) = point
    points.map(distance(x,y)).max
  }

  @inline def distance(x: Int, y: Int)(point: (Int,Int)): Int = {
    val (i,j) = point
    Math.sqrt((x-i)*(x-i) + (y-j)*(y-j)).ceil.toInt
  }

}