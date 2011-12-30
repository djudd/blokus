package blokus.heuristic

import blokus.{Node, Player}
import blokus.core.{Edge, Synthetic}
import edu.uci.ics.jung.algorithms.shortestpath.{DistanceStatistics, DijkstraShortestPath}
import scala.collection.JavaConversions._

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 18, 2010
 * Time: 4:48:26 PM
 * To change this template use File | Settings | File Templates.
 */

class AvgShortestPathDistance extends Calculator {

  def value(node: Node, player: Player): Int = {
    val board = node.board
    val graph = board.accessibleGraph(player)
    val source = (-1,-1)

    board.getAvailableCorners(player).foreach(
      point => graph.addEdge(Synthetic(source, point), source, point)
    )
    
    val sp = new DijkstraShortestPath(graph, Edge.weight)
    val distances = sp.getDistanceMap(source, board.validCells(player)).values
    38 - distances.foldLeft(0)((tot, dist) => tot + dist.intValue) / distances.size
  }

}