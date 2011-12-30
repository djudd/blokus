package blokus

import heuristic.Heuristic
import util.Random

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 1, 2010
 * Time: 7:50:54 AM
 * To change this template use File | Settings | File Templates.
 */

class MaxNStrategy(
        val heuristic: Heuristic,
        val time: Long=10000, // 10 seconds
        val maxDepth: Int=100
) extends IterativeDeepeningStrategy {

  def search(node: Node, children: Seq[Node], depth: Int, timeout: Long) : Seq[Node] = {
    val (nodes, _) = minimax(node, children, depth, timeout)
    return nodes
  }

  def minimax(node: Node, children: Seq[Node], depth: Int, timeout: Long): (Seq[Node], Map[Player,Int]) = {
    if (children.isEmpty) {
      return (Nil, node.terminalScores(heuristic))
    }
    if (depth <= 0) {
      return (Nil, node.heuristicScores(heuristic))
    }    

    val currentPlayer = node.current.player
    val default = node.initialScores(heuristic)

    val scoredChildren = Map(children.map(child =>
      (child, getScores(child, depth, timeout, default))
    ): _ *)

    val sortedChildren = children.sortBy(child =>
      sortKey(child, scoredChildren, currentPlayer)
    )

    return (sortedChildren, scoredChildren(sortedChildren.head))
  }

  private def getScores(child: Node, depth: Int, timeout: Long, default: Map[Player, Int]): Map[Player, Int] = {
    if(System.currentTimeMillis > timeout)
      return default

    nodesSearched = 1 + nodesSearched
    val (_, scores) = minimax(child, child.children, depth-1, timeout)
    return scores
  }

  private def sortKey(child: Node, scoredChildren: Map[Node,Map[Player, Int]], player: Player): (Int,Int,Int) = {
    val scores = scoredChildren(child)
    return (-scores(player), -advantage(scores, player), Random.nextInt)
  }

  private def advantage(scores: Map[Player, Int], player: Player): Int = {
    val myScore = scores(player)
    return myScore - scores.filterKeys(p => p != player).values.max
  }

}