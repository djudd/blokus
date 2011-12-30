package blokus

import heuristic.ConstantSumHeuristic
import util.Random

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 1, 2010
 * Time: 7:50:54 AM
 *
 * See page 80 of:
 * http://webdocs.cs.ualberta.ca/~nathanst/papers/multiplayergamesthesis.pdf
 */

class SpeculativeMaxNStrategy(
        val heuristic: ConstantSumHeuristic,
        val time: Long=10000, // 10 seconds
        val maxDepth: Int=100
) extends IterativeDeepeningStrategy {

  def search(node: Node, children: Seq[Node], depth: Int, timeout: Long) : Seq[Node] = {
    val (nodes, _) = specmax(node, children, 0, 0, 0, depth, timeout)
    return nodes
  }

  def specmax(
          node: Node,
          children: Seq[Node],
          parentScore: Int,
          grandParentScore: Int,
          greatGrandParentScore: Int,
          depth: Int,
          timeout: Long
  ): (Seq[Node], Map[Player,Int]) =
  {
    if (children.isEmpty) {
      return (Nil, node.terminalScores(heuristic))
    }
    if (depth <= 0) {
      return (Nil, node.heuristicScores(heuristic))
    }

    val currentPlayer = node.current.player
    val previousPlayer = currentPlayer.prev
    val playerBeforePrevious = previousPlayer.prev

    var best: Map[Player,Int] = null

    var childQueue = children.toList
    var specPrunedQueue: List[Node] = Nil

    val default = node.initialScores(heuristic)
    var scoredChildren = Map(children.map(child => (child, default)): _ *)

    while(!childQueue.isEmpty && System.currentTimeMillis < timeout) {
      val child = childQueue.head
      childQueue = childQueue.tail

      val curBest = if (best != null) best(currentPlayer) else 0
      val prevBest = if (best != null) best(previousPlayer) else 0
      val prevPrevBest = if (best != null) best(playerBeforePrevious) else 0

      val nextGrandParentScore = if (prevBest <= parentScore) parentScore else 0
      val nextGreatGrandParentScore = if (prevPrevBest <= grandParentScore) grandParentScore else 0

      val (_, scores) = specmax(child, child.children, curBest, nextGrandParentScore, nextGreatGrandParentScore, depth-1, timeout)

      if (scores != null) {
        scoredChildren = scoredChildren.update(child, scores)
        nodesSearched = 1 + nodesSearched
      }
      else {
        nodesPruned = 1 + nodesPruned
      }

      if (best == null)
        best = scores
      else if (scores == null) // child was speculatively pruned
        specPrunedQueue = child :: specPrunedQueue
      else if (best(currentPlayer) < scores(currentPlayer)) {
        best = scores

        // if we find a better move we have to re-search
        if (best(previousPlayer) > parentScore || best(playerBeforePrevious) > grandParentScore) {
          nodesPruned = nodesPruned - childQueue.length
          childQueue = specPrunedQueue ::: childQueue
        }
      }

      val newCurBest = if (best != null) best(currentPlayer) else 0

      if (greatGrandParentScore + grandParentScore + parentScore + newCurBest > heuristic.bestScore)
        return (Nil, null) // speculatively prune
    }

    val sortedChildren = children.sortBy(child =>
      sortKey(child, scoredChildren, currentPlayer)
    )

    return (sortedChildren, best)
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