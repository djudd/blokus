package blokus

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 3, 2010
 * Time: 9:03:45 PM
 * To change this template use File | Settings | File Templates.
 */

trait IterativeDeepeningStrategy extends AntagonisticSearchStrategy {

  val time: Long
  val maxDepth: Int
  var nodesSearched: Int = 0
  var nodesPruned: Int = 0

  def search(node: Node) : Node = {
    val start = System.currentTimeMillis
    val timeout = start + time

    var children = node.children
    var depth = 1

    nodesSearched = 0
    nodesPruned = 0

    while(depth <= maxDepth && System.currentTimeMillis < timeout) {
      children = search(node, children, depth, timeout)
      if(System.currentTimeMillis < timeout) depth += 1
    }

    val move = children.head.move

    if(move != null) {
      println("player " + node.current.player.name + " completed depth " + (depth-1)
        + " in " + (System.currentTimeMillis - start) + " and played " + move.piece
        + " at " + move.cells + " after searching " + nodesSearched + " and pruning " + nodesPruned)
    }

    return children.head
  }

  def search(node: Node, children: Seq[Node], depth: Int, timeout: Long): Seq[Node]

}