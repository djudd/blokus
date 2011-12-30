package blokus.heuristic

import blokus.{Player, Node}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 7, 2010
 * Time: 8:53:18 PM
 * To change this template use File | Settings | File Templates.
 */

class ConstantSumHeuristic(calculator: Calculator) extends Heuristic {

  val worstScore = 0
  val bestScore = 1

  def value(node: Node, player: Player): Int = {
    val scores = Map(node.players.map(p => (p.player, calculator.value(node, p.player))): _ *)
    val sum = scores.values.sum
    if (sum > 0)
      scores(player) / sum
    else
      0
  }

}