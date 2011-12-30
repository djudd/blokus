package blokus.heuristic

import blokus.{Player, Node}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 8, 2010
 * Time: 8:53:11 AM
 * To change this template use File | Settings | File Templates.
 */

class SimpleHeuristic(calculator: Calculator) extends Heuristic {

  def value(node: Node, player: Player): Int = calculator.value(node, player)

  val worstScore = 0
  val bestScore = 1000 // should be Inf

}