package blokus.heuristic

import blokus.{Player, Node}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 11, 2010
 * Time: 11:15:19 AM
 * To change this template use File | Settings | File Templates.
 */

class LinearComboCalculator(weights: List[Double]) extends Calculator {

  val calculators = List(
    new CurrentScore,
    new AvailableCorners,
    new AvailableCornersDiameter,
    new AvgShortestPathDistance
  )

  def value(node: Node, player: Player): Int = {
    val raw = calculators.zip(weights).filter({
      case (calc, weight) => weight > 0      
    }).foldLeft(0.0)({
      case (total, (calc, weight)) => total + weight * calc.value(node, player)
    })
    Math.ceil(raw).toInt
  }

}