package blokus

import heuristic.Heuristic
import org.specs.mock.Mockito
import org.specs.Specification
import org.junit.{Assert, Test, Before}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 2, 2010
 * Time: 10:51:56 PM
 * To change this template use File | Settings | File Templates.
 */

class MaxNStrategyTests extends Specification with Mockito {

  var strategy: MaxNStrategy = null

  val heuristic = mock[Heuristic]
  val player = mockAs[Player]("player")
  val opponent = mockAs[Player]("opponent")

  val start = mockAs[Node]("start")

  val child1 = mockAs[Node]("c1")
  val child1a = mockAs[Node]("c1a")
  val child1b = mockAs[Node]("c1b")
  val child2 = mockAs[Node]("c2")
  val child2a = mockAs[Node]("c2a")
  val child2b = mockAs[Node]("c2b")

  @Before
  def setUp() {
    val myInitialScores = Map((player, 0), (opponent, 1000))
    val theirInitialScores = Map((player, 1000), (opponent, 0))

    start.current returns new PlayerState(player, Set.empty)
    start.initialScores(heuristic) returns myInitialScores

    child1.current returns new PlayerState(opponent, Set.empty)
    child1.initialScores(heuristic) returns theirInitialScores

    child1a.current returns new PlayerState(player, Set.empty)
    child1a.initialScores(heuristic) returns myInitialScores

    child1b.current returns new PlayerState(player, Set.empty)
    child1b.initialScores(heuristic) returns myInitialScores

    child2.current returns new PlayerState(opponent, Set.empty)
    child2.initialScores(heuristic) returns theirInitialScores

    child2a.current returns new PlayerState(player, Set.empty)
    child2a.initialScores(heuristic) returns myInitialScores

    child2b.current returns new PlayerState(player, Set.empty)
    child2b.initialScores(heuristic) returns myInitialScores

    player.next returns opponent
    opponent.next returns player

    start.children returns List(child1, child2)

    child1.children returns List(child1a, child1b)
    child2.children returns List(child2a, child2b)

    child1a.children returns List(mock[Node])
    child1b.children returns List(mock[Node])
    child2a.children returns List(mock[Node])
    child2b.children returns List(mock[Node])
  }

  @Test
  def depth1_usesBestMove() {
    child1.heuristicScores(heuristic) returns Map((player, 20), (opponent, 0))
    child2.heuristicScores(heuristic) returns Map((player, 10), (opponent, 0))

    strategy = new MaxNStrategy(heuristic, maxDepth=1)
    val res = strategy.search(start)

    Assert.assertEquals(child1, res)
  }

  @Test
  def depth2_usesBestMove_givenOpponentBestMove() {
    child1.heuristicScores(heuristic) returns Map((player, 20), (opponent, 0))
    child2.heuristicScores(heuristic) returns Map((player, 10), (opponent, 0))

    child1a.heuristicScores(heuristic) returns Map((player, 5), (opponent, 20))
    child1b.heuristicScores(heuristic) returns Map((player, 30), (opponent, 0))

    child2a.heuristicScores(heuristic) returns Map((player, 20), (opponent, 5))
    child2b.heuristicScores(heuristic) returns Map((player, 15), (opponent, 10))

    strategy = new MaxNStrategy(heuristic, maxDepth=2)
    val res = strategy.search(start)

    Assert.assertEquals(child2, res)
  }

  @Test
  def breaksTieWithOpponentScore() {
    child1.heuristicScores(heuristic) returns Map((player, 10), (opponent, 20))
    child2.heuristicScores(heuristic) returns Map((player, 10), (opponent, 0))

    strategy = new MaxNStrategy(heuristic, maxDepth=1)
    val res = strategy.search(start)

    Assert.assertEquals(child2, res)
  }

}