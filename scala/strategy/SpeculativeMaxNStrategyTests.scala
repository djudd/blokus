package blokus

import heuristic.ConstantSumHeuristic
import org.specs.mock.Mockito
import org.specs.Specification
import org.junit.{Assert, Test, Before}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 7, 2010
 * Time: 9:19:47 PM
 * To change this template use File | Settings | File Templates.
 */

class SpeculativeMaxNStrategyTests extends Specification with Mockito {

  var strategy: SpeculativeMaxNStrategy = null

  val heuristic = mock[ConstantSumHeuristic]
  val p1 = mockAs[Player]("p1")
  val p2 = mockAs[Player]("p2")
  val p3 = mockAs[Player]("p3")
  val p4 = mockAs[Player]("p4")

  val start = mockAs[Node]("start")

  val child1 = mockAs[Node]("c1")
  val child1a = mockAs[Node]("c1a")
  val child1b = mockAs[Node]("c1b")
  val child2 = mockAs[Node]("c2")
  val child2a = mockAs[Node]("c2a")
  val child2b = mockAs[Node]("c2b")

  @Before
  def setUp() {
    heuristic.worstScore returns 0
    heuristic.bestScore returns 30

    val myInitialScores = Map((p1, 0), (p2, 30))
    val theirInitialScores = Map((p1, 30), (p2, 0))

    start.current returns new PlayerState(p1, Set.empty)
    start.initialScores(heuristic) returns myInitialScores

    child1.current returns new PlayerState(p2, Set.empty)
    child1.initialScores(heuristic) returns theirInitialScores

    child1a.current returns new PlayerState(p1, Set.empty)
    child1a.initialScores(heuristic) returns myInitialScores

    child1b.current returns new PlayerState(p1, Set.empty)
    child1b.initialScores(heuristic) returns myInitialScores

    child2.current returns new PlayerState(p2, Set.empty)
    child2.initialScores(heuristic) returns theirInitialScores

    child2a.current returns new PlayerState(p1, Set.empty)
    child2a.initialScores(heuristic) returns myInitialScores

    child2b.current returns new PlayerState(p1, Set.empty)
    child2b.initialScores(heuristic) returns myInitialScores

    p1.next returns p2
    p2.next returns p3
    p3.next returns p4
    p4.next returns p1

    p1.prev returns p4
    p2.prev returns p1
    p3.prev returns p2
    p4.prev returns p3

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
    child1.heuristicScores(heuristic) returns Map((p1, 20), (p2, 0), (p3, 0), (p4, 0))
    child2.heuristicScores(heuristic) returns Map((p1, 10), (p2, 0), (p3, 0), (p4, 0))

    strategy = new SpeculativeMaxNStrategy(heuristic, maxDepth=1)
    val res = strategy.search(start)

    Assert.assertEquals(child1, res)
  }

  @Test
  def depth2_usesBestMove_givenOpponentBestMove() {
    child1.heuristicScores(heuristic) returns Map((p1, 20), (p2, 0), (p3, 0), (p4, 0))
    child2.heuristicScores(heuristic) returns Map((p1, 10), (p2, 0), (p3, 0), (p4, 0))

    child1a.heuristicScores(heuristic) returns Map((p1, 5), (p2, 20), (p3, 0), (p4, 0))
    child1b.heuristicScores(heuristic) returns Map((p1, 30), (p2, 0), (p3, 0), (p4, 0))

    child2a.heuristicScores(heuristic) returns Map((p1, 20), (p2, 5), (p3, 0), (p4, 0))
    child2b.heuristicScores(heuristic) returns Map((p1, 15), (p2, 10), (p3, 0), (p4, 0))

    strategy = new SpeculativeMaxNStrategy(heuristic, maxDepth=2)
    val res = strategy.search(start)

    Assert.assertEquals(child2, res)
  }

}