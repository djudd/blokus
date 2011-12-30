package blokus

import org.specs.mock.Mockito
import org.specs.Specification
import org.junit.{Test, Before}
import org.junit.Assert.{assertEquals,assertNotNull}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 30, 2010
 * Time: 8:23:25 PM
 * To change this template use File | Settings | File Templates.
 */

class ManualStrategyTests extends Specification with Mockito {

  var strat: ManualStrategy = null
  var console: Console = null
  var board: Board = null
  var player: Player = null

  @Before
  def setUp {
    console = mock[Console]
    board = mock[Board]
    player = mock[Player]
    strat = new ManualStrategy(console)
  }

  @Test
  def valid_one_corner {
    console.readline(any[String]) returns "One:0,0:0,0"
    val move = strat.nextMove(player, board)
    assertEquals(Move(One(), (0,0), List((0,0))), move)
  }

  @Test
  def valid_two_corner {
    console.readline(any[String]) returns "Two:0,0:0,0;1,0"
    val move = strat.nextMove(player, board)
    assertEquals(Move(Two(), (0,0), List((0,0), (1,0))), move)
  }

  @Test
  def valid_two_middle {
    console.readline(any[String]) returns "Two:10,10:0,0;0,1"
    val move = strat.nextMove(player, board)
    assertEquals(Move(Two(), (10,10), List((0,0), (0,1))), move)
  }

  @Test
  def ignoresWhitespace {
    console.readline(any[String]) returns "  One : 0, 0 : 0 ,0\n"
    val move = strat.nextMove(player, board)
    assertEquals(Move(One(), (0,0), List((0,0))), move)
  }

  @Test
  def parseError_waitsForValid {
    console.readline(any[String]) returns "%^&%*&^" thenReturns "One:0,0:0,0"
    val move = strat.nextMove(player, board)
    assertEquals(Move(One(), (0,0), List((0,0))), move)
  }

  @Test
  def pieceDoesNotMatchPlacement_waitsForValid {
    console.readline(any[String]) returns "One:0,0:0,0;1,0" thenReturns "One:0,0:0,0"
    val move = strat.nextMove(player, board)
    assertEquals(Move(One(), (0,0), List((0,0))), move)
  }

  @Test
  def cellsNotAvailable_waitsForValid {
    console.readline(any[String]) returns "One:0,0:19,19" thenReturns "One:0,0:0,0"
    board.validate(List((0,0), (19,19)), player) throws new IllegalPlacementException("test")
    val move = strat.nextMove(player, board)
    assertEquals(Move(One(), (0,0), List((0,0))), move)
  }
  
}