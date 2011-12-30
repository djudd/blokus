package blokus

import org.specs.Specification
import org.specs.mock.Mockito
import org.junit.{Assert, Test, Before}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 29, 2010
 * Time: 8:15:24 PM
 * To change this template use File | Settings | File Templates.
 */

class GameTests extends Specification with Mockito {

  var game: Game = null
  var p1: Player = null
  var p2: Player = null
  var p3: Player = null
  var p4: Player = null

  @Before
  def setUp {
    p1 = mock[Player]
    p2 = mock[Player]
    p3 = mock[Player]
    p4 = mock[Player]
    game = new Game(mock[Console], p1,p2,p3,p4)
  }

  @Test
  def placeUntilOnePlayerCantPlay() {
    p1.canPlay(any[Board]) returns true thenReturns true thenReturns true thenReturns false
    p2.canPlay(any[Board]) returns true
    p3.canPlay(any[Board]) returns true
    p4.canPlay(any[Board]) returns true

    game.play()

    there were 3.times(p1).play(any[Board])
    there were 3.times(p2).play(any[Board])
    there were 3.times(p3).play(any[Board])
    there were 3.times(p4).play(any[Board])
  }

  @Test
  def returnsPlayersScoringBestToWorst() {
    p1.score returns 15
    p2.score returns 20
    p3.score returns 25
    p4.score returns 10

    val result = game.play()

    Assert.assertEquals(List(p3, p2, p1, p4), result)
  }

  @Test(timeout=1000)
  def throwsExceptionAfter21Turns() {
    p1.canPlay(any[Board]) returns true
    p2.canPlay(any[Board]) returns true
    p3.canPlay(any[Board]) returns true
    p4.canPlay(any[Board]) returns true

    try {
      game.play()
      Assert.fail()
    }
    catch {
      case e:GameTooLongException =>
      case e => throw e
    }
  }

}