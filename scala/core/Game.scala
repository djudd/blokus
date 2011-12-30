package blokus

import heuristic._

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 28, 2010
 * Time: 4:33:36 PM
 * To change this template use File | Settings | File Templates.
 */

class Game(console: Console, p1: Player, p2: Player, p3: Player, p4: Player) {

  private def setNexts: Unit = {
    p1.next = p2
    p2.prev = p1
    p2.next = p3
    p3.prev = p2
    p3.next = p4
    p4.prev = p3
    p4.next = p1
    p1.prev = p4
  }

  val verbose = true
  val board: Board = new Board()
  val players: List[Player] = List(p1,p2,p3,p4)

  setNexts

  def play() : List[Player] = {
    var canPlay = 4

    for(turn <- 1 to 21) {
      for(p <- players) {
        if(p.canPlay(board)) {
          p.play(board)
          if(verbose)
            console.write(board.toString)
        }
        else
          canPlay = canPlay - 1

        if(canPlay == 0)
          return players.sortBy(-_.score)
      }
    }

    if(players.find(_.canPlay(board)).isDefined)
      throw new GameTooLongException("How did the game not end?")

    return players.sortBy(-_.score)
  }

}

object Game {
  def main(args: Array[String]) {  
    play(allComputer(new Console))
    //time
  }

  def play(game: Game) {
    val console = new Console()
    val game = allComputer(console)
    val res = game.play
    res.foreach(p => println(p.name + ": " + p.score))
  }

  def time {
    val s = new SpeculativeMaxNStrategy(new ConstantSumHeuristic(new CurrentScore), 60000, maxDepth=2)
    val game = create(new Console, List(s, s, s, s))
    val start = System.currentTimeMillis
    s.nextMove(game.players(0), game.board)
    val total = (System.currentTimeMillis - start) / 1000.0
    println("Finished in " + total + " seconds")
  }

  def allComputer(console: Console): Game = {
    // current score, # available corners, diameter available corners, average shortest path 

    val c1 = new LinearComboCalculator(List(1, 0, 0, 1))
    val c2 = new LinearComboCalculator(List(1, 0, 0, 1))
    val c3 = new LinearComboCalculator(List(1, 0, 0, 0))
    val c4 = new LinearComboCalculator(List(1, 0, 0, 1))

    val s1 = new MaxNStrategy(new SimpleHeuristic(c1), 20000)
    val s2 = new MaxNStrategy(new SimpleHeuristic(c2), 20000)
    val s3 = new MaxNStrategy(new SimpleHeuristic(c3), 20000)
    val s4 = new MaxNStrategy(new SimpleHeuristic(c4), 20000)
    
    create(console, List(s1, s2, s3, s4))
  }

  def allHuman(console: Console): Game = {
    val strategy = new ManualStrategy(console)
    create(console, List(strategy, strategy, strategy, strategy))
  }

  def create(console: Console, strategies: List[Strategy]): Game = {
    val p1 = new Player(0,0, strategy=strategies(0), id=1)
    val p2 = new Player(0,19, strategy=strategies(1), id=2)
    val p3 = new Player(19,19, strategy=strategies(2), id=3)
    val p4 = new Player(19,0, strategy=strategies(3), id=4)
    new Game(console, p1, p2, p3, p4)
  }

}