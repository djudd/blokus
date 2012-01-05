import Cmdline
import GameState
import Search

main = print $ minimax 4 newGame

--main = print $ length $ concatMap getChildren $ concatMap getChildren $ concatMap getChildren $ getChildren newGame
    
