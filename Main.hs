import Types
import Cmdline
import GameState
import Search

main' = print $ head $ concatMap getChildren $ concatMap getChildren $ concatMap getChildren $ concatMap getChildren $ getChildren newGame
main = print $ minimax 4 newGame
    
