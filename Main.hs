import Cmdline
import GameState

main = print $ length $ concatMap getChildren $ concatMap getChildren $ concatMap getChildren $ getChildren newGame
    
