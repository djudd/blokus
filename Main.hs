import Types
import Cmdline
import GameState
import Search
import Placement -- TODO remove
import Offset

main = print $ minimax 4 newGame
    
