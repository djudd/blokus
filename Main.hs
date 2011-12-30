import GameState

getNthGrandChildren 1 = getChildren newGame
getNthGrandChildren n = concatMap getChildren $ getNthGrandChildren (n-1)

main = print $ take 20 $ getNthGrandChildren 5
