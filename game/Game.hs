--
import Base
import System.IO
-- 1)

opposite :: Direction -> Direction 
opposite North = South
opposite South = North
opposite East = West
opposite West = East

-- 2)

noActions ::  Item -> GameState -> Next GameState
noActions item newState = Same ("The " ++ show item ++ " cannot be used here.")

-- 3)

winningRoom :: Room
winningRoom = Room "vault" "filled with gold coins" True (Just Key) [] [] [] noActions

-- 4)

startRoom :: Room
startRoom = Room "small room" "a confined space with multiple doors" False Nothing [(Spoon, "On the floor between your feet,")] [] [(West, darkRoom), (North, winningRoom)] noActions

-- 5)
--(West, darkRoom),
darkRoom :: Room
darkRoom = Room "a dark room" "an eerie place" False Nothing [] [(WoodTroll 10 Key)] [] action

rm :: GameState -> Room
rm(GS _ rm) = rm

action :: Item -> GameState -> Next GameState
action Spoon (GS p r) = 
    case monsters r of 
    [] -> Same "there are no monsters in this room"
    ((WoodTroll h i ): ms ) -> 
        if h == 10
    then 
        let
            r' = r { monsters = (WoodTroll 5 i) : ms}
            in Progress "you hit the troll with the spoon and it takes 5 damage" (GS p r')
    else
        if h == 5
            then 
                let
                r' = r {monsters = []} {items = [(Key,"On the dead troll,")]}
                in Progress "you kill the troll!" (GS p r') 
            else Same "there are no monsters in this room"

-- 6)

player1 :: Player
player1 = Player "Player 1" []

game0 :: GameState
game0 = GS player1 startRoom

-- 7)

instance Parsable Item where
    parse "spoon" = Just Spoon
    parse "key" = Just Key
    parse _ = Nothing

-- 8)

instance Parsable Direction where
    parse "north" = Just North
    parse "south" = Just South
    parse "east" = Just East 
    parse "west" = Just West
    parse _ = Nothing

-- 9) 

instance Parsable Command where
    parse ('g':'o':' ': y) = 
        case parse y of
            Just North -> Just (Move North)
            Just South -> Just (Move South)
            Just East -> Just (Move East)
            Just West -> Just (Move West)
            otherwise -> Nothing

    parse ('g':'r':'a': 'b' : ' ' : y) = 
        case parse y of
            Just Spoon -> Just (PickUp Spoon)
            Just Key -> Just (PickUp Key)
            otherwise -> Nothing

    parse ('u':'s':'e' : ' ' : y) = 
        case parse y of
            Just Spoon -> Just (Use Spoon)
            Just Key -> Just (Use Key)
            otherwise -> Nothing

    parse "end" = Just End

    parse "help" = Just Help

-- 10)
tellResponse  s = putStrLn $ "< " ++ s ++ "."

-- 11)

readCommand ::  IO (Maybe Command)
readCommand = do
    putStr "> "
    hFlush stdout
    command <- getLine
    return $ (parse command)

-- 12)

deleteFrom ::  Eq a => a -> [(a, b)] -> [(a, b)]
deleteFrom z [] = []
deleteFrom z ((x,y) : xs) = 
    if z == x
        then deleteFrom z xs
        else (x,y) : deleteFrom z xs

--13)

leaveRoom ::  Room -> Direction -> Room -> Room
leaveRoom fromRoom dir toRoom = 
   case doors fromRoom of
        [] -> toRoom {doors = []}
        ((dir, r) : ds) ->
            let
            doors toRoom = deleteFrom dir (doors toRoom)
            in toRoom {doors = (opposite dir, fromRoom) : ds} 

--14)

step ::  Command -> GameState -> Next GameState
step comm (GS p r) = 
   case comm of 
    (Move y) -> 
        case lookup y (doors r) of
            Nothing -> Same "there are no doors in this direction"
            Just rom ->
                case requires rom of 
                    Nothing ->
                        let
                            r' = leaveRoom r y rom
                            in Progress "you go through door" (GS p r')
                    (Just Key) ->
                        if inventory p == [Key]
                            then
                                let
                                    r' = leaveRoom r y rom
                                    in Progress "you go through door" (GS p r')
                            else
                                Same "You do not possess the key for the door"
    (PickUp y) ->
        case items r of
            [] -> Same "there are no items in room"
            ((i, s): is) ->
                if i == y
                    then
                        let
                            r' = r {items = deleteFrom i (items r)} 
                            p' = p {inventory = [i]}
                            in Progress ("you pickup the item") (GS p' r') 
                    else 
                        Same ("There is no " ++ show y ++ " in this room")
    (Use y) ->
        case inventory p of
            [] -> Same "No items in inventory"
            ((item : items)) ->
                if (item == Spoon)
                    then
                        action item (GS p r)
                    else 
                        Same "no usable items"
            

    

--15)

play ::  GameState -> IO ()
play (GS p r) = do
    tellContext (GS p r)
    playLoop (GS p r)

playLoop :: GameState -> IO ()
playLoop (GS p r) = do
    case isWinRoom r of
        True -> putStrLn ("You won, well done!")
        False -> 
            do
            x <- readCommand
            case x of
                Just End -> putStrLn ("you ended the game. Come back soon!")
                Just Help -> 
                    do
                            putStrLn (" :: Help Menu ::")
                            putStrLn ("Commands")
                            putStrLn ("Move (dir d) - will go through door in direction d if you meet the requirements.")
                            putStrLn ("PickUp (item a) - will pick up item a if it is in the room.")
                            putStrLn ("Use (item a) - will use item a (hint, hit a troll with a spoon).")
                            putStrLn ("End - will end the game.")
                            playLoop (GS p r)
                Just (Move North) -> 
                    case step (Move North) (GS p r) of
                        Same "You do not possess the key for the door" -> 
                            do
                            putStrLn ("You do not possess the key for the door")
                            tellContext (GS p r)
                            playLoop (GS p r) 
                        Progress "you go through door" (GS p r') ->
                            do
                            putStrLn ("you go through door")
                            tellContext (GS p r')
                            playLoop (GS p r')
                Just (Move West) -> 
                    case step (Move West) (GS p r) of
                        Progress "you go through door" (GS p r') ->
                            do
                            putStrLn ("you go through door")
                            tellContext (GS p r')
                            playLoop (GS p r') 
                Just (Move South) -> 
                    case step (Move South) (GS p r) of
                        Same "there are no doors in this direction" -> 
                            do
                            putStrLn ("there are no doors in this direction")
                            tellContext (GS p r)
                            playLoop (GS p r) 
                        Progress "you go through door" (GS p r') ->
                            do
                            putStrLn ("you go through door")
                            tellContext (GS p r')
                            playLoop (GS p r') 
                Just (Move East) -> 
                    case step (Move East) (GS p r) of
                        Same "there are no doors in this direction" -> 
                            do
                            putStrLn ("there are no doors in this direction")
                            tellContext (GS p r)
                            playLoop (GS p r)
                        Progress "you go through door" (GS p r') ->
                            do
                            putStrLn ("you go through door")
                            tellContext (GS p r')
                            playLoop (GS p r')  
                Just (PickUp Spoon) ->
                    case step (PickUp Spoon) (GS p r) of
                        Progress ("you pickup the item") (GS p' r') ->
                            do
                            putStrLn ("you pickup the item")
                            tellContext (GS p' r')
                            playLoop (GS p' r')
                Just (PickUp Key) ->
                    case step (PickUp Key) (GS p r) of
                        Progress ("you pickup the item") (GS p' r') ->
                            do
                            putStrLn ("you pickup the item")
                            tellContext (GS p' r')
                            playLoop (GS p' r')
                Just (Use Spoon) ->
                    case step (Use Spoon) (GS p r) of
                        Same "No items in inventory" ->
                            do
                            putStrLn ("No items in inventory")
                            tellContext (GS p r)
                            playLoop (GS p r)
                        Same "no usable items" -> 
                            do
                            putStrLn ("no usable items")
                            tellContext (GS p r)
                            playLoop (GS p r)
                        Progress "you hit the troll with the spoon and it takes 5 damage" (GS p r') ->
                            do
                            putStrLn ("you hit the troll with the spoon and it takes 5 damage")
                            tellContext (GS p r')
                            playLoop (GS p r')
                        Progress "you kill the troll!" (GS p r') ->
                            do
                            putStrLn ("you kill the troll!")
                            tellContext (GS p r')
                            playLoop (GS p r')
                
 
 
-- 16)

main :: IO ()
main = do 
    play game0
