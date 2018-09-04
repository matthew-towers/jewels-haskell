import System.Random
import Data.Array.IO
import Data.Array.MArray
import System.Environment

width = 8 :: Int
height = 8 :: Int
numberOfColours = 7 :: Int
vanishLength = 3 :: Int

type Position = (Int, Int)
type Move = (Position, Position)
type Score = IO Int
type Game = IOArray Position Int

--Game will store the state of the board in positions
--(1,1),...,(height,width) and the score at position (0,0).
--(like a dartboard with the score written at the side)
--Unpleasant but probably not a big deal, and it saves us the trouble
--of figuring out how to carry around a mutable Int containing the
--score.

score :: Game -> Score
score game = readArray game (0,0)

applyMove :: Move -> Game -> IO ()
applyMove move game = do {
    arr_xy1 <- readArray game xy1;
    arr_xy2 <- readArray game xy2;
    writeArray game xy1 arr_xy2;
    writeArray game xy2 arr_xy1 }
        where xy1 = fst move
              xy2 = snd move

addToScore :: Int -> Game -> IO ()
addToScore n game = do {
    currentScore <- readArray game (0,0);
    writeArray game (0,0) (n + currentScore)}

randomGame :: IO Game
randomGame = do {
    arr <- newArray ((0,0),(width,height)) undefined :: IO Game;
    writeArray arr (0,0) 0; -- the score
    sequence_ $ do {
        i <- [1..height];
        j <- [1..width];
        return $ (randomRIO (1,numberOfColours)) >>= writeArray arr (i,j) };
    return arr}

pr :: Game -> IO ()
-- pr game displays the current board and score
pr arr = do {
    sc <- readArray arr (0,0);
    putStrLn $ "score: " ++ (show sc) ++ "\n";
    sequence_ $ do { -- in list
       i <- [1..height];
       j <- [1..width];
       return $ do { -- in IO
        aij <- readArray arr (i,j);
        putStr ((show aij) ++ " ");
        if j == width
            then putChar '\n'
            else return () }}}

findMonos :: Game -> IO [[Position]]
-- return a list whose elements are list of the coordinates of the monos
-- in the game
findMonos game = do {
    ho <- horizMonos game;
    ve <- vertMonos game;
    return $ ho ++ ve}

allowable :: Game -> Move -> IO Bool
--does this move leave the game board with at least one mono?
allowable game move = do {
    applyMove move game;
    mo <- findMonos game; -- this is inefficient because it examines the whole board, but we need only look at the rows/cols affected by move.  Just using the appropriate fcm and frm would improve things a little. This will get called a lot, so there's probably significant improvement to be found here.
    applyMove move game; -- undoing the first application
    if (length mo > 0)
        then return True;
        else return False}

allMoves :: [Move]
-- list of all moves that could possibly be applied
allMoves = [((i,j),(i+1,j)) | i <- [1..(height - 1)], j <- [1..width]] ++ [((i,j),(i,j+1)) | i <- [1..height], j <- [1..(width-1)]]

allowableMoves :: Game -> IO [Move]
-- a move is "playable" if after doing it, there's a mono on the board.
-- This function returns a list of all playable moves, wrapped in IO
allowableMoves game = valid game allMoves
    where valid game [] = return []
          valid game (x:xs) = do {
                ok <- allowable game x;
                if ok
                    then do {vs <- valid game xs;
                         return (x:vs)}
                    else valid game xs }
--TODO: write filterIO :: (a -> IO Bool) -> [a] -> IO [a]

choose :: [Move] -> IO Move
-- selects a move at random from the given list
choose moves = do {
    randomIndex <- randomRIO (0, length moves - 1);
    -- randomRIO :: (a,a) -> IO a
    return $ moves !! randomIndex}

zap :: Game -> IO ()
-- find the monos, update the score, zero out the monos, let the blocks
-- fall down and then randomly fill the spaces at the top
zap game = do {
    monos <- findMonos game;
    addToScore (sum (map (\ m -> (length m) - vanishLength + 1) monos)) game; -- we'll end up running through the list of monos more than once, but let's keep it simple for now
    sequence_ [writeArray game posn 0 | posn <- (concat monos)];
    sequence_ [ do {
                    cj <- getcol game j;
                    cjz <- zapcol cj;
                    setcol game j cjz} | j <- [1..width] ] }
    
zapcol :: [Int] -> IO [Int]
-- take a column represented as a list of Ints with 0 representing an
-- empty space. Remove 0s, shifting everything else "down", and fill the
-- top randomly.
zapcol c = do {
    let {cWithoutZeroes = filter ( /= 0 ) c};
    g <- newStdGen;
    return $ (take (height - (length cWithoutZeroes)) (randomRs (1,numberOfColours) g)) ++ cWithoutZeroes }
    
zapUntilNoMonos :: Game -> IO ()
-- repeatedly zap until there are no monos on the board.
zapUntilNoMonos game = do {
    mono <- findMonos game;
    if (null mono)
        then return ()
        else do { zap game;
             zapUntilNoMonos game }}

playout :: Game -> Score -- a Score is an IO Int
-- play a game until there are no further moves available, then return
-- the score
playout game = do {
    zapUntilNoMonos game;
    moves <- allowableMoves game;
    if (null moves)
        then score game
        else do { move <- choose moves;
                  applyMove move game;
                  playout game} }
             
blocks2 :: (Eq b) => [(a,b)] -> [[(a,b)]]
-- split a list of tuples into sublists, where each sublist has maximum
-- length subject to the second elements being equal
blocks2 [] = []
blocks2 w@(x:xs) = i : (blocks2 rest)
    where (i, rest) = span (\ r -> ((snd r) == (snd x))) w

longBlocks2 :: (Eq b) => [(a,b)] -> [[(a,b)]]
--do blocks2, then throw out anything shorter than vanishLength
longBlocks2 = (filter (\ x -> (length x) >= vanishLength)) . blocks2

getrow :: Game -> Int -> IO [Int]
getrow arr i = sequence [readArray arr (i,j) | j <- [1..width]]

getcol :: Game -> Int -> IO [Int]
getcol arr j = sequence [readArray arr (i,j) | i <- [1..height]]

setcol :: Game -> Int -> [Int] -> IO ()
setcol game j vals = sequence_ [ writeArray game (i,j) (vals !! (i-1)) | i <- [1..height]] 
    
frm :: Int -> Game -> IO [[Position]]
--frm i game gets a list of the coordinates of any monos in the
--ith row, as a stepping stone towards findMonos
frm i arr = do {
    rowi <- getrow arr i;
    return (map (map fst) (longBlocks2 (zip coords rowi)))}
        where coords = [(i,j) | j <- [1..width]]

fcm :: Int -> Game -> IO [[Position]]
-- like frm, but for columns
fcm j arr = do {
    colj <- getcol arr j;
    return (map (map fst) (longBlocks2 (zip coords colj)))}
        where coords = [(i,j) | i <- [1..height]]
    
horizMonos :: Game -> IO [[Position]]
-- return a list whose elements are lists of the coords of any
-- horizontal monos in the game
horizMonos arr = do {
  hm <- sequence [ frm i arr | i <- [1..height] ];
  return (concat hm) }

vertMonos :: Game -> IO [[Position]]
vertMonos arr = do {
  vm <- sequence [ fcm j arr | j <- [1..width] ];
  return (concat vm) }

meansd :: [Int] -> (Float, Float)
--take a list of ints, return (its mean, its standard dev)
--none of this is good, see Bird pp.151-152, but it doesn't matter since
--we only call it once
meansd xs = (m,s)
    where n = fromIntegral (length xs)
          m = (fromIntegral (sum xs)) / n
          s = sqrt $ (1/(n-1)) * sum (map (\ x -> ((fromIntegral x) - m) ^2) xs)

playoutNGames :: Int ->  IO [Int]
--play out n games, return a list of the scores wrapped in IO
playoutNGames n = sequence $ replicate n (do { u <- randomGame; playout u})

ngamestats :: Int -> IO (Float, Float)
ngamestats n = fmap meansd (playoutNGames n)

main :: IO ()
main = do {
    args <- getArgs;
    results <- ngamestats (read (head args) :: Int);
    putStrLn (show results) }

-- at the moment, on the X250, jewels2 100 takes 45-50s.
