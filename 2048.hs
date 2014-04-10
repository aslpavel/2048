module Main where

import System.IO
import Text.Printf (printf)
import Control.Exception (bracket_)
import System.Random (StdGen, randomR, getStdGen)
import Data.List (intercalate, transpose)
import Control.Monad (foldM)
import Control.Monad.State (State, evalState, modify, get)
import Control.Applicative ((<$>))


newtype Board = Board { rows :: [[Maybe Int]] }

instance Show Board where
    show = intercalate "\n" . map showLine . rows
        where showLine = intercalate " " . map (maybe ".   " (printf "%-4d"))

data GameState = GameState {
      gen :: StdGen,  -- game random seed
      score :: Int,   -- game score
      empty :: Int    -- empty counter or new value offset
    } deriving Show

type Game a = State GameState a

columns :: Board -> [[Maybe Int]]
columns = transpose . rows

mkBoard :: Int -> Int -> Board
mkBoard h w = Board . replicate h . replicate w $ Nothing

runGame :: StdGen -> [Board -> Game Board] -> [(GameState, Board)]
runGame stdGen moves =
    evalState (evalMoves initBoard moves) initState
        where initBoard = testBoard
              initState = GameState stdGen 0 0

-- collapse row fro right to left
collapseL :: [Maybe Int] -> Game [Maybe Int]
collapseL fs = do
  e <- empty <$> get
  fs' <- foldM step [] fs
  e' <- empty <$> get
  return $ reverse fs' ++ replicate (e' - e) Nothing
      where
        -- might be better use lenses?
        modEmpty f = modify $ \s -> s { empty = f . empty $ s }
        modScore f = modify $ \s -> s { score = f . score $ s }
        -- step through fields
        step xs Nothing = modEmpty (+1) >> return xs
        step [] f = return [f]
        step ms@(x:xs) f@(Just v) =
            if x == f
            then do
              modScore (+v*2)
              modEmpty (+1)
              return . (:xs) $ (*2) <$> x
            else return (f:ms)

collapseR :: [Maybe Int] -> Game [Maybe Int]
collapseR = fmap reverse . collapseL . reverse

-- moves
moveL :: Board -> Game Board
moveL = fmap Board . mapM collapseL . rows

moveU :: Board -> Game Board
moveU = fmap (Board . transpose) . mapM collapseL . columns

moveR :: Board -> Game Board
moveR = fmap Board . mapM collapseR . rows

moveD :: Board -> Game Board
moveD = fmap (Board . transpose) . mapM collapseR . columns

-- generate next random in (l, h)
nextRandom :: Int -> Int -> Game Int
nextRandom l h = do
  g <- gen <$> get
  let (v, g') = randomR (l, h) g
  modify $ \s -> s { gen = g' }
  return v

-- add random field to the board
addValue :: Game Board -> Game Board
addValue game = do
  -- set empty to random value in (0, empty+1)
  empty <$> get >>= nextRandom 0 . (+1) >>= modEmpty . const
  v <- (*2) <$> nextRandom 1 2  -- value to be added (2|4)
  rs <- fmap rows game
  board <- Board <$> setRows v rs
  modEmpty $ const 0            -- reset empty value to 0
  return board
    where
      modEmpty f = modify $ \s -> s { empty = f . empty $ s }
      -- set single field and decrease empty counter on empyt == 0
      setField :: Int -> (Maybe Int) -> Game (Maybe Int)
      setField v Nothing = do
                  e <- empty <$> get
                  if e < 0
                  then return Nothing
                  else do modEmpty (+(-1))
                          return $ if e == 0 then Just v
                                   else Nothing
      setField _ f = return f
      -- process all rows
      setRows v = mapM (mapM (setField v))

-- eval moves on specified board
evalMoves :: Board -> [Board -> Game Board] -> Game [(GameState, Board)]
evalMoves board moves =
    get >>= \s -> ((s,board):) <$> loop moves board
        where loop [] _ = return []
              loop (m:ms) b = do
                s <- get
                b' <- m b
                ((s, b'):) <$> loop ms b'

-- read moves from stdin
getMoves :: Handle -> IO [Board -> Game Board]
getMoves h = do
  hSetBuffering h NoBuffering
  fmap parse $ hGetContents h
      where parse :: [Char] -> [Board -> Game Board]
            parse [] = []
            parse ('q':_) = []
            parse ('\ESC':'[':x:xs)
                | x == 'A' = moveU : parse xs
                | x == 'B' = moveD : parse xs
                | x == 'D' = moveL : parse xs
                | x == 'C' = moveR : parse xs
            parse (_:xs) = parse xs

-- initialize terminal for rendering
renderScope :: Handle -> IO a -> IO a
renderScope handle = bracket_ before after
    where before = do
            hSetEcho handle False
            putStr "\ESC[?25l"  -- hide cursor
            putStr "\ESC7"      -- save cursor
          after = do
            hSetEcho handle True
            putStr "\ESC[?25h"  -- show cursor

render :: (GameState, Board) -> IO ()
render (state, board) = do
  putStr "\ESC8"  -- restore cursor
  printf "Score: %d\n" $ score state
  printf "Empty (DEBUG): %d\n" $ empty state
  putStrLn . (++ "\n") . show $ board

main :: IO ()
main = renderScope stdin $ do
  moves <- getMoves stdin
  stdGen <- getStdGen
  mapM_ render $ runGame stdGen moves

-- testing board
testBoard :: Board
testBoard =
    Board [[Just 2, Just 2, Nothing, Just 4, Just 2],
           [Just 4, Nothing, Just 4, Nothing, Just 4],
           [Just 8, Nothing, Nothing, Just 2, Just 2],
           [Just 4, Nothing, Just 4, Just 2, Just 2]]

