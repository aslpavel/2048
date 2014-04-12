module Main (main) where

import Text.Printf (printf)
import Control.Exception (bracket_)
import System.Random (StdGen, randomR, getStdGen)
import Data.List (intercalate, transpose, delete)
import Control.Monad (foldM, when)
import Control.Monad.State (State, evalState, modify, get)
import Control.Applicative ((<$>))
import System.IO (hSetEcho, hSetBuffering, hGetContents,
                  stdin, Handle, BufferMode(..))

-- board
newtype Board = Board { rows :: [[Maybe Int]] }

instance Show Board where
    show = intercalate "\n" . map showLine . rows
        where showLine = intercalate " " . map (maybe ".   " (printf "%-4d"))

columns :: Board -> [[Maybe Int]]
columns = transpose . rows

mkBoard :: Int -> Int -> Board
mkBoard w h = Board . replicate h . replicate w $ Nothing

-- flags
data GameFlag = Moved | Won | Lost deriving (Eq, Show)

-- state
data GameState = GameState {
      gen :: StdGen,      -- game random seed
      score :: Int,       -- game score
      empty :: Int,       -- empty counter or new value offset
      flags :: [GameFlag] -- game flags
    } deriving Show

-- whole game
type Game a = State GameState a

-- state modfiers
modifyEmpty :: (Int -> Int) -> Game ()
modifyEmpty f = modify $ \s -> s { empty = f . empty $ s }

modifyScore :: (Int -> Int) -> Game ()
modifyScore f = modify $ \s -> s { score = f . score $ s }

takeFlag :: GameFlag -> Game Bool
takeFlag f = do
  p <- elem f . flags <$> get
  when p $ modify $ \s -> s { flags = delete f $ flags s }
  return p

putFlag :: GameFlag -> Game ()
putFlag f = do
  p <- not . elem f . flags <$> get
  when p $ modify $ \s -> s { flags = f : flags s }

-- game runner
runGame :: StdGen -> Int -> Int -> [Board -> Game Board]
        -> [(GameState, Board)]
runGame stdGen w h moves = evalState game initState
    where initState = GameState stdGen 0 0 [Moved]
          initBoard = mkBoard w h
          game = evalMoves initBoard $ ((addValue . checkLost).) <$> moves

-- collapse row fro right to left
collapseL :: [Maybe Int] -> Game [Maybe Int]
collapseL fs = do
  e <- empty <$> get
  xs' <- foldM step [] fs
  e' <- empty <$> get
  let fs' = reverse xs' ++ replicate (e' - e) Nothing
  when (fs /= fs') $ putFlag Moved
  return fs'
      where
        -- step through fields
        step xs Nothing = modifyEmpty (+1) >> return xs
        step [] f = return [f]
        step ms@(x:xs) f@(Just v) =
            if x == f
            then do
              modifyScore (+v*2)
              modifyEmpty (+1)
              when (v == 1024) $ putFlag Won  -- game is won
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

-- set Lost flag if game is lost
checkLost :: Game Board -> Game Board
checkLost game = do
  b <- game
  e <- empty <$> get
  if e /= 0 then return b
  else do
    -- try to move
    moveL b
    moveU b
    m <- takeFlag Moved
    if m then game -- restore state
    else do
      putFlag Lost
      return b

-- add random field to the board
addValue :: Game Board -> Game Board
addValue game = do
  board <- game
  moved <- takeFlag Moved
  if not moved  -- dont add anything if nothing is moved
  then do
    modifyEmpty $ const 0
    return board
  else do
    -- set empty to random value in (0, empty-1)
    empty <$> get >>= nextRandom 0 . (subtract 1) >>= modifyEmpty . const
    value <- (*2) <$> nextRandom 1 2  -- value to be added (2|4)
    board' <- Board <$> setRows value (rows board)
    modifyEmpty $ const 0             -- reset empty value to 0
    return board'
        where
          -- set single field and decrease empty counter on empyt == 0
          setField :: Int -> (Maybe Int) -> Game (Maybe Int)
          setField v Nothing = do
                  e <- empty <$> get
                  if e < 0
                  then return Nothing
                  else do modifyEmpty $ subtract 1
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
                b' <- m b
                s' <- get
                l <- takeFlag Lost
                if l then return [(s', b')]
                else ((s', b'):) <$> loop ms b'

-- read moves from handle
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
  putStrLn $ "Flags: " ++ intercalate " " (show <$> flags state)
  putStrLn . (++ "\n") . show $ board

main :: IO ()
main = renderScope stdin $ do
  moves <- getMoves stdin
  stdGen <- getStdGen
  mapM_ render $ runGame stdGen 4 4 moves

