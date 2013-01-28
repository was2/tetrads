-----------------------------------------------------------------------------
--
-- Module      :  TetradsModel
-- Copyright   :  William Sawyer
-- License     :  Gnu General Public License v3
--
-- Maintainer  :  William Sawyer
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module TetradsModel

where

import Data.List
import System.Random

data GameState = GameState {
                 playFieldRows :: Int, playFieldCols :: Int,
                 currentBlock :: Block, randomGen :: StdGen,
                 cellsByRow :: [[Bool]], cellsByCol :: [[Bool]] }
                 deriving (Show)

data GameInput = MoveUp
               | MoveDown
               | MoveRight
               | MoveLeft
               | Rotate
               deriving (Eq)

data Block = Block {
             blockBitMap :: [[Bool]],
             blockWidth :: Int,
             blockHeight :: Int,
             blockRow :: Int,
             blockCol :: Int }
             deriving (Eq, Show)

data BlockType = LBlock
               | SquareBlock
               | TBlock
               | BarBlock
               | SBlock
               deriving (Show)

spawnNewBlock :: GameState -> (StdGen, Block)
spawnNewBlock game = case (randomResult `mod` 5) of
                     0 -> (newRandomGen, newBlock LBlock)
                     1 -> (newRandomGen, newBlock TBlock)
                     2 -> (newRandomGen, newBlock BarBlock)
                     3 -> (newRandomGen, newBlock SquareBlock)
                     4 -> (newRandomGen, newBlock SBlock)
                     where genPair = next (randomGen game)
                           randomResult = fst(genPair)
                           newRandomGen = snd(genPair)


newBlock :: BlockType -> Block
newBlock LBlock = Block { blockBitMap = [[True, False, False, False],
                                         [True, True,  True,  True]],
                                         blockWidth = 4, blockHeight = 2,
                                         blockRow = 0, blockCol = 6 }
newBlock SquareBlock = Block { blockBitMap = [[True, True],
                                              [True, True]],
                                              blockWidth = 2, blockHeight = 2,
                                              blockRow = 0, blockCol = 7 }
newBlock TBlock = Block { blockBitMap = [[True,  True,  True ],
                                         [False, True,  False],
                                         [False, True,  False]],
                                         blockWidth = 3, blockHeight = 3,
                                         blockRow = 0, blockCol = 6 }
newBlock BarBlock = Block { blockBitMap = [[True, True, True, True]],
                                         blockWidth = 4, blockHeight = 1,
                                         blockRow = 0, blockCol = 5 }
newBlock SBlock = Block { blockBitMap = [[True, True, False ],
                                          [False, True, True]],
                                         blockWidth = 3, blockHeight = 2,
                                         blockRow = 0, blockCol = 5 }

blockDown :: Block -> Block
blockDown block = block{ blockRow=(blockRow block) + 1 }

blockUp :: Block -> Block
blockUp block = block { blockRow=(blockRow block) - 1 }

blockLeft :: Block -> Block
blockLeft block = block { blockCol=(blockCol block) - 1 }

blockRight :: Block -> Block
blockRight block = block { blockCol=(blockCol block) + 1 }

blockRotate :: Block -> Block
blockRotate block = block { blockBitMap=rotateblockBitMap $ blockBitMap block,
                            blockWidth=blockHeight block,
                            blockHeight=blockWidth block }
                    where   rotateblockBitMap :: [[Bool]] -> [[Bool]]
                            rotateblockBitMap blockBitMap = map reverse $ transpose blockBitMap

-- expand block bitmap to playfield size indicated
-- offset to blockRow and blockCol
expandBlockBitmap :: Block -> Int -> Int -> [[Bool]]
expandBlockBitmap block fieldHeight fieldWidth = take numRowsBeforeBlock blankRows
    ++ expandRows bitMap
    ++ take numRowsAfterBlock blankRows
    where bitMap = blockBitMap block
          offset = blockCol block
          numRowsBeforeBlock = (blockRow block)
          numRowsAfterBlock = fieldHeight - (numRowsBeforeBlock + (blockHeight block))
          expandRow row = (take (offset) $ repeat False) ++
            row ++
            (take (fieldWidth-offset-(length row)) $ repeat False)
          expandRows rows =  map expandRow rows
          blankRows = repeat (take (max fieldWidth ((blockCol block)+(blockWidth block))) $ repeat False)


newGame :: StdGen -> GameState
newGame rndGen = GameState { playFieldRows = 30, playFieldCols = 15,
                      currentBlock = newBlock LBlock, randomGen = rndGen,
                      cellsByRow = take 30 $ repeat $ take 15 $ repeat False,
                      cellsByCol = take 15 $ repeat $ take 30 $ repeat False}

-- external entry point for moving the game to
-- a new state
applyAction :: GameState -> GameInput -> GameState
applyAction game action
            | action == MoveUp = applyAction' blockUp
            | action == MoveLeft = applyAction' blockLeft
            | action == MoveRight = applyAction' blockRight
            | action == MoveRight = applyAction' blockRight
            | action == Rotate = applyAction' blockRotate
            | action == MoveDown = applyAction' blockDown
            where applyAction' :: (Block -> Block) -> GameState
                  applyAction' blockTransform = let oldBlock = currentBlock game
                                                    newBlock = blockTransform oldBlock
                                                in case (blockWouldBeValid newBlock game) of
                                                        False | action == MoveDown -> killBlock game
                                                              | otherwise -> game
                                                        True  -> game { currentBlock=newBlock }


-- if currentBlock hits the bottom or another dead block,
-- it becomes part of new GameState's cells arrays, new block is spawned.
-- also calls full row detection function removeFullRows
killBlock :: GameState -> GameState
killBlock oldGame = let fieldHeight = playFieldRows oldGame
                        fieldWidth = playFieldCols oldGame
                        oldBlock = currentBlock oldGame
                        expandedBitmap = (expandBlockBitmap oldBlock fieldHeight fieldWidth)
                        gameRows = cellsByRow oldGame
                        spawnedPair = spawnNewBlock oldGame
                        spawnedBlock = snd(spawnedPair)
                        nextRandomGen = fst(spawnedPair)
                        in oldGame{ cellsByRow = removeFullRows $ zipWith (zipWith (||)) expandedBitmap gameRows,
                                    randomGen = nextRandomGen, currentBlock = spawnedBlock}


--detects and removes full rows from a bitmap
--adding equal number of empty rows at the top
removeFullRows :: [[Bool]] -> [[Bool]]
removeFullRows rows = (take (length (elemIndices fullRow rows)) blankRows) ++
                      (filter (/= fullRow) rows)
                      where blankRows = repeat $ take (length $ head rows) $ repeat False
                            fullRow = take (length $ head rows) $ repeat True


-- determines if a given block wold
-- be in a valid location (not overlapping
-- other blocks or past the edges of the
-- field) for a give GameState
blockWouldBeValid :: Block -> GameState -> Bool
blockWouldBeValid block game = row <= (rows-height) && col <= (cols-width) && row >= 0 && col >= 0 && --not hitting borders
              not ((foldr1 (||) (concat (zipWith (zipWith (&&)) expandedBitMap gameRows)))) --nothing overlaps with existing dead blocks
              where row = blockRow block
                    col = blockCol block
                    height = blockHeight block
                    width = blockWidth block
                    rows = playFieldRows game
                    cols = playFieldCols game
                    fieldWidth = playFieldCols game
                    fieldHeight = playFieldRows game
                    expandedBitMap = expandBlockBitmap block fieldHeight fieldWidth
                    gameRows = cellsByRow game


