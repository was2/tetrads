-----------------------------------------------------------------------------
--
-- Module      :  TetradsView.ANSI
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

module TetradsView.ANSI where

import System.Console.ANSI
import System.IO
import TetradsModel

--default colors for game elements
bgColor = SetColor Background Dull Cyan
fgColor = SetColor Background Vivid Red --terminal background is used to create game foreground with ' ' characters
borderColor = SetColor Background Dull Green

-- ' '-character generator -- so... I found 'repeat' after I wrote this...
spaces :: String
spaces = ' ' : spaces

initView :: GameState -> IO()
initView game = do
        isTerm <- hIsTerminalDevice stdout
        if not isTerm
            then error "stdout is not a terminal device!"
            else do
                --prep terminal for full-screen use
                hSetBuffering stdout NoBuffering
                hSetEcho stdout False
                hideCursor

                -- clear w/bgColor
                setSGR [Reset]
                setSGR[bgColor]
                clearScreen

                -- draw border of game field
                let gameCols = playFieldCols game
                let gameRows = playFieldRows game
                setSGR[borderColor]
                setCursorPosition 0 0
                putStr $ take ((gameCols * 2) + 2) spaces -- 1 game column = 2 terminal spaces
                setCursorPosition (gameRows+1) 0
                putStr $ take ((gameCols * 2) + 2) spaces
                -- generate list of lists of actions to draw vertical borders
                -- concat to flatten list, then apply w/sequence_
                sequence_ $ concat [ [setCursorPosition x 0, putChar ' ',
                                      setCursorPosition x ((gameCols * 2) + 1), putChar ' '] | x<-[1..(gameRows)] ]

cleanUpView :: IO()
cleanUpView = do
              setSGR[Reset]
              showCursor
              setCursorPosition 0 0
              clearScreen

updateView :: GameState -> GameState -> IO()
updateView oldGame newGame = let oldGameCells = cellsByRow oldGame
                                 newGameCells = cellsByRow newGame
                                 oldBlock = currentBlock oldGame
                                 newBlock = currentBlock newGame
                             in do case (oldGameCells == newGameCells) of
                                        False                        -> do {redrawField newGame; drawBlock newBlock}
                                        True  | newBlock /= oldBlock -> do {eraseBlock oldBlock; drawBlock newBlock}
                                              | otherwise            -> return ()

drawBlock :: Block -> IO()
drawBlock block = do
            setSGR[fgColor]
            setCursorPosition (1 + (blockRow block)) (1 + ((blockCol block) * 2))
            sequence_ $ getBlockPrintActions $ block

eraseBlock :: Block -> IO()
eraseBlock block = do
            setSGR[bgColor]
            setCursorPosition (1 + (blockRow block)) (1 + ((blockCol block) * 2))
            sequence_ $ getBlockPrintActions block

redrawField :: GameState -> IO()
redrawField game = do
            setCursorPosition 1 1
            setSGR[bgColor]
            sequence_ $ getFieldPrintActions game


getBlockPrintActions :: Block -> [IO()]
getBlockPrintActions block = actionsFromBitMap $ blockBitMap block
                        where actionsFromBitMap []     = []
                              actionsFromBitMap (x:xs) = (map (\a -> if a then putStr "  " else cursorForward 2) x)
                                                          ++ [cursorBackward ((blockWidth block) *2), cursorDown 1]
                                                          ++ actionsFromBitMap xs
getFieldPrintActions :: GameState -> [IO()]
getFieldPrintActions game = actionsFromBitMap $ cellsByRow game
                        where actionsFromBitMap []     = []
                              actionsFromBitMap (x:xs) = (map (\a -> if a then do {setSGR[fgColor]; putStr "  "; setSGR[bgColor]}
                                                                     else putStr "  ")
                                                                     x)
                                                          ++ [cursorBackward ((playFieldCols game)*2), cursorDown 1]
                                                          ++ actionsFromBitMap xs


