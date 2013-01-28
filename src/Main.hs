-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main where

import TetradsModel
import TetradsView.ANSI
import System.IO
import System.Random
import Control.Concurrent

import System.Console.ANSI

main::IO()
main = do
       --init random number generator, init game state
       rndGen <- getStdGen
       let gameState = newGame rndGen

       -- init input and output
       hSetBuffering stdin NoBuffering
       initView gameState
       --updateView theGameState theGameState

       gameMVar <- newMVar gameState
       tid <- forkIO (sendPeriodicMoveDown gameMVar)

       -- start the game
       inputLoop gameMVar

       -- clean up
       cleanUpView

sendPeriodicMoveDown :: MVar GameState -> IO()
sendPeriodicMoveDown gameMVar = do
        threadDelay 500000
        gameState <- takeMVar gameMVar
        let newState = applyAction gameState MoveDown
        updateView gameState newState
        putMVar gameMVar newState
        sendPeriodicMoveDown gameMVar

inputLoop :: MVar GameState -> IO()
inputLoop gameMVar = do

        c <- getChar
        gameState <- takeMVar gameMVar -- must come after getChar, since getChar blocks waiting on input

        case c of
                'j' -> do let newState=applyAction gameState MoveLeft
                          updateView gameState newState
                          putMVar gameMVar newState
                          inputLoop gameMVar
                'l' -> do let newState=applyAction gameState MoveRight
                          updateView gameState newState
                          putMVar gameMVar newState
                          inputLoop gameMVar
                'i' -> do let newState=applyAction gameState MoveUp
                          updateView gameState newState
                          putMVar gameMVar newState
                          inputLoop gameMVar
                'k' -> do let newState=applyAction gameState MoveDown
                          updateView gameState newState
                          putMVar gameMVar newState
                          inputLoop gameMVar
                ' ' -> do let newState=applyAction gameState Rotate
                          updateView gameState newState
                          putMVar gameMVar newState
                          inputLoop gameMVar
                'q' -> do putMVar gameMVar gameState
                          return()
                _ -> do putMVar gameMVar gameState
                        inputLoop gameMVar
