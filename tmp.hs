module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import UI.NCurses
import Data.List (intercalate)
import Data.Map.Strict (fromList, lookup, Map)

import Types


block1 = MBlock {
  mb_width = 3,
  mb_height = 2,
  mb_top = Point (-2.0) 1.0,
  mb_speed = 0.05
}

block2 = MBlock {
  mb_width = 6,
  mb_height = 1,
  mb_top = Point (-2.0) 5.0,
  mb_speed = 0.1
}

block3 = MBlock {
  mb_width = 3,
  mb_height = 5,
  mb_top = Point (-2.0) 7.0,
  mb_speed = 0.1
}

gField = GameField 20 20 [block1, block2, block3]

recalcGf (GameField w h items) = let
  newItems = recalcBlocks items
  in GameField w h newItems

tst = vFieldToString . gFieldToVField
--tstVF = VField 40 40 (createEmptyGridWithBorders 40 40)

main :: IO ()
main = runCurses $ main1 gField 0

--main1 :: GameField -> Curses ()
main1 oldGf x = do
    --setEcho False
    setCursorMode CursorVeryVisible
    w <- defaultWindow
    --let newGf = recalcGf oldGf
  
    --let vf = gFieldToVField oldGf
    --let mChar = Data.Map.Strict.lookup (1,3) $ vf_grid vf
    --let char = case mChar of
    --              (Just d) -> d
    --              Nothing -> 'E'

    let lines = vFieldToLines $ gFieldToVField oldGf

    updateWindow w $ do
      moveCursor 0 0
      drawString $ intercalate "\n" lines
    render

    let newGf = recalcGf oldGf

    ev <- getEvent w (Just 10)
    case ev of
      Nothing -> main1 newGf x
      Just ev' -> main1 newGf x
        


--tstStr = tst $ recalcGf gField

--tstRend :: String -> Curses ()
--tstRend str = do
--    setEcho False
--    w <- defaultWindow

--    --render
--    updateWindow w $ do
--        moveCursor 0 0
--        drawString str
--    render
    
--    ev <- getEvent w Nothing
--    case ev of
--      Nothing -> tstRend str
--      Just ev' -> tstRend (tst $ gField)


--main1 :: IO ()
--main = runCurses $ main1 1

--main1 x = do
--    setEcho False
--    w <- defaultWindow
--    updateWindow w $ do
--        moveCursor 1 10
--        drawString $ show x
--    render
--    --x <- threadDelay 1000000
--    --main1 $ x + 1
--    --return ()
--    ev <- getEvent w (Just 100)
--    case ev of
--      Nothing -> main1 $ x + 1
--      Just ev' -> return ()
      

    --x <- threadDelay 1000000
    --Curses ()
    --return $ do
    --  threadDelay 1000000
    --  putStrLn "aaa"
    --waitFor w (\ev -> True)
    --waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')



--cursTst = do
--    setEcho False
--    w <- defaultWindow
--    updateWindow w $ do
--        moveCursor 1 10
--        drawString "Hello world!"
--        moveCursor 3 10
--        drawString "(press q to quit)"
--        moveCursor 0 0
--    render
    --waitFor w (\ev -> True)



--tst1 = do
--  main1
--  timer1 <- newTimer (2000 * 1000)
--  waitTimer timer1

--waitFor :: Window -> (Event -> Bool) -> Curses ()
--waitFor w p = loop where
--    loop = do
--        ev <- getEvent w Nothing
--        case ev of
--            Nothing -> loop
--            Just ev' -> if p ev' then return () else loop


--main = do
--    timer1 <- newTimer (200 * 1000)
--    waitTimer timer1
--    putStrLn "Timer 1 expired"
--    main
--    timer2 <- newTimer (1 * 1000000)
--    forkIO $ do
--        waitTimer timer2
--        putStrLn "Timer 2 expired"
--    stopTimer timer2
--    putStrLn "Timer 2 stopped"

----tst :: State Integer Integer
----tst = do
----  x <- get
----  wait1000
----  put $ x + 1
----  return 0

--wait1000 = do 
--  timer1 <- newTimer (1000 * 1000)
--  waitTimer timer1

--data TState = Start | Stop
--type Timer = (TVar TState, TMVar ())

--waitTimer :: Timer -> IO ()
--waitTimer (_, timer) = atomically $ readTMVar timer

--stopTimer :: Timer -> IO ()
--stopTimer (state, _) = atomically $ writeTVar state Stop

--newTimer :: Int -> IO Timer
--newTimer n = do
--    state <- atomically $ newTVar Start
--    timer <- atomically $ newEmptyTMVar
--    forkIO $ do
--        threadDelay n
--        atomically $ do
--            runState <- readTVar state
--            case runState of
--                Start -> putTMVar timer ()
--                Stop  -> return ()
--    return (state, timer)


--wait1 n = threadDelay n