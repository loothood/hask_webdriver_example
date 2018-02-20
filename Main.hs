{-# LANGUAGE OverloadedStrings #-}
module Main where

-- modules
import Prelude as P
import Test.WebDriver
import Test.WebDriver.Commands.Wait (waitUntil)
import Data.Text as T
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Concurrent
import Control.Monad

-- screenshots
screenshotWriteFile :: FilePath -> WD ()
screenshotWriteFile name = do
                                string <- screenshot                -- screenshot name
                                liftIO . B.writeFile name $ string   

-- list of the browsers
capsChrome = defaultCaps { browser = chrome } -- chrome
capsFF = defaultCaps                          -- firefox

-- the site
etagi :: WD ()
etagi = openPage "https://www.etagi.com/"

getNumber :: Text -> Int
getNumber text = read (P.filter (\x -> x `elem` ['0'..'9']) (unpack text)) :: Int

countCheckStep :: (Num a, Ord a) => a -> String -> FilePath -> String -> WD ()
countCheckStep number messageFail filenameFail message  | number <= 0 = do 
                                                                            screenshotWriteFile filenameFail 
                                                                            fail messageFail
                                                        | otherwise = liftIO $ putStrLn message

check1 :: Text -> Bool
check1 =  P.all ( ("1"==) . (!! 2)  . T.words) . P.drop 4 . T.lines  

checkOneTable :: Text -> WD ()
checkOneTable tableElement 
    | check1 tableElement = liftIO $ putStrLn ("The table " ++ (unpack tableElement) ++ " checked successfully")
    | otherwise = do 
                          screenshotWriteFile "1RoomFail.png"
                          fail "There is no studio apartments" 

checkFoundTables :: [Element] -> WD ()
checkFoundTables = mapM_ (\tableForCheck -> do 
                                    tableForCheckText <- getText tableForCheck
                                    checkOneTable tableForCheckText)
                                    
checkOne :: Text -> Bool
checkOne inputWord
    | P.head (T.words inputWord) == "\1054\1076\1085\1086\1082\1086\1084\1085\1072\1090\1085\1072\1103" ||
      P.head (T.words inputWord) == "Studio" = True
    | otherwise = False
                                    
checkOneRoomWord :: Text -> WD ()
checkOneRoomWord inputWord 
    | checkOne inputWord = liftIO $ putStrLn ("The flat " ++ (unpack inputWord) ++ " is studio")
    | otherwise = do 
                          screenshotWriteFile "OneRoomFail.png"
                          fail "There is not a studio flat" 
    
checkOneRoomObjects :: [Element] -> WD ()
checkOneRoomObjects = mapM_ (\objectsForCheck -> do 
                                    objectsForCheckText <- getText objectsForCheck
                                    checkOneRoomWord objectsForCheckText)
                                    
-- The main scenario
etagiScenario = do
    searchCount <- findElem $ ByClass "search_count" 
    searchCountText <- getText searchCount           
    
    countCheckStep (getNumber searchCountText) "There is on objects!" 
                    "failFirstStep.png" "First check passed"
    newBuild <- findElem $ ByLinkText "НОВОСТРОЙКИ"
    click newBuild                                
    -- wait for element present
    waitUntil 20 (findElem $ ById "search_on_map2") 
    searchCount <- findElem $ ByClass "search_count"
    newBuildSearchCountText <- getText searchCount
    countCheckStep (getNumber newBuildSearchCountText) "There is on objects!" 
                    "failSecondStep.png" "Secnd check passed"
    oneRoomFilter <- findElem $ ByClass "room_check"
    click oneRoomFilter                              
    liftIO $ threadDelay 1000000                     
    searchCount <- findElem $ ByClass "search_count" 
    newBuildSearchCountTextNewState <- getText searchCount
    
    countCheckStep (getNumber newBuildSearchCountText -
                    getNumber newBuildSearchCountTextNewState) 
                    "There is no changes after changing the conditions!" 
                    "failThirdStep.png" "Third check passed"
    searchButton <- findElem $ ByTag "BUTTON"
    click searchButton
    allTablesElements <- findElems $ ByClass "newhouses_flats"
    checkFoundTables allTablesElements
    investorsBuildsSpan <- findElem $ ByCSS "#objects > div.js-tabs.tabs.line1 > a > span"
    click investorsBuildsSpan
    openedWindows <- windows                    
    focusWindow $ P.last openedWindows          
    investorsBuils <- findElems $ ByClass "title-obj"
    checkOneRoomObjects investorsBuils
    liftIO $ putStrLn "The studio checking finished successfully!"
                            
testCase c = void $ runSession (defaultConfig { wdCapabilities = c }) (etagi >> etagiScenario)


testSuits = mapM_ testCase  [capsFF] -- uncomment to run test in google chrome --, capsChrome]                            
           
-- main
main :: IO ()
main = do
  testSuits
  putStrLn "The test passed successffully"
