{-# LANGUAGE OverloadedStrings #-}
{- Тестовое задание "Этажи"
сценарий: 
поиск по новостройкам, поиск по однокомнатным квартирам, проверка что квартиры от застройщиков однокомнатные, 
проверка что квартиры от инвесторов однокомнатные.
Я не стал прикручивать пакет, который отвечает за логирование. Это заняло бы больше времени. 
Потому все сообщения об ошибках или сообщения об успешном прохождении проверки, пишутся в консоль.
Из-за этого, выполнение автотеста занимает до 30 секунд. Если выключить запись в консоль, автотест завершится за 5
секунд, если не будет тормозить интернет\сайт -}

module Main where

-- Импорнтируем необходимые модули
import Prelude as P
import Test.WebDriver
import Test.WebDriver.Commands.Wait (waitUntil)
import Data.Text as T
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Concurrent
import Control.Monad

-- Метод для создания скриншотов
screenshotWriteFile :: FilePath -> WD ()
screenshotWriteFile name = do
                                string <- screenshot                -- имя скриншота
                                -- с помощью монады IO сохраним скриншот по указанному имени
                                liftIO . B.writeFile name $ string   

-- Браузеры, которые будем использовать
capsChrome = defaultCaps { browser = chrome } -- "прогоним" атотест в хроме
capsFF = defaultCaps                          -- и в браузере по умолчанию. У меня ФФ.

-- Адрес сайта
etagi :: WD ()
etagi = openPage "https://www.etagi.com/"

-- метод, который будет брать из фразы типа "6307 объектов" только цифры и приводить к типу Int
getNumber :: Text -> Int
getNumber text = read (P.filter (\x -> x `elem` ['0'..'9']) (unpack text)) :: Int

-- Общий метод проверки для всех трех случаев. Если первый параметр меньше либо равен нулю, значит произошла ошибка.
-- В случае ошибки, снимем скриншот с именем filenameFail и выведем в лог сообщение о падении messageFail
-- Если number  больше нуля, сообщим в лог об удачном прохожении проверки сообщением message
countCheckStep :: (Num a, Ord a) => a -> String -> FilePath -> String -> WD ()
countCheckStep number messageFail filenameFail message  | number <= 0 = do 
                                                                            screenshotWriteFile filenameFail 
                                                                            fail messageFail
                                                        | otherwise = liftIO $ putStrLn message

-- Метод проверки, что в таблице в каждом третем столбце написано 1
-- первые 4 строки вырезаем так как это шапка таблицы 
check1 :: Text -> Bool
check1 =  P.all ( ("1"==) . (!! 2)  . T.words) . P.drop 4 . T.lines  

-- Проверка очередной таблицы на наличие что-либо отличного от "1" в третей колонке
-- Если это так, то снимем скриншот страницы, выведем сообщение об ошике, прекратим автотест
-- Если проверка очередной таблицы прошла успешно, выведем сообщение в лог
checkOneTable :: Text -> WD ()
checkOneTable tableElement 
    | check1 tableElement = liftIO $ putStrLn ("Таблица " ++ (unpack tableElement) ++ " проверена успешно")
    | otherwise = do 
                          screenshotWriteFile "1RoomFail.png"
                          fail "В таблице застройщиков не однокомнатная квартира" 

-- Метод проверки всех найденных на странице таблиц на нахождение в третем столбце цифры 1                                                          
checkFoundTables :: [Element] -> WD ()
checkFoundTables = mapM_ (\tableForCheck -> do 
                                    tableForCheckText <- getText tableForCheck
                                    checkOneTable tableForCheckText)
                                    
-- Метод проверки текста. Если в передаваемом тексте первое слово "Однокомнатная", то проверка прошла успешно
-- TODO по-хорошему, текст надо приводить к utf-8 и оставить одну проверку, но это займет больше времени, чем я ожидал
checkOne :: Text -> Bool
checkOne inputWord
    | P.head (T.words inputWord) == "\1054\1076\1085\1086\1082\1086\1084\1085\1072\1090\1085\1072\1103" ||
      P.head (T.words inputWord) == "Однокомнатная" = True
    | otherwise = False
                                    
-- Метод проверки что очередная квартира в списке имеет в своем названии слово "Однокомнатная"
-- Если это не так, сделаем скриншот, выведем сообщение об ошибке, завершим автотест
checkOneRoomWord :: Text -> WD ()
checkOneRoomWord inputWord 
    | checkOne inputWord = liftIO $ putStrLn ("Квартира " ++ (unpack inputWord) ++ " действительно однокомнатная")
    | otherwise = do 
                          screenshotWriteFile "OneRoomFail.png"
                          fail "В списке инвесторов не однокомнатная квартира" 
    
-- Метод проверки списка однокомнатных квартир
checkOneRoomObjects :: [Element] -> WD ()
checkOneRoomObjects = mapM_ (\objectsForCheck -> do 
                                    objectsForCheckText <- getText objectsForCheck
                                    checkOneRoomWord objectsForCheckText)
-- Основной сценарий           
etagiScenario = do
    searchCount <- findElem $ ByClass "search_count" -- Ищем количество квартир до того, как мы нажали кнопку
    searchCountText <- getText searchCount           -- "Новостройки"
    -- Проверяем, что это количество не меньше 1
    countCheckStep (getNumber searchCountText) "Количество объектов меньше, либо равно нулю!" 
                    "failFirstStep.png" "Прошли первую проверку"
    newBuild <- findElem $ ByLinkText "НОВОСТРОЙКИ"
    click newBuild                                  -- нажимаем на вкладку "НОВОСТРОЙКИ"
    -- так как тормозит сайт(или интернет у меня), и часто автотест падает именно здесь, установим таймаут
    -- ждем не более 20 секунд. Ждем пока появится кнопка "По Карте города"
    waitUntil 20 (findElem $ ById "search_on_map2") 
    searchCount <- findElem $ ByClass "search_count" -- из-за того, что в хаскеле каждой переменной дается свой 
                                                     -- идентификатор, то мы не можем переиспользовать такую же
                                                     -- переменную, которая была объявлена выше
                                                     -- потому необходимо написать еще раз
    newBuildSearchCountText <- getText searchCount   -- запоминаем количество квартир
    -- проверяем, что количество новостроек больше 1
    countCheckStep (getNumber newBuildSearchCountText) "Количество новостроек меньше, либо равно нулю" 
                    "failSecondStep.png" "Прошли вторую проверку"
    oneRoomFilter <- findElem $ ByClass "room_check" -- кнопка "1", которая вызывае js скрипт и вызывает пересчет
                                                     -- квартир у элемента class="search_count"
    click oneRoomFilter                              -- нажимаем эту кнопку
    liftIO $ threadDelay 1000000                     -- так как в пакете webdriver нет методов для работы с js
                                                     -- то придется использовать стандартные средства для приостановки 
                                                     -- работы программы, чтобы данные по кличеству квартир
                                                     -- успели пересчитаться
    searchCount <- findElem $ ByClass "search_count" -- перечитываем количество квартир
    newBuildSearchCountTextNewState <- getText searchCount
    -- проверяем, что количество квартир изменилось
    countCheckStep (getNumber newBuildSearchCountText -
                    getNumber newBuildSearchCountTextNewState) 
                    "Количество новостроек не изменилось после выбора однокомнатных квартир, либо их стало меньше!" 
                    "failThirdStep.png" "Прошли третью проверку"
    -- Жмем кнопку "Найти"
    searchButton <- findElem $ ByTag "BUTTON"
    click searchButton
    -- Со страницы собираем все таблицы, в которых есть информация по квартирам
    allTablesElements <- findElems $ ByClass "newhouses_flats"
    -- Проверяем, что все квартиры однокомнатные
    checkFoundTables allTablesElements
    -- Нажимаем на вкладу "новостройки от инвесторов"
    -- не нашел более хорошего способа, чем поиск этого элемента по XPath или по CSS. Решил что по
    -- CSS будет более наглядно в коде
    investorsBuildsSpan <- findElem $ ByCSS "#objects > div.js-tabs.tabs.line1 > a > span"
    click investorsBuildsSpan
    -- TODO придумать, как получать вплывающее окно. На данный момент, мы берем список всех окон, которые открыты
    openedWindows <- windows                    -- во время работы автотеста
    focusWindow $ P.last openedWindows          -- и берем идентификатор последнего
    investorsBuils <- findElems $ ByClass "title-obj" -- ищем надписи с указанием количество комнат квартир
    -- Проверяем что все квартиры однокомнатные
    checkOneRoomObjects investorsBuils
    liftIO $ putStrLn "Проверка однокомнатных квартир прошла успешно"
                            
-- Список автотестов, которые запустим в конкретном кейсе
testCase c = void $ runSession (defaultConfig { wdCapabilities = c }) (etagi >> etagiScenario)

-- список браузеров, тест-кейсов и браузерах, в которых тест-кейсы будут запущены
testSuits = mapM_ testCase  [capsFF] -- раскомментить, если хотим запустить автотесты в хроме --, capsChrome]                            
           
-- Вызов тестовых сценариев. Ничего больше 
main :: IO ()
main = do
  testSuits
  putStrLn "Автотест успешно завершен"