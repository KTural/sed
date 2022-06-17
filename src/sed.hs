{-# LANGUAGE FlexibleContexts #-}

import System.Environment ( getArgs )
import Control.Monad ( when )
import System.IO ()
import System.Exit ( exitFailure, exitSuccess )
import System.Directory ( doesFileExist )
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust, isNothing)
import Data.Char (isDigit, isSpace)
import Text.Regex (subRegex, mkRegex)
import Text.Regex.TDFA
    ( (=~),
      AllMatches(getAllMatches),
      RegexContext,
      RegexMaker,
      CompOption,
      ExecOption,
      Regex
    )

main :: IO ()
main = do
    args <- getArgs
    when (checkHelpOption args) $ do
        printHelp
        exitSuccess
    when (checkSilentOption args) $ do
        exitSuccess
    case args of
        [pattern, fileName] -> do
            fileExists <- doesFileExist fileName
            if fileExists then do
                process pattern fileName
                exitSuccess
            else do
                putStrLn "\nFile doesn't exist!\n"
                exitFailure
        [switch1, pattern1, switch2, pattern2, fileName] -> do
            fileExists <- doesFileExist fileName
            if fileExists && switch1 == "-e" && switch2 == "-e" then do
                processDoublePatterns pattern1 pattern2 fileName
                exitSuccess
            else do
                putStrLn "\nFile doesn't exist! or Invalid command-line option\n"
                exitFailure 
        [switch, scriptFile, fileName] -> do 
            fileExists <- doesFileExist fileName 
            scriptExists <- doesFileExist scriptFile
            if fileExists && scriptExists && switch == "-f" then do 
                loadScript scriptFile fileName 
                exitSuccess 
            else do 
                exitFailure
        _ -> printErrorMessage

printErrorMessage :: IO ()
printErrorMessage = do
    putStrLn "\nRun runhaskell sed.hs -h or runhaskell sed.hs --help to see program usage\n"
    exitFailure

printHelp :: IO ()
printHelp = do
    putStrLn "USAGE: runhaskell sed.hs OPTIONS... [SCRIPT] [INPUTFILE...]\n\n"
    putStrLn "Basic functionalities: \n"
    putStrLn "      Replacing or substituting string: e.g."
    putStrLn "              runhaskell sed.hs 's/unix/linux/' input.txt\n"
    putStrLn "      Replacing the nth occurrence of a pattern in a line: e.g."
    putStrLn "              runhaskell sed.hs 's/unix/linux/2' input.txt\n"
    putStrLn "      Replacing all the occurrences of the pattern in a line: e.g."
    putStrLn "              runhaskell sed.hs 's/unix/linux/g' input.txt\n"
    putStrLn "      Replacing string on a specific line number: e.g."
    putStrLn "              runhaskell sed.hs '3 s/unix/linux/' input.txt\n"
    putStrLn "      Replacing string on a range of lines: e.g."
    putStrLn "              runhaskell sed.hs '1,3 s/unix/linux/' input.txt\n"
    putStrLn "      Replacing the nth occurence of a pattern on a range of lines: e.g."
    putStrLn "              runhaskell sed.hs '1,3 s/unix/linux/2' input.txt\n"
    putStrLn "      Replacing all the occurrences of the pattern on a range of lines: e.g."
    putStrLn "              runhaskell sed.hs '1,3 s/unix/linux/g' input.txt\n"
    putStrLn "      Adding double pattern scripts to be run while processing the input : e.g."
    putStrLn "              runhaskell sed.hs -e '1,3 s/unix/linux/g' -e '1,3 s/linux/unix/g' input.txt\n"
    putStrLn "      Loading double pattern scripts from the file to be run while processing the input : e.g."
    putStrLn "              runhaskell sed.hs -f input.sed input.txt\n\n"    
    putStrLn "Basic command-line options: \n"
    putStrLn "      -n; --quiet; --silent \n"
    putStrLn "      -e script"
    putStrLn "              add the commands in script to the set of commands \ 
                            \to be run while processing the input\n"
    putStrLn "      -f script-file"
    putStrLn "              add the commands contained in the file script-file \ 
                            \to the set of commands to be run while processing the input\n"
    putStrLn "      -h ; --help\n"

printEveryLine :: [String] -> IO ()
printEveryLine = mapM_ putStrLn

printResult :: Foldable t => t String -> IO ()
printResult output = do
    mapM_ (\line -> if null line then do
                        putStrLn "\nInvalid pattern!\n"
                    else do
                        putStrLn line) output

getEachLine :: FilePath -> IO [String]
getEachLine fileName = do
    contents <- readFile fileName
    let linesOfFiles = lines contents
    return linesOfFiles

parser :: String -> [String] -> IO [String]
parser pattern lines = do
    let x = splitOn "/" pattern
        maybeNum = readMaybe $ last x :: Maybe Int
        digits = [filter isDigit x | x <- splitOn "," $ head x]
        rangeNums = map (\x -> readMaybe x :: Maybe Int) digits
        firstNum = fromJust $ head rangeNums
        (firstPattern, replacement) = getPattern x
        lastPattern = last x
    when (firstPattern == "" ) $ do
        putStrLn "\n No regular expression is given\n"
        exitFailure
    when (isJust (head rangeNums) && firstNum == 0) $ do
        putStrLn "\nInvalid usage of line address 0\n"
        exitFailure
    let result = processHelper x maybeNum rangeNums firstNum
                            lines firstPattern replacement lastPattern
    return result

startProcess :: String -> String -> IO [String]
startProcess pattern fileName = do
    let linesOfFiles = getEachLine fileName
    lines <- linesOfFiles
    parser pattern lines

process :: String -> String -> IO ()
process pattern fileName = do
    result <- startProcess pattern fileName
    printResult result

processDoublePatterns :: String -> String -> String -> IO ()
processDoublePatterns pattern1 pattern2 fileName = do
    result <- startProcess pattern1 fileName 
    lastResult <- parser pattern2 result
    printResult lastResult

processHelper :: [String] -> Maybe Int -> [Maybe Int] -> Int -> [String] ->
                String -> String -> String -> [String]
processHelper x maybeNum rangeNums firstNum lines firstPattern replacement lastPattern
            | last x == "" && isNothing (head rangeNums) =
                map (\line -> replaceString line firstPattern replacement) lines
            | isJust maybeNum && not (all isJust rangeNums) =
                map (\line -> replaceNthOcc line firstPattern replacement
                            (fromJust maybeNum)) lines
            | last x == "g" && not (all isJust rangeNums) =
                map (\line -> replaceAllOcc line firstPattern replacement) lines
            | isJust (head rangeNums) && length rangeNums == 1 =
                replaceStringLineNum firstNum firstPattern replacement lastPattern lines
            | checkRangeNums rangeNums =
                let n1 = firstNum
                    n2 = fromJust $ last rangeNums
                    (newN1, newN2) = (min n1 n2, max n1 n2)
                in
                    if newN1 > length lines && newN2 /= 0 then
                        if (checkInvalidNums x == 2 && newN1 > 0) ||
                            (checkInvalidNum x == 1 ) then
                                lines
                        else []
                    else
                        replaceStringRangeLines newN1 newN2 firstPattern
                                                replacement lastPattern lines
            | otherwise = []

loadScript :: FilePath -> String -> IO ()
loadScript scriptFile fileName = do 
    line <- getEachLine scriptFile 
    if length line /= 2 then do 
        putStrLn "\nInvalid number of scripts\n"
        exitFailure
    else do 
        result <- startProcess (head line) fileName 
        lastRes <- parser (last line) result
        printResult lastRes

checkSilentOption :: Foldable t => t String -> Bool
checkSilentOption args = "-n" `elem` args || "--quiet" `elem` args || "--silent" `elem` args

checkHelpOption :: Foldable t => t String -> Bool
checkHelpOption args = "-h" `elem` args || "--help" `elem` args

checkRangeNums :: Foldable t => t (Maybe a) -> Bool
checkRangeNums rangeNums = all isJust rangeNums && length rangeNums == 2

getPattern :: [b] -> (b, b)
getPattern pattern =
    let (x, y) = (pattern !! 1, pattern !! 2)
    in (x, y)

replaceString :: (RegexMaker Regex CompOption ExecOption source,
                        RegexContext Regex source1 (String, String, String)) =>
                        source1 -> source -> String -> String
replaceString line search replacement =
    let (x, y, z) = line =~ search :: (String, String, String)
    in if y == "" then x ++ z else x ++ replacement ++ z

replaceNthOcc :: RegexMaker Regex CompOption ExecOption source =>
                       String -> source -> String -> Int -> String
replaceNthOcc line search replacement n =
    let x = getAllMatches (line =~ search) :: [(Int, Int)]
        numMatches = length x
    in if numMatches < n then line
       else let untillSearch = take (fst (x !! (n - 1))) line
                lengthSearch = uncurry (+) (x !! (n - 1))
                afterSearch = drop lengthSearch line
            in (untillSearch ++ replacement ++ afterSearch)

replaceAllOcc :: String -> String -> String -> String
replaceAllOcc line search replacement =
    let pattern = mkRegex search
    in subRegex pattern line replacement

checkNum :: String -> Integer
checkNum x = if isJust (readMaybe x :: Maybe Int) then 1 else 0

checkInvalidNum :: [String] -> Integer
checkInvalidNum k =
    sum (map checkNum (splitOn " " (head k)))

checkInvalidNums :: [String] -> Integer
checkInvalidNums k =
    sum (map checkNum (splitOn " " $ unwords $ splitOn "," (head k)))

replaceStringLineNum :: Int -> String -> String -> String -> [String] -> [String]
replaceStringLineNum n = replaceStringRangeLines n n

replaceStringRangeLinesHelper :: [String] -> Maybe Int -> p -> String -> String ->
                                String -> [String]
replaceStringRangeLinesHelper targetRange lastNum linesOfFiles
                            firstPattern replacement lastPattern =
    if isJust lastNum then
        map (\line -> replaceNthOcc line firstPattern replacement (fromJust lastNum)) targetRange
    else
        case lastPattern of
            "g" -> map (\line -> replaceAllOcc line firstPattern replacement) targetRange
            _ -> map (\line-> replaceString line firstPattern replacement) targetRange

replaceStringRangeLines :: Int -> Int -> String -> String -> String -> [String] -> [String]
replaceStringRangeLines newN1 newN2 firstPattern replacement lastPattern linesOfFiles =
    let (beforeRange, rest) = splitAt (newN1 - 1) linesOfFiles
        (targetRange, afterRange) = splitAt (newN2 - newN1 + 1) rest
        lastNum = readMaybe lastPattern :: Maybe Int
    in (beforeRange ++
        replaceStringRangeLinesHelper targetRange lastNum linesOfFiles
        firstPattern replacement lastPattern ++
        afterRange)