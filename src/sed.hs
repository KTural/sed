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
    case args of
        [pattern, fileName] -> do
            fileExists <- doesFileExist fileName
            if fileExists then do
                process pattern fileName
                exitSuccess
            else do
                printFileError
        [switch1, pattern1, switch2, pattern2, fileName] -> do
            fileExists <- doesFileExist fileName
            if fileExists && switch1 == "-e" && switch2 == "-e" then do
                processDoublePatterns pattern1 pattern2 fileName
                exitSuccess
            else do
                printFileOrInvalidError
        [switch, scriptFile, fileName] -> do
            fileExists <- doesFileExist fileName
            scriptExists <- doesFileExist scriptFile
            if fileExists && scriptExists && switch == "-f" then do
                loadScript scriptFile fileName
                exitSuccess
            else do
                printFileOrInvalidError
        _ -> printErrorMessage

printErrorMessage :: IO ()
printErrorMessage = do
    putStrLn "\nRun runhaskell sed.hs -h or runhaskell sed.hs --help to see program usage\n"
    exitFailure

printFileError :: IO ()
printFileError = do
    putStrLn "\nFile doesn't exist!\n"
    exitFailure

printFileOrInvalidError :: IO ()
printFileOrInvalidError = do
    putStrLn "\nFile doesn't exist! or Invalid command-line option\n"
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
                        putStrLn ""
                    else do
                        putStrLn line) output

getEachLine :: FilePath -> IO [String]
getEachLine fileName = do
    contents <- readFile fileName
    let linesOfFiles = lines contents
    return linesOfFiles

parser :: String -> [String] -> IO [String]
parser pattern lines = do
    let splittedPattern = splitOn "/" pattern
        [startLines, firstPattern, replacement, lastPattern] = splittedPattern
        maybeNum = readMaybe lastPattern :: Maybe Int
        digits = [filter isDigit x | x <- splitOn "," startLines]
        rangeNums = map (\x -> readMaybe x :: Maybe Int) digits
        firstNum = fromJust $ head rangeNums
    when (firstPattern == "" ) $ do
        putStrLn "\n No regular expression is given\n"
        exitFailure
    when (isJust (head rangeNums) && firstNum == 0) $ do
        putStrLn "\nInvalid usage of line address 0\n"
        exitFailure 
    if lastPattern == "" && isNothing (head rangeNums) then do
        return $ processReplaceString firstPattern replacement lines
    else if isJust maybeNum && not (all isJust rangeNums) then do 
        return $ processReplaceNthOcc firstPattern replacement maybeNum lines
    else if lastPattern == "g" && not (all isJust rangeNums) then do 
        return $ processReplaceAllOcc firstPattern replacement lines
    else if isJust (head rangeNums) && length rangeNums == 1 then do 
        return $ processReplaceStringLineNum firstNum firstPattern replacement lastPattern lines
    else if checkRangeNums rangeNums then do 
            let n1 = firstNum
                n2 = fromJust $ last rangeNums
                (newN1, newN2) = (min n1 n2, max n1 n2)
            if newN1 > length lines && newN2 /= 0 then do
                if (checkInvalidNums splittedPattern == 2 && newN1 > 0) ||
                    (checkInvalidNum splittedPattern == 1 ) then
                        return lines
                else return []
            else 
                return $ replaceStringRangeLines newN1 newN2 firstPattern
                                                 replacement lastPattern lines
    else return []
    

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

loadScript :: FilePath -> String -> IO ()
loadScript scriptFile fileName = do
    line <- getEachLine scriptFile
    case line of
        [pattern1, pattern2] -> processDoublePatterns pattern1 pattern2 fileName
        _ -> do
            putStrLn "\nInvalid number of scripts\n"
            exitFailure

processReplaceString :: (RegexMaker Regex CompOption ExecOption source,
                        RegexContext Regex source1 (String, String, String)) =>
                        source -> String -> [source1] -> [String]
processReplaceString firstPattern replacement =
    map (\line -> replaceString line firstPattern replacement)

processReplaceNthOcc :: RegexMaker Regex CompOption ExecOption source =>
                        source -> String -> Maybe Int -> [String] -> [String]
processReplaceNthOcc firstPattern replacement maybeNum =
    map (\line -> replaceNthOcc line firstPattern replacement (fromJust maybeNum))

processReplaceAllOcc :: String -> String -> [String] -> [String]
processReplaceAllOcc firstPattern replacement =
    map (\line -> replaceAllOcc line firstPattern replacement)

processReplaceStringLineNum :: Int -> String -> String -> String -> [String] -> [String]
processReplaceStringLineNum = replaceStringLineNum

processReplaceStringRangeLines :: Int -> Int -> String -> String -> String -> [String] -> [String]
processReplaceStringRangeLines = replaceStringRangeLines

checkHelpOption :: Foldable t => t String -> Bool
checkHelpOption args = "-h" `elem` args || "--help" `elem` args

checkRangeNums :: Foldable t => t (Maybe a) -> Bool
checkRangeNums rangeNums = all isJust rangeNums && length rangeNums == 2

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