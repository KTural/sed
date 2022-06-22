{-# LANGUAGE FlexibleContexts #-}

import System.Environment ( getArgs )
import Control.Monad ( when )
import System.IO ()
import System.Exit ( exitFailure, exitSuccess )
import System.Directory ( doesFileExist )
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust, isNothing, mapMaybe)
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
        ["-e", pattern1, "-e", pattern2, fileName] -> do
            fileExists <- doesFileExist fileName
            if fileExists then do
                processDoublePatterns pattern1 pattern2 fileName
                exitSuccess
            else do
                printFileOrInvalidError
        ["-f", scriptFile, fileName] -> do
            fileExists <- doesFileExist fileName
            scriptExists <- doesFileExist scriptFile
            if fileExists && scriptExists then do
                loadScript scriptFile fileName
                exitSuccess
            else do
                printFileOrInvalidError
        _ -> printErrorMessage

checkHelpOption :: Foldable t => t String -> Bool
checkHelpOption args = "-h" `elem` args || "--help" `elem` args

printHelp :: IO ()
printHelp = do
    putStrLn "USAGE: runhaskell sed.hs OPTIONS... [SCRIPT] [INPUTFILE...]\n"
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
    putStrLn "              runhaskell sed.hs -f input.sed input.txt\n"
    putStrLn "Basic command-line options: \n"
    putStrLn "      -e script"
    putStrLn "              add the commands in script to the set of commands \ 
                            \to be run while processing the input\n"
    putStrLn "      -f script-file"
    putStrLn "              add the commands contained in the file script-file \ 
                            \to the set of commands to be run while processing the input\n"
    putStrLn "      -h ; --help\n"

process :: String -> String -> IO ()
process pattern fileName = do
    result <- startProcess pattern fileName
    printResult result

printFileError :: IO ()
printFileError = do
    putStrLn "\nFile doesn't exist!\n"
    exitFailure

printFileOrInvalidError :: IO ()
printFileOrInvalidError = do
    putStrLn "\nFile doesn't exist! or Invalid command-line option\n"
    exitFailure

loadScript :: FilePath -> String -> IO ()
loadScript scriptFile fileName = do
    line <- getEachLine scriptFile
    case line of
        [pattern1, pattern2] -> processDoublePatterns pattern1 pattern2 fileName
        _ -> do
            putStrLn "\nInvalid number of scripts\n"
            exitFailure

processDoublePatterns :: String -> String -> String -> IO ()
processDoublePatterns pattern1 pattern2 fileName = do
    result <- startProcess pattern1 fileName
    lastResult <- parser pattern2 result
    printResult lastResult

printErrorMessage :: IO ()
printErrorMessage = do
    putStrLn "\nRun runhaskell sed.hs -h or runhaskell sed.hs --help to see program usage\n"
    exitFailure

startProcess :: String -> String -> IO [String]
startProcess pattern fileName = do
    let linesOfFiles = getEachLine fileName
    lines <- linesOfFiles
    parser pattern lines

parser :: String -> [String] -> IO [String]
parser pattern lines = do
    case splitOn "/" pattern of 
        [_, _, _, _] -> return []
        _ -> error "\nInvalid pattern\n"
    let [startLines, firstPattern, replacement, flagOrOcc] = splitOn "/" pattern
        maybeOcc = determineFlagOrOcc flagOrOcc
        digits = [filter isDigit x | x <- splitOn "," startLines]
        lineRange = mapMaybe readMaybe digits
    when (firstPattern == "" ) $ do
        putStrLn "\n No regular expression is given\n"
        exitFailure
    when (not (null lineRange) && head lineRange == 0) $ do
        putStrLn "\nInvalid usage of line address 0\n"
        exitFailure
    case lineRange of
        [] 
           | maybeOcc > 0 ->
                return $ processReplaceNthOcc firstPattern replacement maybeOcc lines
           | maybeOcc == 0 ->
                return $ processReplaceAllOcc firstPattern replacement lines
        [n1] ->
            return $ replaceStringLineNum n1 firstPattern replacement maybeOcc lines
        [n1, n2] -> do
            let (newN1, newN2) = (min n1 n2, max n1 n2)
            return $ replaceStringRangeLines newN1 newN2 firstPattern replacement maybeOcc lines
        _ ->
            return []

printResult :: Foldable t => t String -> IO ()
printResult output = do
    mapM_ putStrLn output

getEachLine :: FilePath -> IO [String]
getEachLine fileName = do
    contents <- readFile fileName
    let linesOfFiles = lines contents
    return linesOfFiles

determineFlagOrOcc :: String -> Int
determineFlagOrOcc flagOrOcc
    | flagOrOcc == "g" = 0
    | flagOrOcc == "" = 1
    | Just occ <- readMaybe flagOrOcc = occ
    | otherwise = error "\nOccurence number or flag is invalid!\n"

processReplaceNthOcc :: RegexMaker Regex CompOption ExecOption source =>
                        source -> String -> Int -> [String] -> [String]
processReplaceNthOcc firstPattern replacement occ =
    map (replaceNthOcc firstPattern replacement occ)

processReplaceAllOcc :: String -> String -> [String] -> [String]
processReplaceAllOcc firstPattern replacement =
    map (replaceAllOcc firstPattern replacement)

replaceStringLineNum :: Int -> String -> String -> Int -> [String] -> [String]
replaceStringLineNum n = replaceStringRangeLines n n

replaceStringRangeLines :: Int -> Int -> String -> String -> Int -> [String] -> [String]
replaceStringRangeLines n1 n2 firstPattern replacement lastNum linesOfFiles =
    let (beforeRange, rest) = splitAt (n1 - 1) linesOfFiles
        (targetRange, afterRange) = splitAt (n2 - n1 + 1) rest
    in (beforeRange ++
        replaceStringRangeLinesHelper targetRange lastNum firstPattern replacement ++ afterRange)

replaceStringRangeLinesHelper :: [String] -> Int -> String -> String -> [String]
replaceStringRangeLinesHelper targetRange lastNum firstPattern replacement =
    case lastNum of
        0 -> map (replaceAllOcc firstPattern replacement) targetRange
        _ -> map (replaceNthOcc firstPattern replacement lastNum) targetRange

replaceAllOcc :: String -> String -> String -> String
replaceAllOcc search replacement line =
    let pattern = mkRegex search
    in subRegex pattern line replacement

replaceNthOcc :: RegexMaker Regex CompOption ExecOption source =>
                    source -> String -> Int -> String -> String
replaceNthOcc search replacement n line =
    let x = getAllMatches (line =~ search) :: [(Int, Int)]
        numMatches = length x
    in if numMatches < n then line
       else let (from, to) = x !! (n - 1)
                untillSearch = take from line
                lengthSearch = from + to
                afterSearch = drop lengthSearch line
            in (untillSearch ++ replacement ++ afterSearch)