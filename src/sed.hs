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
    let numArgs = length args
    when (checkHelpOption args) $ do
        printHelp
        exitSuccess
    when (checkSilentOption args) $ do
        exitSuccess
    when (numArgs == 2) $ do
        let pattern = head args
            fileName = last args
        fileExists <- doesFileExist fileName
        if fileExists then do
            process pattern fileName
            exitSuccess
        else do
            putStrLn "\nFile doesn't exist!\n"
            exitFailure
    when (numArgs == 3) $ do
        -- (to be implemented)
        exitSuccess
    putStrLn "\nRun runhaskell sed.hs -h or runhaskell sed.hs --help to see program usage\n"

checkSilentOption :: Foldable t => t String -> Bool
checkSilentOption args = "-n" `elem` args || "--quiet" `elem` args || "--silent" `elem` args

checkHelpOption :: Foldable t => t String -> Bool
checkHelpOption args = "-h" `elem` args || "--help" `elem` args

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
    putStrLn "              runhaskell sed.hs '1,3 s/unix/linux/' input.txt\n\n"
    putStrLn "Basic command-line options: \n"
    putStrLn "      -n; --quiet; --silent \n"
    putStrLn "      -e script; --expression=script"
    putStrLn "              add the commands in script to the set of commands \ 
                            \to be run while processing the input\n"
    putStrLn "      -f script-file ; --file=script-file"
    putStrLn "              add the commands contained in the file script-file \ 
                            \to the set of commands to be run while processing the input\n"
    putStrLn "      -h ; --help\n"

getPattern :: [b] -> (b, b)
getPattern pattern = 
    let (x, y) = (pattern !! 1, pattern !! 2)
    in (x, y)

replaceStringHelper :: (RegexMaker Regex CompOption ExecOption source,
                        RegexContext Regex source1 (String, String, String)) =>
                        source1 -> source -> String -> IO ()
replaceStringHelper line search replacement =
    let (x, y, z) = line =~ search :: (String, String, String)
    in if y == "" then putStrLn (x ++ z) else putStrLn (x ++ replacement ++ z)

replaceString :: [String] -> FilePath -> IO ()
replaceString pattern fileName = do
    contents <- readFile fileName
    let linesOfFiles = lines contents
        (firstPattern, replacement) = getPattern pattern
    mapM_ (\line -> replaceStringHelper line firstPattern replacement) linesOfFiles

replaceNthOccHelper :: RegexMaker Regex CompOption ExecOption source =>
                       String -> source -> String -> Int -> IO ()
replaceNthOccHelper line search replacement n =
    let x = getAllMatches (line =~ search) :: [(Int, Int)]
    in if n == 0 then do
        putStrLn "\nOccurence number may not be zero\n"
        exitFailure
    else do
        let numMatches = length x
        if numMatches < n then
            putStrLn line
        else do
            let untillSearch = take (fst (x !! (n - 1))) line
                lengthSearch = uncurry (+) (x !! (n - 1))
                afterSearch = drop lengthSearch line
            putStrLn (untillSearch ++ replacement ++ afterSearch)

replaceNthOcc :: Int -> [String] -> FilePath -> IO ()
replaceNthOcc n pattern fileName = do
    contents <- readFile fileName
    let linesOfFiles = lines contents
        (firstPattern, replacement) = getPattern pattern
    mapM_ (\line -> replaceNthOccHelper line firstPattern replacement n) linesOfFiles

replaceAllOccHelper :: String -> String -> String -> IO ()
replaceAllOccHelper line search replacement = do
    let pattern = mkRegex search
    putStrLn $ subRegex pattern line replacement

replaceAllOcc :: [String] -> FilePath -> IO ()
replaceAllOcc pattern fileName = do
    contents <- readFile fileName
    let linesOfFiles = lines contents
        (firstPattern, replacement) = getPattern pattern
    if firstPattern == "" then do
        putStrLn "\n No regular expression is given\n"
        exitFailure
    else do
        mapM_ (\line -> replaceAllOccHelper line firstPattern replacement) linesOfFiles

printEveryLine :: [String] -> IO ()
printEveryLine = mapM_ putStrLn

checkNum :: String -> Integer
checkNum x = if isJust (readMaybe x :: Maybe Int) then 1 else 0

checkInvalidNum :: [String] -> Integer
checkInvalidNum k =
    sum (map checkNum (splitOn " " (head k)))

checkInvalidNums :: [String] -> Integer
checkInvalidNums k =
    sum (map checkNum (splitOn " " $ unwords $ splitOn "," (head k)))

replaceStringLineNum :: Int -> [String] -> FilePath -> IO ()
replaceStringLineNum n pattern fileName = do
    if n == 0 then do
        putStrLn "\nInvalid usage of line address 0\n"
        exitFailure
    else do
        contents <- readFile fileName
        let linesOfFiles = lines contents
            beforeNthLine = take (n - 1) linesOfFiles
            nthLine = linesOfFiles !! (n - 1)
            afterNthLine = drop n linesOfFiles
            (firstPattern, replacement) = getPattern pattern
        if n > length linesOfFiles then do
            if checkInvalidNum pattern == 1 then do
                printEveryLine linesOfFiles
            else do
                putStrLn "\nInvalid line number!\n"
                exitFailure
        else do
            mapM_ putStrLn beforeNthLine
            replaceStringHelper nthLine firstPattern replacement
            mapM_ putStrLn afterNthLine

replaceStringRangeLines :: Int -> Int -> [String] -> FilePath -> IO ()
replaceStringRangeLines n1 n2 pattern fileName = do
    if n1 == 0 then do
        putStrLn "\nInvalid usage of line address 0\n"
    else do
        contents <- readFile fileName
        let linesOfFiles = lines contents
            newN1 = if n1 > n2 then n2 else n1
            newN2 = if n1 > n2 then n1 else n2
            beforeRange = take (newN1 - 1) linesOfFiles
            targetRange = take newN2 $ drop (newN1 - 1) linesOfFiles
            afterRange = drop newN2 linesOfFiles
            (firstPattern, replacement) = getPattern pattern
        if newN1 > length linesOfFiles && newN2 /= 0 then do
            if checkInvalidNums pattern == 2 && newN1 > 0 then do
                printEveryLine linesOfFiles
            else do
                putStrLn "\nInvalid line number!\n"
                exitFailure
        else do
            mapM_ putStrLn beforeRange
            mapM_ (\line-> replaceStringHelper line firstPattern replacement) targetRange
            mapM_ putStrLn afterRange

process :: String -> String -> IO ()
process pattern fileName =
    let x = splitOn "/" pattern
        maybeNum = readMaybe $ last x :: Maybe Int
        digits = [[y | y <- x , isDigit y] | x <- splitOn "," $ head x]
        rangeNums = map (\x -> readMaybe x :: Maybe Int) digits
        firstNum = fromJust $ head rangeNums
    in check x maybeNum rangeNums firstNum
    where check x maybeNum rangeNums firstNum
            | last x == "" && isNothing (head rangeNums) =
                    replaceString x fileName
            | isJust maybeNum =
                    replaceNthOcc (fromJust maybeNum) x fileName
            | last x == "g" =
                    replaceAllOcc x fileName
            | isJust (head rangeNums) && length rangeNums == 1 =
                    replaceStringLineNum firstNum x fileName
            | all isJust rangeNums && length rangeNums == 2 =
                    replaceStringRangeLines firstNum (fromJust $ last rangeNums) x fileName
            | otherwise =  putStrLn "\nInvalid pattern!\n"