module Main where


import System.IO ( stdin, stderr, hGetContents, hPutStrLn )
import System.Environment ( getArgs, getProgName, getExecutablePath )
import System.Exit ( exitFailure, exitSuccess )

import Data.List

import LexLang
import ParLang
import Interpreter
import PrintLang
import AbsLang
import Types



import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int


loadStdlib :: IO Prog
loadStdlib = do
  execLoc <- getExecutablePath
  let loc = concat (init (groupBy (const $ (/= '/')) execLoc))
  contents <- readFile (loc ++ "/stdlib.in")
  case pProg (myLLexer contents) of
    Bad s -> do putStrLn "\nParsing of stdlib failed"
                exitFailure
    Ok tree -> return tree

mergeTrees :: Prog -> Prog -> Prog
mergeTrees (Prog d1) (Prog d2) = Prog (d1 ++ d2)

--putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

hPutStrV x v s = if v > 1 then hPutStrLn x s else return ()

--runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

--run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do hPutStrLn stderr "\nParse Failed!\n"
                          hPutStrV stderr v "Tokens:"
                          hPutStrV stderr v $ show ts
                          hPutStrLn stderr s
                          exitFailure
           Ok  tree -> do hPutStrLn stderr "\nParse Successful!"
                          stdlib <- loadStdlib
                          let xtree = mergeTrees stdlib tree
                          case tryInferProgram xtree of
                            Right _ -> return ()
                            Left err -> do
                              hPutStrLn stderr ("Typing error:\n\n" ++ err)
                              exitFailure

                          case interpret xtree of
                            Bad s -> hPutStrLn stderr ("Interpreter failure:\n\n" ++ s)
                            Ok s -> putStrLn ("Interpreter OK: " ++ s)

                          exitSuccess


showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  hPutStrLn stderr $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> hGetContents stdin >>= run 2 pProg
    "-s":fs -> mapM_ (runFile 0 pProg) fs
    fs -> mapM_ (runFile 2 pProg) fs





