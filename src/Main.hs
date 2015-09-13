module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL

import IRTS.Compiler
import IRTS.CodegenJava

import System.Environment
import System.Exit

data Opts = Opts { really :: Bool,
                   inputs :: [FilePath],
                   output :: FilePath }

showUsage = do putStrLn "Usage: idris-java <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts False [] "a.out") xs
  where
    process opts ("--yes-really":xs) = process (opts { really = True }) xs
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

java_main :: Opts -> Idris ()
java_main opts = do elabPrims
                    loadInputs (inputs opts) Nothing
                    mainProg <- elabMain
                    ir <- compile (Via "java") (output opts) (Just mainProg)
                    runIO $ codegenJava ir

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts)) 
             then showUsage
             else if (not $ really opts)
                     then do putStrLn "This code generator is intended to be called by the Idris compiler. \
                                      \Please pass Idris the '--codegen' flag to choose a backend."
                             exitWith ExitSuccess
                     else runMain (java_main opts)



