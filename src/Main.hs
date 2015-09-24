module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL

import IRTS.Compiler
import IRTS.CodegenCommon
import IRTS.CodegenJava

import System.Environment
import System.Exit

data Opts = Opts { inputs :: [FilePath],
                   output :: FilePath,
                   outTy :: OutputType }

showUsage = do
  putStrLn "Usage: idris-java <ibc-files> [--mvn] [-S|--codegenonly] [-c|--compileonly] [-o <output-file>]"
  exitWith ExitSuccess


getOpts :: IO Opts
getOpts = do xs <- getArgs
             putStrLn (show xs)
             return $ process (Opts [] "a.out" Executable) xs
  where
    process opts ("--mvn":xs)         = process (opts { outTy  = MavenProject }) xs
    process opts ("-c":xs)            = process (opts { outTy  = Object }) xs
    process opts ("--compileonly":xs) = process (opts { outTy  = Object }) xs    
    process opts ("-S":xs)            = process (opts { outTy  = Raw }) xs
    process opts ("--codegenonly":xs) = process (opts { outTy  = Raw }) xs            
    process opts ("-o":o:xs)          = process (opts { output = o }) xs
    process opts (x:xs)               = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

java_main :: Opts -> Idris ()
java_main opts = do elabPrims
                    loadInputs (inputs opts) Nothing
                    mainProg <- elabMain
                    setOutputTy (outTy opts)
                    ir <- compile (Via "java") (output opts) (Just mainProg)
                    runIO $ codegenJava ir

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts)) 
             then showUsage
             else runMain (java_main opts)



