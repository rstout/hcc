{- L1 Compiler
   Author: Ryan Stout

   The entry point to the compiler
-}

import Compile
import Args
import System.Environment
import System.IO
import System.Exit
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Compile.Frontend.Parse
import Compile.Types
import Compile

getDefaults "c0c" = defaultJob
getDefaults "l1c" = defaultJob {jobOutFormat = Asm}
getDefaults _ = defaultJob

main :: IO ()
main = do 
  sourceCode <- getSourceCode
  let targetCode = compile sourceCode
  return ()
  
getSourceCode :: IO SourceCode
getSourceCode = do  
  args <- getArgs
  if length args <= 0 then do exitFailure else return () 
  let file = head args
  code <- BS.readFile file
  let source = SourceCode file code
  return source
  
writeTargetCode :: FilePath -> TargetCode -> IO ()
writeTargetCode = undefined

{-  
  prog <- getEnv "COMPILER"
  args <- getArgs
  case parseArgs (getDefaults prog) args of
    Left  err -> do hPrint stderr err
                    hPutStr stderr (usage prog)
                    exitFailure
    Right job -> compile job
  exitSuccess
-}